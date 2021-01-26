use crate::math_expr::lorentz::LorentzTensor;
use crate::math_expr::{
    BinaryOperator, ColorTensor, ComparisonOperator, Constant, Function, MathExpr, SummationIndex,
    UnaryOperator,
};
use num_complex::Complex64;
use num_traits::ToPrimitive;
use rustpython_parser::ast;
use rustpython_parser::ast::ExpressionType;
use rustpython_parser::error;
use rustpython_parser::parser;
use serde::Deserialize;
use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParseMode {
    Normal,
    Lorentz,
    Color,
}

pub fn parse_math(expr: &str, mode: ParseMode) -> Result<MathExpr, ParseError> {
    let ast = parser::parse_expression(&expr)?;
    let mut indices = Indices::new();
    convert_math(ast, mode, &mut indices)
        .map_err(|err| ParseError::Conversion(expr.to_string(), err))
}

#[derive(Debug)]
pub enum ParseError {
    Parse(error::ParseError),
    Conversion(String, ConversionError),
}
impl From<error::ParseError> for ParseError {
    fn from(err: error::ParseError) -> ParseError {
        ParseError::Parse(err)
    }
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::Parse(err) => writeln!(f, "Failed to parse math expression:\n{}", err),
            ParseError::Conversion(expr, err) => {
                writeln!(
                    f,
                    "Failed to convert math expression: {}\n{}",
                    err.kind, expr
                )?;
                if let Some(location) = err.location {
                    writeln!(f, "{}^", " ".repeat(location.column() - 1))?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct ConversionError {
    location: Option<ast::Location>,
    kind: ConversionErrorKind,
}
impl ConversionError {
    fn new(kind: ConversionErrorKind) -> ConversionError {
        ConversionError {
            kind,
            location: None,
        }
    }
    fn localize(mut self, location: ast::Location) -> ConversionError {
        self.location = self.location.or(Some(location));
        self
    }
}
#[derive(Debug)]
pub enum ConversionErrorKind {
    UnsupportedExpression,
    UnsupportedBinaryOperator,
    UnsupportedUnaryOperator,
    UnsupportedFunctionType,
    UnsupportedFunction,
    UnsupportedAttributeType,
    UnsupportedAttribute,
    UnsupportedConstant,
    UnsupportedComparisonOperator,
    IntegerOutOfRange,
    IntegerExpected,
}
impl fmt::Display for ConversionErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConversionErrorKind::UnsupportedExpression => write!(f, "Unsupported expression"),
            ConversionErrorKind::UnsupportedBinaryOperator => {
                write!(f, "Unsupported binary operator")
            }
            ConversionErrorKind::UnsupportedUnaryOperator => {
                write!(f, "Unsupported unary operator")
            }
            ConversionErrorKind::UnsupportedComparisonOperator => {
                write!(f, "Unsupported comparison operator")
            }
            ConversionErrorKind::UnsupportedFunctionType => write!(f, "Unsupported function call"),
            ConversionErrorKind::UnsupportedFunction => write!(f, "Unsupported function"),
            ConversionErrorKind::UnsupportedAttributeType => {
                write!(f, "Unsupported type of attribute")
            }
            ConversionErrorKind::UnsupportedAttribute => write!(f, "Unsupported attribute"),
            ConversionErrorKind::UnsupportedConstant => write!(f, "Unsupported constant"),
            ConversionErrorKind::IntegerOutOfRange => write!(f, "Integer out of range"),
            ConversionErrorKind::IntegerExpected => write!(f, "Integer expected"),
        }
    }
}

trait ResultExt {
    fn localize_err(self, location: ast::Location) -> Self;
}
impl<T> ResultExt for Result<T, ConversionError> {
    fn localize_err(self, location: ast::Location) -> Result<T, ConversionError> {
        self.map_err(|e| e.localize(location))
    }
}

pub fn deserialize_lorentz_expr<'de, D>(deserializer: D) -> Result<MathExpr, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let expr = String::deserialize(deserializer)?;
    let math = parse_math(&expr, ParseMode::Lorentz).unwrap();
    Ok(math)
}

#[derive(Debug, PartialEq, Eq)]
struct Indices {
    indices: HashSet<SummationIndex>,
}
impl Indices {
    fn new() -> Indices {
        Indices {
            indices: HashSet::new(),
        }
    }
    fn len(&self) -> usize {
        self.indices.len()
    }
    fn insert<T: Into<SummationIndex>>(&mut self, index: T) {
        self.indices.insert(index.into());
    }
    fn add_differences(&mut self, i1: &Indices, i2: &Indices) {
        for i in i1.indices.symmetric_difference(&i2.indices) {
            self.indices.insert(*i);
        }
    }
    fn extend(&mut self, other: &Indices) {
        self.indices.extend(&other.indices)
    }
    fn intersection<'a>(&'a self, other: &'a Indices) -> impl Iterator<Item = &'a SummationIndex> {
        self.indices.intersection(&other.indices)
    }
    fn is_empty(&self) -> bool {
        self.indices.is_empty()
    }
    fn single(&self) -> SummationIndex {
        *self
            .indices
            .iter()
            .next()
            .expect("BUG: Single called on empty index collection")
    }
}

fn convert_math(
    expr: ast::Expression,
    mode: ParseMode,
    indices: &mut Indices,
) -> Result<MathExpr, ConversionError> {
    let result = match expr.node {
        ExpressionType::Number { value } => convert_number(value),
        ExpressionType::Binop { op, a, b } => convert_binop(op, *a, *b, mode, indices),
        ExpressionType::Unop { op, a } => convert_unop(op, *a, mode, indices),
        ExpressionType::Identifier { name } => Ok(MathExpr::Variable { name }),
        ExpressionType::Call {
            function,
            args,
            keywords,
        } => convert_call(*function, args, keywords, mode, indices),
        ExpressionType::Attribute { value, name } => convert_attribute(name, *value),
        ExpressionType::IfExpression { test, body, orelse } => {
            convert_if_expression(*test, *body, *orelse, mode, indices)
        }
        ExpressionType::Compare { vals, ops } => convert_comparison(vals, ops),
        _ => Err(ConversionError {
            location: Some(expr.location),
            kind: ConversionErrorKind::UnsupportedExpression,
        }),
    };
    result.localize_err(expr.location)
}

fn convert_number(value: ast::Number) -> Result<MathExpr, ConversionError> {
    let num = match value {
        ast::Number::Integer { value } => {
            let num = value
                .to_f64()
                .ok_or_else(|| ConversionError::new(ConversionErrorKind::IntegerOutOfRange))?;
            MathExpr::Number { value: num }
        }
        ast::Number::Float { value } => MathExpr::Number { value: value },
        ast::Number::Complex { real, imag } => MathExpr::Complex {
            value: Complex64::new(real, imag),
        },
    };
    Ok(num)
}

fn convert_binop(
    operator: ast::Operator,
    left_ast: ast::Expression,
    right_ast: ast::Expression,
    mode: ParseMode,
    indices: &mut Indices,
) -> Result<MathExpr, ConversionError> {
    let mut indices_left = Indices::new();
    let mut indices_right = Indices::new();
    let left = Box::new(convert_math(left_ast, mode, &mut indices_left)?);
    let right = Box::new(convert_math(right_ast, mode, &mut indices_right)?);
    let expr = match operator {
        ast::Operator::Add => {
            assert_eq!(indices_left, indices_right);
            indices.extend(&indices_left);
            MathExpr::BinaryOp {
                operator: BinaryOperator::Add,
                left,
                right,
            }
        }
        ast::Operator::Sub => {
            assert_eq!(indices_left, indices_right);
            indices.extend(&indices_left);
            MathExpr::BinaryOp {
                operator: BinaryOperator::Sub,
                left,
                right,
            }
        }
        ast::Operator::Mult => {
            let mut expr = MathExpr::BinaryOp {
                operator: BinaryOperator::Mul,
                left,
                right,
            };
            for i in indices_left.intersection(&indices_right) {
                expr = MathExpr::Sum {
                    expr: Box::new(expr),
                    index: *i,
                }
            }
            indices.add_differences(&indices_left, &indices_right);
            expr
        }
        ast::Operator::Div => {
            assert!(indices_right.is_empty());
            indices.extend(&indices_left);
            MathExpr::BinaryOp {
                operator: BinaryOperator::Div,
                left,
                right,
            }
        }
        ast::Operator::Pow => {
            if indices_left.len() == 1
                && extract_numeric_literal(&right)
                    .map(|f| f == 2f64)
                    .unwrap_or(false)
            {
                MathExpr::Sum {
                    expr: Box::new(MathExpr::BinaryOp {
                        operator: BinaryOperator::Mul,
                        left: left.clone(),
                        right: left,
                    }),
                    index: indices_left.single(),
                }
            } else {
                assert!(indices_left.is_empty());
                assert!(indices_right.is_empty());
                MathExpr::BinaryOp {
                    operator: BinaryOperator::Pow,
                    left,
                    right,
                }
            }
        }
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedBinaryOperator,
            ))
        }
    };
    Ok(expr)
}

fn convert_unop(
    operator: ast::UnaryOperator,
    operand_ast: ast::Expression,
    mode: ParseMode,
    indices: &mut Indices,
) -> Result<MathExpr, ConversionError> {
    let operand = Box::new(convert_math(operand_ast, mode, indices)?);
    let op = match operator {
        ast::UnaryOperator::Pos => MathExpr::UnaryOp {
            operator: UnaryOperator::Plus,
            operand,
        },
        ast::UnaryOperator::Neg => MathExpr::UnaryOp {
            operator: UnaryOperator::Minus,
            operand,
        },
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedUnaryOperator,
            ))
        }
    };
    Ok(op)
}

fn convert_attribute(name: String, value: ast::Expression) -> Result<MathExpr, ConversionError> {
    let prefix = match value.node {
        ExpressionType::Identifier { name } => name,
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedAttributeType,
            ))
        }
    };
    let attr = match (&*prefix, &*name) {
        ("cmath", "pi") => MathExpr::Constant { name: Constant::Pi },
        ("cmath", _) => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedConstant,
            ))
        }
        (_, "real") => MathExpr::Call {
            function: Function::RealPart,
            args: vec![MathExpr::Variable { name: prefix }],
        },
        (_, "imag") => MathExpr::Call {
            function: Function::ImaginaryPart,
            args: vec![MathExpr::Variable { name: prefix }],
        },
        (_, _) => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedAttribute,
            ))
        }
    };
    Ok(attr)
}

fn convert_call(
    function: ast::Expression,
    args_ast: Vec<ast::Expression>,
    keywords: Vec<ast::Keyword>,
    mode: ParseMode,
    indices: &mut Indices,
) -> Result<MathExpr, ConversionError> {
    assert!(keywords.is_empty());
    let args: Result<Vec<_>, _> = args_ast
        .into_iter()
        .map(|arg| convert_math(arg, mode, indices))
        .collect();
    let args = args?;
    match function.node {
        ExpressionType::Identifier { name } => match mode {
            ParseMode::Lorentz => convert_call_lorentz(name, args, indices),
            ParseMode::Color => convert_call_color(name, args, indices),
            ParseMode::Normal => convert_call_simple(name, args),
        },
        ExpressionType::Attribute { value, name } => convert_call_attr(name, *value, args),
        _ => Err(ConversionError::new(
            ConversionErrorKind::UnsupportedFunctionType,
        )),
    }
}

fn convert_call_simple(function: String, args: Vec<MathExpr>) -> Result<MathExpr, ConversionError> {
    let fun = match &*function {
        "complex" => {
            let re = args.get(0).and_then(extract_numeric_literal);
            let im = args.get(1).and_then(extract_numeric_literal);
            match (re, im) {
                (Some(re), Some(im)) => MathExpr::Complex {
                    value: Complex64::new(re, im),
                },
                _ => MathExpr::Call {
                    function: Function::Complex,
                    args,
                },
            }
        }
        "complexconjugate" => MathExpr::Call {
            function: Function::ComplexConjugate,
            args,
        },
        _ => MathExpr::Call {
            function: Function::Other(function),
            args,
        },
    };
    Ok(fun)
}

fn convert_call_attr(
    name: String,
    value: ast::Expression,
    args: Vec<MathExpr>,
) -> Result<MathExpr, ConversionError> {
    let attr = match value.node {
        ExpressionType::Identifier { name } => name,
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedFunctionType,
            ))
        }
    };
    match &*attr {
        "cmath" => return convert_call_cmath(name, args),
        _ => (),
    };
    let fun = match &*name {
        "conjugate" => MathExpr::Call {
            function: Function::ComplexConjugate,
            args: vec![MathExpr::Variable { name: attr }],
        },
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedFunction,
            ))
        }
    };
    Ok(fun)
}

fn convert_call_cmath(function: String, args: Vec<MathExpr>) -> Result<MathExpr, ConversionError> {
    let fun = match &*function {
        "abs" => MathExpr::Call {
            function: Function::Abs,
            args,
        },
        "cos" => MathExpr::Call {
            function: Function::Cos,
            args,
        },
        "sin" => MathExpr::Call {
            function: Function::Sin,
            args,
        },
        "tan" => MathExpr::Call {
            function: Function::Tan,
            args,
        },
        "asin" => MathExpr::Call {
            function: Function::ASin,
            args,
        },
        "acos" => MathExpr::Call {
            function: Function::ACos,
            args,
        },
        "sqrt" => MathExpr::Call {
            function: super::Function::Sqrt,
            args,
        },
        "log" => MathExpr::Call {
            function: super::Function::Log,
            args,
        },
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedFunction,
            ))
        }
    };
    Ok(fun)
}

fn convert_call_lorentz(
    name: String,
    args: Vec<MathExpr>,
    indices: &mut Indices,
) -> Result<MathExpr, ConversionError> {
    let lorentz = match &*name {
        "ProjP" => LorentzTensor::ProjP {
            i1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
        },
        "ProjM" => LorentzTensor::ProjM {
            i1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
        },
        "Gamma" => LorentzTensor::Gamma {
            mu1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
            i3: extract_index(&args[2], indices)?,
        },
        "Identity" => LorentzTensor::KroneckerDelta {
            i1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
        },
        "Gamma5" => LorentzTensor::Gamma5 {
            i1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
        },
        "P" => LorentzTensor::Momentum {
            mu1: extract_index(&args[0], indices)?,
            particle: extract_integer_literal(&args[1])?,
        },
        "Metric" => LorentzTensor::Metric {
            mu1: extract_index(&args[0], indices)?,
            mu2: extract_index(&args[1], indices)?,
        },
        "Epsilon" => LorentzTensor::Epsilon {
            mu1: extract_index(&args[0], indices)?,
            mu2: extract_index(&args[1], indices)?,
            mu3: extract_index(&args[2], indices)?,
            mu4: extract_index(&args[3], indices)?,
        },
        "Sigma" => LorentzTensor::Sigma {
            mu1: extract_index(&args[0], indices)?,
            mu2: extract_index(&args[1], indices)?,
            i3: extract_index(&args[2], indices)?,
            i4: extract_index(&args[3], indices)?,
        },
        _ => return convert_call_simple(name, args),
    };
    Ok(MathExpr::LorentzTensor { lorentz })
}

fn convert_call_color(
    name: String,
    args: Vec<MathExpr>,
    indices: &mut Indices,
) -> Result<MathExpr, ConversionError> {
    let color = match &*name {
        "f" => ColorTensor::StructureConstant {
            a1: extract_index(&args[0], indices)?,
            a2: extract_index(&args[1], indices)?,
            a3: extract_index(&args[2], indices)?,
        },
        "d" => ColorTensor::SymmetricTensor {
            a1: extract_index(&args[0], indices)?,
            a2: extract_index(&args[1], indices)?,
            a3: extract_index(&args[2], indices)?,
        },
        "T" => ColorTensor::FundamentalRep {
            a1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
            jb3: extract_index(&args[2], indices)?,
        },
        "Epsilon" => ColorTensor::Epsilon {
            i1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
            i3: extract_index(&args[2], indices)?,
        },
        "EpsilonBar" => ColorTensor::EpsilonBar {
            ib1: extract_index(&args[0], indices)?,
            ib2: extract_index(&args[1], indices)?,
            ib3: extract_index(&args[2], indices)?,
        },
        "T6" => ColorTensor::SextetRep {
            a1: extract_index(&args[0], indices)?,
            alpha2: extract_index(&args[1], indices)?,
            betab3: extract_index(&args[2], indices)?,
        },
        "K6" => ColorTensor::SextetClebschGordan {
            alpha1: extract_index(&args[0], indices)?,
            ib2: extract_index(&args[1], indices)?,
            jb3: extract_index(&args[2], indices)?,
        },
        "K6Bar" => ColorTensor::AntiSextetClebschGordan {
            alphab1: extract_index(&args[0], indices)?,
            i2: extract_index(&args[1], indices)?,
            j3: extract_index(&args[2], indices)?,
        },
        "Identity" => ColorTensor::KroneckerDelta {
            i1: extract_index(&args[0], indices)?,
            jb2: extract_index(&args[1], indices)?,
        },
        _ => return convert_call_simple(name, args),
    };
    Ok(MathExpr::ColorTensor { color })
}

fn extract_index<T>(expr: &MathExpr, indices: &mut Indices) -> Result<T, ConversionError>
where
    T: Clone + From<i64> + Into<SummationIndex>,
{
    let number = extract_integer_literal(expr)?;
    let index = T::from(number);
    if number < 0 {
        indices.insert(index.clone())
    }
    Ok(index)
}

fn extract_integer_literal(expr: &MathExpr) -> Result<i64, ConversionError> {
    extract_numeric_literal(expr)
        .and_then(|f| f.to_i64())
        .ok_or_else(|| ConversionError::new(ConversionErrorKind::IntegerExpected))
}

fn extract_numeric_literal(expr: &MathExpr) -> Option<f64> {
    match expr {
        MathExpr::Number { value } => Some(*value),
        MathExpr::UnaryOp { operator, operand } => match operator {
            UnaryOperator::Plus => extract_numeric_literal(&operand),
            UnaryOperator::Minus => extract_numeric_literal(&operand).map(|x| -x),
        },
        _ => None,
    }
}

fn convert_if_expression(
    test: ast::Expression,
    body: ast::Expression,
    orelse: ast::Expression,
    mode: ParseMode,
    indices: &mut Indices,
) -> Result<MathExpr, ConversionError> {
    let condition = Box::new(convert_condition(test)?);
    let mut indices_left = Indices::new();
    let mut indices_right = Indices::new();
    let expr = MathExpr::Conditional {
        condition,
        if_true: Box::new(convert_math(body, mode, &mut indices_left)?),
        if_false: Box::new(convert_math(orelse, mode, &mut indices_right)?),
    };
    assert_eq!(indices_left, indices_right);
    indices.extend(&indices_left);
    Ok(expr)
}

fn convert_condition(expr: ast::Expression) -> Result<MathExpr, ConversionError> {
    convert_math(expr, ParseMode::Normal, &mut Indices::new())
}

fn convert_comparison(
    values: Vec<ast::Expression>,
    operators: Vec<ast::Comparison>,
) -> Result<MathExpr, ConversionError> {
    let operators: Result<_, _> = operators
        .into_iter()
        .map(|op| {
            let op = match op {
                ast::Comparison::Equal => ComparisonOperator::Equals,
                _ => {
                    return Err(ConversionError::new(
                        ConversionErrorKind::UnsupportedComparisonOperator,
                    ))
                }
            };
            Ok(op)
        })
        .collect();
    let values: Result<_, _> = values
        .into_iter()
        .map(|expr| convert_math(expr, ParseMode::Normal, &mut Indices::new()))
        .collect();
    Ok(MathExpr::Comparison {
        values: values?,
        operators: operators?,
    })
}

#[cfg(test)]
mod test {
    use super::ParseMode;
    use crate::math_expr::MathExpr;
    use crate::ufo;
    use crate::ufo::UfoModel;

    #[test]
    fn expr1() {
        super::parse_math("1+1", ParseMode::Normal).unwrap();
    }

    #[test]
    fn gc_1() {
        let value = "-(ee*complex(0,1))/3.";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    #[test]
    fn gc_47() {
        let value = "(CKM3x1*ee*complex(0,1))/(sw*cmath.sqrt(2))";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    #[test]
    fn gc_81() {
        let value = "ee**2*complex(0,1)*vev + (cw**2*ee**2*complex(0,1)*vev)/(2.*sw**2) + (ee**2*complex(0,1)*sw**2*vev)/(2.*cw**2)";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    #[test]
    fn ffs1() {
        let structure = "ProjM(2,1)";
        super::parse_math(structure, ParseMode::Lorentz).unwrap();
    }

    #[test]
    fn ffv2() {
        let structure = "Gamma(3,2,-1)*ProjM(-1,1)";
        let expr = super::parse_math(structure, ParseMode::Lorentz).unwrap();
        match expr {
            MathExpr::Sum { .. } => (),
            _ => panic!("Sum expected"),
        }
    }

    #[test]
    fn i1x31() {
        let value = "yb*complexconjugate(CKM1x3)";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    #[test]
    fn mw() {
        let value =
            "cmath.sqrt(MZ**2/2. + cmath.sqrt(MZ**4/4. - (aEW*cmath.pi*MZ**2)/(Gf*cmath.sqrt(2))))";
        let result = super::parse_math(value, ParseMode::Normal);
        if result.is_err() {
            println!("{}", result.err().unwrap());
            panic!("Error");
        }
    }

    #[test]
    fn uvgc_123_24() {
        let value = "( 0 if MB else (complex(0,1)*G**3)/(48.*cmath.pi**2) )";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }
    #[test]
    fn uvgc_147_46() {
        let value = "( (5*complex(0,1)*G**2)/(12.*cmath.pi**2) - (complex(0,1)*G**2*reglog(MB/MU_R))/(2.*cmath.pi**2) if MB else (complex(0,1)*G**2)/(12.*cmath.pi**2) ) - (complex(0,1)*G**2)/(12.*cmath.pi**2)";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    #[test]
    fn re() {
        let value = "z.real";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    #[test]
    fn asc() {
        let value = "asin(1./z)";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    #[test]
    fn reglog() {
        let value = "(0.0 if z==0.0 else cmath.log(z.real))";
        super::parse_math(value, ParseMode::Normal).unwrap();
    }

    fn parse_model(model: &str) {
        let model = UfoModel::load(model).unwrap();
        for parameter in model.parameters.values() {
            if let ufo::ValOrExpr::Expr(ufo::UfoMath(ref expr)) = &parameter.value {
                super::parse_math(&expr, ParseMode::Normal).unwrap();
            }
        }
        for lorentz in model.lorentz_structures.values() {
            let ufo::UfoMath(ref expr) = lorentz.structure;
            super::parse_math(&expr, ParseMode::Lorentz).unwrap();
        }
        for vertex in model.vertices.iter() {
            for c in vertex.color.iter() {
                let ufo::UfoMath(ref expr) = c;
                super::parse_math(&expr, ParseMode::Color).unwrap();
            }
        }
        for coupling in model.couplings.values() {
            match coupling.value {
                ufo::CouplingValue::Simple(ref expr) => {
                    let ufo::UfoMath(ref expr) = expr;
                    super::parse_math(&expr, ParseMode::Color).unwrap();
                }
                ufo::CouplingValue::Orders(ref orders) => {
                    for o in orders.values() {
                        let ufo::UfoMath(ref expr) = o;
                        super::parse_math(&expr, ParseMode::Color).unwrap();
                    }
                }
            }
        }
        for function in model.function_library.values() {
            let ufo::UfoMath(ref expr) = function.expr;
            super::parse_math(&expr, ParseMode::Normal).unwrap();
        }
    }

    #[test]
    fn parse_sm_mg5() {
        parse_model("tests/models_json/sm_mg5");
    }

    #[test]
    fn parse_sm_nlo() {
        parse_model("tests/models_json/SM_NLO");
    }
}
