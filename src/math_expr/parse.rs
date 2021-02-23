use crate::math_expr::{
    BinaryOperator, Comparison, ComparisonOperator, Function, MathExpr, Number, Tensor,
    TensorIndex, UnaryOperator,
};
use num_complex::Complex64;
use num_traits::{ToPrimitive, Zero};
use rustpython_parser::ast;
use rustpython_parser::ast::ExpressionType;
use rustpython_parser::error;
use rustpython_parser::parser;
use serde::Deserialize;
use std::collections::HashMap;
use std::f64;
use std::fmt;

pub fn parse_math<T: Tensor>(expr: &str) -> Result<MathExpr<T>, ParseError> {
    let ast = parser::parse_expression(&expr.trim_start().replace('\n', " "))?;
    let mut indices = Indices::new();
    convert_math(ast, &mut indices).map_err(|err| ParseError::Conversion(expr.to_string(), err))
}

pub fn parse_math_alias<T: Tensor>(
    expr: &str,
    aliases: HashMap<String, i64>,
) -> Result<MathExpr<T>, ParseError> {
    let ast = parser::parse_expression(&expr.trim_start().replace('\n', " "))?;
    let mut indices = Indices::with_aliases(aliases);
    convert_math(ast, &mut indices).map_err(|err| ParseError::Conversion(expr.to_string(), err))
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
    UnknownIndex,
    IntegerOutOfRange,
    IntegerExpected,
    NotEnoughIndices,
    TooManyIndices,
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
            ConversionErrorKind::UnknownIndex => write!(f, "Unknown index"),
            ConversionErrorKind::TooManyIndices => write!(f, "Too many indices"),
            ConversionErrorKind::NotEnoughIndices => write!(f, "Not enough indices"),
        }
    }
}

pub fn deserialize_math_expr<'de, D, T: Tensor>(deserializer: D) -> Result<MathExpr<T>, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let expr = String::deserialize(deserializer)?;
    parse_math(&expr).map_err(serde::de::Error::custom)
}

pub fn deserialize_math_map<'de, D, S, T>(
    deserializer: D,
) -> Result<HashMap<S, MathExpr<T>>, D::Error>
where
    D: serde::de::Deserializer<'de>,
    S: serde::Deserialize<'de> + Eq + std::hash::Hash,
    T: Tensor,
{
    let expr = HashMap::<S, &str>::deserialize(deserializer)?;
    expr.into_iter()
        .map(|(k, v)| {
            parse_math(v)
                .map(|v| (k, v))
                .map_err(serde::de::Error::custom)
        })
        .collect()
}

pub fn deserialize_math_array_four<'de, D, T>(deserializer: D) -> Result<[MathExpr<T>; 4], D::Error>
where
    D: serde::de::Deserializer<'de>,
    T: Tensor,
{
    let expr: [&str; 4] = Deserialize::deserialize(deserializer)?;
    let mut out: [MathExpr<T>; 4] = Default::default();
    for (i, e) in expr.iter().enumerate() {
        out[i] = parse_math(e).map_err(serde::de::Error::custom)?;
    }
    Ok(out)
}

trait ResultExt {
    fn localize_err(self, location: ast::Location) -> Self;
}
impl<T> ResultExt for Result<T, ConversionError> {
    fn localize_err(self, location: ast::Location) -> Result<T, ConversionError> {
        self.map_err(|e| e.localize(location))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Indices<T: TensorIndex> {
    indices: HashMap<i64, T>,
    names: HashMap<String, i64>,
}
impl<T: TensorIndex> Indices<T> {
    fn new() -> Indices<T> {
        Indices::with_aliases(HashMap::new())
    }
    fn with_aliases(aliases: HashMap<String, i64>) -> Indices<T> {
        Indices {
            indices: HashMap::new(),
            names: aliases,
        }
    }
    fn len(&self) -> usize {
        self.indices.len()
    }
    fn insert<I: Into<T>>(&mut self, number: i64, index: I) {
        self.indices.insert(number, index.into().normalize());
    }
    fn add_differences(&mut self, i1: &Indices<T>, i2: &Indices<T>) {
        for (k, v) in i1.indices.iter() {
            if !i2.indices.contains_key(k) {
                self.indices.insert(*k, *v);
            }
        }
        for (k, v) in i2.indices.iter() {
            if !i1.indices.contains_key(k) {
                self.indices.insert(*k, *v);
            }
        }
    }
    fn extend(&mut self, other: &Indices<T>) {
        self.indices.extend(&other.indices)
    }
    fn intersection<'a>(&'a self, other: &'a Indices<T>) -> impl Iterator<Item = &'a T> {
        self.indices.iter().filter_map(move |(k, v)| {
            if other.indices.contains_key(k) {
                Some(v)
            } else {
                None
            }
        })
    }
    fn is_empty(&self) -> bool {
        self.indices.is_empty()
    }
    fn single(&self) -> T {
        *self
            .indices
            .values()
            .next()
            .expect("BUG: Single called on empty index collection")
    }
    fn get_alias(&self, name: &str) -> Option<i64> {
        self.names.get(name).copied()
    }
}

fn convert_math<T: Tensor>(
    expr: ast::Expression,
    indices: &mut Indices<T::Indices>,
) -> Result<MathExpr<T>, ConversionError> {
    let result = match expr.node {
        ExpressionType::Number { value } => convert_number(value),
        ExpressionType::Binop { op, a, b } => convert_binop(op, *a, *b, indices),
        ExpressionType::Unop { op, a } => convert_unop(op, *a, indices),
        ExpressionType::Identifier { name } => Ok(MathExpr::Variable { name }),
        ExpressionType::Call {
            function,
            args,
            keywords,
        } => convert_call(*function, args, keywords, indices),
        ExpressionType::Attribute { value, name } => convert_attribute(name, *value),
        ExpressionType::IfExpression { test, body, orelse } => {
            convert_if_expression(*test, *body, *orelse, indices)
        }
        _ => Err(ConversionError {
            location: Some(expr.location),
            kind: ConversionErrorKind::UnsupportedExpression,
        }),
    };
    result.localize_err(expr.location)
}

fn convert_number<T: Tensor>(value: ast::Number) -> Result<MathExpr<T>, ConversionError> {
    let num = match value {
        ast::Number::Integer { value } => {
            let num = value
                .to_i64()
                .ok_or_else(|| ConversionError::new(ConversionErrorKind::IntegerOutOfRange))?;
            MathExpr::Number {
                value: Number::Integer(num),
            }
        }
        ast::Number::Float { value } => MathExpr::Number {
            value: Number::Real(value),
        },
        ast::Number::Complex { real, imag } => MathExpr::Number {
            value: Number::Complex(Complex64::new(real, imag)),
        },
    };
    Ok(num)
}

fn convert_binop<T: Tensor>(
    operator: ast::Operator,
    left_ast: ast::Expression,
    right_ast: ast::Expression,
    indices: &mut Indices<T::Indices>,
) -> Result<MathExpr<T>, ConversionError> {
    let mut indices_left = Indices::with_aliases(indices.names.clone());
    let mut indices_right = Indices::with_aliases(indices.names.clone());
    let left = Box::new(convert_math(left_ast, &mut indices_left)?);
    let right = Box::new(convert_math(right_ast, &mut indices_right)?);
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
                    .map(|i| i == Number::Integer(2))
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

fn convert_unop<T: Tensor>(
    operator: ast::UnaryOperator,
    operand_ast: ast::Expression,
    indices: &mut Indices<T::Indices>,
) -> Result<MathExpr<T>, ConversionError> {
    let operand = Box::new(convert_math(operand_ast, indices)?);
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

fn convert_attribute<T: Tensor>(
    name: String,
    value: ast::Expression,
) -> Result<MathExpr<T>, ConversionError> {
    let prefix = match value.node {
        ExpressionType::Identifier { name } => name,
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedAttributeType,
            ))
        }
    };
    let attr = match (&*prefix, &*name) {
        ("cmath", "pi") => MathExpr::Number {
            value: Number::Real(f64::consts::PI),
        },
        ("cmath", "e") => MathExpr::Number {
            value: Number::Real(f64::consts::E),
        },
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

fn convert_call<T: Tensor>(
    function: ast::Expression,
    args: Vec<ast::Expression>,
    keywords: Vec<ast::Keyword>,
    indices: &mut Indices<T::Indices>,
) -> Result<MathExpr<T>, ConversionError> {
    assert!(keywords.is_empty());
    match function.node {
        ExpressionType::Identifier { name } => {
            let mut index_parser = IndexParser::new(&args, indices);
            match T::parse(&name, &mut index_parser)? {
                Some(tensor) => {
                    index_parser.finalize()?;
                    Ok(MathExpr::Tensor { tensor })
                }
                None => convert_call_simple(name, convert_call_args(args, indices)?),
            }
        }
        ExpressionType::Attribute { value, name } => {
            convert_call_attr(name, *value, convert_call_args(args, indices)?)
        }
        _ => Err(ConversionError::new(
            ConversionErrorKind::UnsupportedFunctionType,
        )),
    }
}

fn convert_call_args<T: Tensor>(
    args_ast: Vec<ast::Expression>,
    indices: &mut Indices<T::Indices>,
) -> Result<Vec<MathExpr<T>>, ConversionError> {
    args_ast
        .into_iter()
        .map(|arg| convert_math(arg, indices))
        .collect()
}

fn convert_call_simple<T: Tensor>(
    function: String,
    args: Vec<MathExpr<T>>,
) -> Result<MathExpr<T>, ConversionError> {
    let fun = match &*function {
        "complex" => {
            let re = args.get(0).and_then(extract_float_literal);
            let im = args.get(1).and_then(extract_float_literal);
            match (re, im) {
                (Some(re), Some(im)) => MathExpr::Number {
                    value: Number::Complex(Complex64::new(re, im)),
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

fn convert_call_attr<T: Tensor>(
    name: String,
    value: ast::Expression,
    args: Vec<MathExpr<T>>,
) -> Result<MathExpr<T>, ConversionError> {
    let attr = match value.node {
        ExpressionType::Identifier { name } => name,
        _ => {
            return Err(ConversionError::new(
                ConversionErrorKind::UnsupportedFunctionType,
            ))
        }
    };
    if attr == "cmath" {
        return convert_call_cmath(name, args);
    }
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

fn convert_call_cmath<T: Tensor>(
    function: String,
    args: Vec<MathExpr<T>>,
) -> Result<MathExpr<T>, ConversionError> {
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
        "atan" => MathExpr::Call {
            function: Function::ATan,
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

pub struct IndexParser<'a, T: TensorIndex> {
    index: usize,
    args: &'a [ast::Expression],
    indices: &'a mut Indices<T>,
}
impl<'a, T: TensorIndex> IndexParser<'a, T> {
    fn new(args: &'a [ast::Expression], indices: &'a mut Indices<T>) -> IndexParser<'a, T> {
        IndexParser {
            index: 0,
            args,
            indices,
        }
    }
    pub fn n_args(&self) -> usize {
        self.args.len()
    }
    pub fn next_integer(&mut self) -> Result<i64, ConversionError> {
        if self.index < self.args.len() {
            let result = extract_integer_literal(&self.args[self.index]);
            self.index += 1;
            result
        } else {
            Err(ConversionError::new(ConversionErrorKind::NotEnoughIndices))
        }
    }
    pub fn next_index<I: Clone + From<i64> + Into<T>>(&mut self) -> Result<I, ConversionError> {
        if self.index < self.args.len() {
            let result = extract_index(&self.args[self.index], self.indices);
            self.index += 1;
            result
        } else {
            Err(ConversionError::new(ConversionErrorKind::NotEnoughIndices))
        }
    }
    fn finalize(&self) -> Result<(), ConversionError> {
        if self.index != self.args.len() {
            Err(ConversionError::new(ConversionErrorKind::TooManyIndices))
        } else {
            Ok(())
        }
    }
}

fn extract_index<T, U>(
    expr: &ast::Expression,
    indices: &mut Indices<U>,
) -> Result<T, ConversionError>
where
    U: TensorIndex,
    T: Clone + From<i64> + Into<U>,
{
    let number = match &expr.node {
        ast::ExpressionType::Number { value } => match value {
            ast::Number::Integer { value } => value
                .to_i64()
                .ok_or_else(|| ConversionError::new(ConversionErrorKind::IntegerOutOfRange))?,
            _ => return Err(ConversionError::new(ConversionErrorKind::IntegerExpected)),
        },
        ast::ExpressionType::Unop {
            op: ast::UnaryOperator::Neg,
            a,
        } => extract_integer_literal(a).map(|i| -i)?,
        ast::ExpressionType::Identifier { name } => match indices.get_alias(&name) {
            Some(i) => i,
            _ => return Err(ConversionError::new(ConversionErrorKind::UnknownIndex)),
        },
        ast::ExpressionType::String {
            value: ast::StringGroup::Constant { value },
        } => match indices.get_alias(&value) {
            Some(i) => i,
            _ => return Err(ConversionError::new(ConversionErrorKind::UnknownIndex)),
        },
        _ => return Err(ConversionError::new(ConversionErrorKind::IntegerExpected)),
    };
    let index = T::from(number);
    if number < 0 {
        indices.insert(number, index.clone())
    }
    Ok(index)
}

fn extract_integer_literal(expr: &ast::Expression) -> Result<i64, ConversionError> {
    match &expr.node {
        ast::ExpressionType::Number { value } => match value {
            ast::Number::Integer { value } => value
                .to_i64()
                .ok_or_else(|| ConversionError::new(ConversionErrorKind::IntegerOutOfRange)),
            _ => Err(ConversionError::new(ConversionErrorKind::IntegerExpected)),
        },
        ast::ExpressionType::Unop {
            op: ast::UnaryOperator::Neg,
            a,
        } => extract_integer_literal(a).map(|i| -i),
        _ => Err(ConversionError::new(ConversionErrorKind::IntegerExpected)),
    }
}

fn extract_float_literal<T: Tensor>(expr: &MathExpr<T>) -> Option<f64> {
    match extract_numeric_literal(expr) {
        Some(Number::Integer(i)) => Some(i as f64),
        Some(Number::Real(f)) => Some(f),
        _ => None,
    }
}

fn extract_numeric_literal<T: Tensor>(expr: &MathExpr<T>) -> Option<Number> {
    match expr {
        MathExpr::Number { value } => Some(*value),
        MathExpr::UnaryOp { operator, operand } => match operator {
            UnaryOperator::Plus => extract_numeric_literal(&operand),
            UnaryOperator::Minus => extract_numeric_literal(&operand).map(|x| -x),
        },
        _ => None,
    }
}

fn convert_if_expression<T: Tensor>(
    test: ast::Expression,
    body: ast::Expression,
    orelse: ast::Expression,
    indices: &mut Indices<T::Indices>,
) -> Result<MathExpr<T>, ConversionError> {
    let condition = Box::new(convert_condition(test)?);
    let mut indices_left = Indices::with_aliases(indices.names.clone());
    let mut indices_right = Indices::with_aliases(indices.names.clone());
    let expr = MathExpr::Conditional {
        condition,
        if_true: Box::new(convert_math(body, &mut indices_left)?),
        if_false: Box::new(convert_math(orelse, &mut indices_right)?),
    };
    assert_eq!(indices_left, indices_right);
    indices.extend(&indices_left);
    Ok(expr)
}

fn convert_condition<T: Tensor>(expr: ast::Expression) -> Result<Comparison<T>, ConversionError> {
    match expr.node {
        ExpressionType::Compare { vals, ops } => convert_comparison(vals, ops),
        _ => {
            // If there is no explicit comparison, this means that the expression is compared using `!= 0`
            let val = convert_math(expr, &mut Indices::new())?;
            Ok(Comparison {
                operators: vec![ComparisonOperator::NotEqual],
                values: vec![
                    val,
                    MathExpr::Number {
                        value: Number::zero(),
                    },
                ],
            })
        }
    }
}

fn convert_comparison<T: Tensor>(
    values: Vec<ast::Expression>,
    operators: Vec<ast::Comparison>,
) -> Result<Comparison<T>, ConversionError> {
    let operators: Result<_, _> = operators
        .into_iter()
        .map(|op| {
            let op = match op {
                ast::Comparison::Equal => ComparisonOperator::Equals,
                ast::Comparison::NotEqual => ComparisonOperator::NotEqual,
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
        .map(|expr| convert_math(expr, &mut Indices::new()))
        .collect();
    Ok(Comparison {
        values: values?,
        operators: operators?,
    })
}

#[cfg(test)]
mod test {
    use crate::color::ColorExpr;
    use crate::math_expr::MathExprPlain;
    use crate::spin::LorentzExpr;
    use crate::ufo;
    use crate::ufo::{Parameter, UfoMath, UfoModel};

    #[test]
    fn expr1() {
        let _: MathExprPlain = super::parse_math("1+1").unwrap();
    }

    #[test]
    fn gc_1() {
        let value = "-(ee*complex(0,1))/3.";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn gc_47() {
        let value = "(CKM3x1*ee*complex(0,1))/(sw*cmath.sqrt(2))";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn gc_81() {
        let value = "ee**2*complex(0,1)*vev + (cw**2*ee**2*complex(0,1)*vev)/(2.*sw**2) + (ee**2*complex(0,1)*sw**2*vev)/(2.*cw**2)";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn ffs1() {
        let structure = "ProjM(2,1)";
        let _: LorentzExpr = super::parse_math(structure).unwrap();
    }

    #[test]
    fn ffv2() {
        let structure = "Gamma(3,2,-1)*ProjM(-1,1)";
        let expr: LorentzExpr = super::parse_math(structure).unwrap();
        match expr {
            LorentzExpr::Sum { .. } => (),
            _ => panic!("Sum expected"),
        }
    }

    #[test]
    fn i1x31() {
        let value = "yb*complexconjugate(CKM1x3)";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn mw() {
        let value =
            "cmath.sqrt(MZ**2/2. + cmath.sqrt(MZ**4/4. - (aEW*cmath.pi*MZ**2)/(Gf*cmath.sqrt(2))))";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn uvgc_123_24() {
        let value = "( 0 if MB else (complex(0,1)*G**3)/(48.*cmath.pi**2) )";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }
    #[test]
    fn uvgc_147_46() {
        let value = "( (5*complex(0,1)*G**2)/(12.*cmath.pi**2) - (complex(0,1)*G**2*reglog(MB/MU_R))/(2.*cmath.pi**2) if MB else (complex(0,1)*G**2)/(12.*cmath.pi**2) ) - (complex(0,1)*G**2)/(12.*cmath.pi**2)";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn re() {
        let value = "z.real";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn asc() {
        let value = "asin(1./z)";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    #[test]
    fn reglog() {
        let value = "(0.0 if z==0.0 else cmath.log(z.real))";
        let _: MathExprPlain = super::parse_math(value).unwrap();
    }

    fn parse_model(model: &str) {
        let model = UfoModel::load(model).unwrap();
        for parameter in model.parameters.values() {
            if let Parameter::Internal {
                expr: UfoMath(expr),
                ..
            } = parameter
            {
                let _: MathExprPlain = super::parse_math(&expr).unwrap();
            }
        }
        for lorentz in model.lorentz_structures.values() {
            let ufo::UfoMath(ref expr) = lorentz.structure;
            let _: LorentzExpr = super::parse_math(&expr).unwrap();
        }
        for vertex in model.vertices.values() {
            for c in vertex.color.iter() {
                let ufo::UfoMath(ref expr) = c;
                let _: ColorExpr = super::parse_math(&expr).unwrap();
            }
        }
        for coupling in model.couplings.values() {
            match coupling.value {
                ufo::CouplingValue::Simple(ref expr) => {
                    let ufo::UfoMath(ref expr) = expr;
                    let _: ColorExpr = super::parse_math(&expr).unwrap();
                }
                ufo::CouplingValue::Orders(ref orders) => {
                    for o in orders.values() {
                        let ufo::UfoMath(ref expr) = o;
                        let _: ColorExpr = super::parse_math(&expr).unwrap();
                    }
                }
            }
        }
        for function in model.function_library.values() {
            let ufo::UfoMath(ref expr) = function.expr;
            let _: MathExprPlain = super::parse_math(&expr).unwrap();
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

    #[test]
    #[ignore]
    fn parse_loop_mssm() {
        parse_model("tests/models_json/loop_MSSM");
    }
}
