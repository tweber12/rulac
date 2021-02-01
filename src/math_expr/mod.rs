pub mod lorentz;
pub mod parse;

use num_complex::Complex64;
use num_traits::identities::Zero;
use serde::{Deserialize, Serialize};
use std::ops;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum Function {
    Abs,
    Cos,
    Sin,
    Tan,
    ASin,
    ACos,
    Sqrt,
    Log,
    Complex,
    ComplexConjugate,
    RealPart,
    ImaginaryPart,
    Other(String),
}

fn deserialize_function<'de, D>(deserializer: D) -> Result<Function, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    match s.as_ref() {
        "abs" => Ok(Function::Abs),
        "cos" => Ok(Function::Cos),
        "sin" => Ok(Function::Sin),
        "sqrt" => Ok(Function::Sqrt),
        "complex" => Ok(Function::Complex),
        "__complex_conjugate__" => Ok(Function::ComplexConjugate),
        "__real_" => Ok(Function::RealPart),
        "__imag__" => Ok(Function::ImaginaryPart),
        _ => Ok(Function::Other(s)),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FundamentalIndex(i64);
impl From<i64> for FundamentalIndex {
    fn from(index: i64) -> FundamentalIndex {
        FundamentalIndex(index)
    }
}
impl IndexRange for FundamentalIndex {
    fn range() -> std::ops::Range<u8> {
        1..4
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AdjointIndex(i64);
impl From<i64> for AdjointIndex {
    fn from(index: i64) -> AdjointIndex {
        AdjointIndex(index)
    }
}
impl IndexRange for AdjointIndex {
    fn range() -> std::ops::Range<u8> {
        1..9
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SextetIndex(i64);
impl From<i64> for SextetIndex {
    fn from(index: i64) -> SextetIndex {
        SextetIndex(index)
    }
}
impl IndexRange for SextetIndex {
    fn range() -> std::ops::Range<u8> {
        1..7
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "tensor")]
pub enum ColorTensor {
    #[serde(rename = "Identity")]
    KroneckerDelta {
        i1: FundamentalIndex,
        jb2: FundamentalIndex,
    },
    #[serde(rename = "T")]
    FundamentalRep {
        a1: AdjointIndex,
        i2: FundamentalIndex,
        jb3: FundamentalIndex,
    },
    #[serde(rename = "f")]
    StructureConstant {
        a1: AdjointIndex,
        a2: AdjointIndex,
        a3: AdjointIndex,
    },
    #[serde(rename = "d")]
    SymmetricTensor {
        a1: AdjointIndex,
        a2: AdjointIndex,
        a3: AdjointIndex,
    },
    Epsilon {
        i1: FundamentalIndex,
        i2: FundamentalIndex,
        i3: FundamentalIndex,
    },
    EpsilonBar {
        ib1: FundamentalIndex,
        ib2: FundamentalIndex,
        ib3: FundamentalIndex,
    },
    #[serde(rename = "T6")]
    SextetRep {
        a1: AdjointIndex,
        alpha2: SextetIndex,
        betab3: SextetIndex,
    },
    #[serde(rename = "K6")]
    SextetClebschGordan {
        alpha1: SextetIndex,
        ib2: FundamentalIndex,
        jb3: FundamentalIndex,
    },
    #[serde(rename = "K6Bar")]
    AntiSextetClebschGordan {
        alphab1: SextetIndex,
        i2: FundamentalIndex,
        j3: FundamentalIndex,
    },
}

pub trait IndexRange {
    fn range() -> std::ops::Range<u8>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SummationIndex {
    Fundamental { index: FundamentalIndex },
    Adjoint { index: AdjointIndex },
    Sextet { index: SextetIndex },
    Lorentz { index: lorentz::LorentzIndex },
    Spinor { index: lorentz::SpinorIndex },
}
impl SummationIndex {
    fn range(&self) -> std::ops::Range<u8> {
        match self {
            SummationIndex::Fundamental { .. } => FundamentalIndex::range(),
            SummationIndex::Sextet { .. } => SextetIndex::range(),
            SummationIndex::Adjoint { .. } => AdjointIndex::range(),
            SummationIndex::Lorentz { .. } => lorentz::LorentzIndex::range(),
            SummationIndex::Spinor { .. } => lorentz::SpinorIndex::range(),
        }
    }
}
impl From<FundamentalIndex> for SummationIndex {
    fn from(index: FundamentalIndex) -> SummationIndex {
        SummationIndex::Fundamental { index }
    }
}
impl From<SextetIndex> for SummationIndex {
    fn from(index: SextetIndex) -> SummationIndex {
        SummationIndex::Sextet { index }
    }
}
impl From<AdjointIndex> for SummationIndex {
    fn from(index: AdjointIndex) -> SummationIndex {
        SummationIndex::Adjoint { index }
    }
}
impl From<lorentz::LorentzIndex> for SummationIndex {
    fn from(index: lorentz::LorentzIndex) -> SummationIndex {
        SummationIndex::Lorentz { index }
    }
}
impl From<lorentz::SpinorIndex> for SummationIndex {
    fn from(index: lorentz::SpinorIndex) -> SummationIndex {
        SummationIndex::Spinor { index }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ComparisonOperator {
    Equals,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Constant {
    Pi,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Number {
    Integer(i64),
    Real(f64),
    Complex(Complex64),
}
impl Number {
    pub fn as_complex(self) -> Complex64 {
        match self {
            Number::Integer(i) => Complex64::new(i as f64, 0f64),
            Number::Real(f) => Complex64::new(f, 0f64),
            Number::Complex(c) => c,
        }
    }
}
macro_rules! impl_op {
    ($trait:ident, $op:ident) => {
        impl ops::$trait<Number> for Number {
            type Output = Number;
            fn $op(self, other: Number) -> Number {
                match (self, other) {
                    (Number::Integer(i), Number::Integer(j)) => Number::Integer(i.$op(j)),
                    (Number::Integer(i), Number::Real(f)) => Number::Real((i as f64).$op(f)),
                    (Number::Integer(i), Number::Complex(c)) => {
                        Number::Complex(Complex64::new(i as f64, 0f64).$op(c))
                    }
                    (Number::Real(f), Number::Integer(j)) => Number::Real(f.$op(j as f64)),
                    (Number::Real(f), Number::Real(g)) => Number::Real(f.$op(g)),
                    (Number::Real(f), Number::Complex(c)) => {
                        Number::Complex(Complex64::new(f, 0f64).$op(c))
                    }
                    (Number::Complex(c), Number::Integer(j)) => {
                        Number::Complex(c.$op(Complex64::new(j as f64, 0f64)))
                    }
                    (Number::Complex(c), Number::Real(f)) => {
                        Number::Complex(c.$op(Complex64::new(f, 0f64)))
                    }
                    (Number::Complex(c), Number::Complex(d)) => Number::Complex(c.$op(d)),
                }
            }
        }
    };
}
impl_op!(Add, add);
impl_op!(Mul, mul);
impl_op!(Sub, sub);
impl_op!(Div, div);

impl ops::Neg for Number {
    type Output = Number;
    fn neg(self) -> Number {
        match self {
            Number::Integer(i) => Number::Integer(i.neg()),
            Number::Real(f) => Number::Real(f.neg()),
            Number::Complex(c) => Number::Complex(c.neg()),
        }
    }
}

impl Zero for Number {
    fn zero() -> Number {
        Number::Integer(0)
    }
    fn is_zero(&self) -> bool {
        match self {
            Number::Integer(i) => i.is_zero(),
            Number::Real(f) => f.is_zero(),
            Number::Complex(c) => c.is_zero(),
        }
    }
}

impl From<i64> for Number {
    fn from(other: i64) -> Number {
        Number::Integer(other)
    }
}
impl From<f64> for Number {
    fn from(other: f64) -> Number {
        Number::Real(other)
    }
}
impl From<Complex64> for Number {
    fn from(other: Complex64) -> Number {
        Number::Complex(other)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum MathExpr {
    Number {
        value: Number,
    },
    Call {
        #[serde(deserialize_with = "deserialize_function")]
        function: Function,
        args: Vec<MathExpr>,
    },
    Variable {
        name: String,
    },
    String {
        value: String,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<MathExpr>,
    },
    BinaryOp {
        operator: BinaryOperator,
        left: Box<MathExpr>,
        right: Box<MathExpr>,
    },
    Constant {
        name: Constant,
    },
    Conditional {
        condition: Box<MathExpr>,
        if_true: Box<MathExpr>,
        if_false: Box<MathExpr>,
    },
    LorentzTensor {
        #[serde(flatten)]
        lorentz: lorentz::LorentzTensor,
    },
    ColorTensor {
        #[serde(flatten)]
        color: ColorTensor,
    },
    Comparison {
        values: Vec<MathExpr>,
        operators: Vec<ComparisonOperator>,
    },
    Sum {
        expr: Box<MathExpr>,
        index: SummationIndex,
    },
}
impl MathExpr {
    pub fn apply_on_subexpressions<F>(&self, fun: &mut F) -> MathExpr
    where
        F: FnMut(&MathExpr) -> MathExpr,
    {
        match self {
            MathExpr::BinaryOp {
                operator,
                left,
                right,
            } => MathExpr::BinaryOp {
                operator: *operator,
                left: Box::new(fun(left)),
                right: Box::new(fun(right)),
            },
            MathExpr::Call { function, args } => MathExpr::Call {
                function: function.clone(),
                args: args.iter().map(|e| fun(e)).collect(),
            },
            MathExpr::Comparison { operators, values } => MathExpr::Comparison {
                operators: operators.clone(),
                values: values.iter().map(|e| fun(e)).collect(),
            },
            MathExpr::Conditional {
                condition,
                if_true,
                if_false,
            } => MathExpr::Conditional {
                condition: Box::new(fun(condition)),
                if_true: Box::new(fun(if_true)),
                if_false: Box::new(fun(if_false)),
            },
            MathExpr::Sum { expr, index } => MathExpr::Sum {
                expr: Box::new(fun(expr)),
                index: *index,
            },
            MathExpr::UnaryOp { operator, operand } => MathExpr::UnaryOp {
                operator: *operator,
                operand: Box::new(fun(operand)),
            },
            _ => self.clone(),
        }
    }
    fn constant_propagation(&self) -> MathExpr {
        match self {
            MathExpr::UnaryOp { operator, operand } => {
                let op = operand.constant_propagation();
                if let Some(value) = op.extract_number() {
                    if *operator == UnaryOperator::Minus {
                        MathExpr::Number { value: -value }
                    } else {
                        MathExpr::Number { value }
                    }
                } else {
                    MathExpr::UnaryOp {
                        operator: *operator,
                        operand: Box::new(op),
                    }
                }
            }
            MathExpr::BinaryOp {
                operator,
                left,
                right,
            } => constant_propagation_binary(
                *operator,
                left.constant_propagation(),
                right.constant_propagation(),
            ),
            _ => self.apply_on_subexpressions(&mut |e| e.constant_propagation()),
        }
    }
    fn extract_number(&self) -> Option<Number> {
        match self {
            MathExpr::Number { value } => Some(*value),
            _ => None,
        }
    }
}

macro_rules! impl_binary_op_expr {
    ($trait:ident, $op:ident) => {
        impl ops::$trait<MathExpr> for MathExpr {
            type Output = MathExpr;
            fn $op(self, other: MathExpr) -> MathExpr {
                MathExpr::BinaryOp {
                    operator: BinaryOperator::$trait,
                    left: Box::new(self),
                    right: Box::new(other),
                }
            }
        }
    };
}
impl_binary_op_expr!(Add, add);
impl_binary_op_expr!(Mul, mul);
impl_binary_op_expr!(Sub, sub);
impl_binary_op_expr!(Div, div);

impl ops::Neg for MathExpr {
    type Output = MathExpr;
    fn neg(self) -> MathExpr {
        MathExpr::UnaryOp {
            operator: UnaryOperator::Minus,
            operand: Box::new(self),
        }
    }
}

impl std::iter::Sum for MathExpr {
    fn sum<I>(iter: I) -> MathExpr
    where
        I: Iterator<Item = MathExpr>,
    {
        iter.fold(
            MathExpr::Number {
                value: Number::from(0),
            },
            ops::Add::add,
        )
    }
}

impl Default for MathExpr {
    fn default() -> MathExpr {
        MathExpr::Number {
            value: Number::from(0),
        }
    }
}

fn constant_propagation_binary(
    operator: BinaryOperator,
    left: MathExpr,
    right: MathExpr,
) -> MathExpr {
    let vl = left.extract_number();
    let vr = right.extract_number();
    match operator {
        BinaryOperator::Mul => match (vl, vr) {
            (Some(l), Some(r)) => return MathExpr::Number { value: l * r },
            (Some(l), _) if l.is_zero() => {
                return MathExpr::Number {
                    value: Number::zero(),
                }
            }
            (_, Some(r)) if r.is_zero() => {
                return MathExpr::Number {
                    value: Number::zero(),
                }
            }
            _ => (),
        },
        BinaryOperator::Add => match (vl, vr) {
            (Some(l), Some(r)) => return MathExpr::Number { value: l + r },
            (Some(l), _) if l.is_zero() => return right,
            (_, Some(r)) if r.is_zero() => return left,
            _ => (),
        },
        BinaryOperator::Sub => match (vl, vr) {
            (Some(l), Some(r)) => return MathExpr::Number { value: l - r },
            (Some(l), _) if l.is_zero() => {
                return MathExpr::UnaryOp {
                    operator: UnaryOperator::Minus,
                    operand: Box::new(right),
                }
            }
            (_, Some(r)) if r.is_zero() => return left,
            _ => (),
        },
        BinaryOperator::Div => match (vl, vr) {
            (Some(l), Some(r)) if !r.is_zero() => return MathExpr::Number { value: l / r },
            (Some(l), None) if l.is_zero() => {
                return MathExpr::Number {
                    value: Number::zero(),
                }
            }
            _ => (),
        },
        _ => (),
    };
    MathExpr::BinaryOp {
        operator,
        left: Box::new(left),
        right: Box::new(right),
    }
}
