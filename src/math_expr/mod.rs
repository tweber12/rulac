pub mod lorentz;

use num_complex::Complex64;
use serde::{Deserialize, Serialize};
use num_traits::identities::Zero;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum Function {
    Abs,
    Cos,
    Sin,
    Sqrt,
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
impl IndexRange for FundamentalIndex {
    fn range() -> std::ops::Range<u8> {
        1..4
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AdjointIndex(i64);
impl IndexRange for AdjointIndex {
    fn range() -> std::ops::Range<u8> {
        1..9
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SextetIndex(i64);
impl IndexRange for SextetIndex {
    fn range() -> std::ops::Range<u8> {
        1..7
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag="tensor")]
pub enum ColorTensor {
    #[serde(rename = "Identity")]
    KroneckerDelta { i1: FundamentalIndex, jb2: FundamentalIndex },
    #[serde(rename = "T")]
    FundamentalRep { a1: AdjointIndex, i2: FundamentalIndex, jb3: FundamentalIndex },
    #[serde(rename = "f")]
    StructureConstant { a1: AdjointIndex, a2: AdjointIndex, a3: AdjointIndex },
    #[serde(rename = "d")]
    SymmetricTensor { a1: AdjointIndex, a2: AdjointIndex, a3: AdjointIndex },
    Epsilon {i1: FundamentalIndex, i2: FundamentalIndex, i3: FundamentalIndex},
    EpsilonBar {ib1: FundamentalIndex, ib2: FundamentalIndex, ib3: FundamentalIndex},
    #[serde(rename = "T6")]
    SextetRep {a1: AdjointIndex, alpha2: SextetIndex, betab3: SextetIndex},
    #[serde(rename = "K6")]
    SextetClebschGordan{alpha1: SextetIndex, ib2: FundamentalIndex, jb3: FundamentalIndex},
    #[serde(rename = "K6Bar")]
    AntiSextetClebschGordan{alphab1: SextetIndex, i2: FundamentalIndex, j3: FundamentalIndex},
}

pub trait IndexRange {
    fn range() -> std::ops::Range<u8>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SummationIndex {
    Fundamental {index: FundamentalIndex},
    Adjoint {index: AdjointIndex},
    Sextet {index: SextetIndex},
    Lorentz {index: lorentz::LorentzIndex},
    Spinor {index: lorentz::SpinorIndex},
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum MathExpr {
    Number {
        value: f64,
    },
    Complex {
        value: Complex64,
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
        lorentz: lorentz::LorentzTensor
    },
    ColorTensor {
        #[serde(flatten)]
        color: ColorTensor
    },
    Comparison {
        left: Box<MathExpr>,
        operators: Vec<ComparisonOperator>,
        comparators: Vec<MathExpr>,
    },
    Sum {
        expr: Box<MathExpr>,
        index: SummationIndex,
    }
}
impl MathExpr {
    fn constant_propagation(self) -> MathExpr {
        match self {
            MathExpr::UnaryOp { operator, operand } => {
                let op = operand.constant_propagation();
                if let Some(value) = op.get_value() {
                    if operator == UnaryOperator::Minus {
                        MathExpr::with_value(-value)
                    } else {
                        MathExpr::with_value(value)
                    }
                } else {
                    MathExpr::UnaryOp { operator, operand: Box::new(op)}
                }
            }
            MathExpr::BinaryOp { operator, left, right } => constant_propagation_binary(operator, left.constant_propagation(), right.constant_propagation()),
            _ => self,
        }
    }
    fn with_value(value: Complex64) -> MathExpr {
        if value.im == 0f64 {
            MathExpr::Number { value: value.re }
        } else {
            MathExpr::Complex { value }
        }
    }
    fn get_value(&self) -> Option<Complex64> {
        match self {
            MathExpr::Number { value } => Some(Complex64::new(*value,0f64)),
            MathExpr::Complex { value } => Some(*value),
            _ => None,
        }
    }
}

fn constant_propagation_binary(operator: BinaryOperator, left: MathExpr, right: MathExpr) -> MathExpr {
    let vl = left.get_value();
    let vr = right.get_value();
    match operator {
        BinaryOperator::Mul => {
            match (vl,vr) {
                (Some(l), Some(r)) => return MathExpr::with_value(l*r),
                (Some(l), _) if l.is_zero() => return MathExpr::Number { value: 0f64 },
                (_, Some(r)) if r.is_zero() => return MathExpr::Number { value: 0f64 },
                _ => (),
            }
        }
        BinaryOperator::Add => {
            match (vl, vr) {
                (Some(l), Some(r)) => return MathExpr::with_value(l+r),
                (Some(l), _) if l.is_zero() => return right,
                (_, Some(r)) if r.is_zero() => return left,
                _ => (),
            }
        }
        BinaryOperator::Sub => {
            match (vl, vr) {
                (Some(l), Some(r)) => return MathExpr::with_value(l-r),
                (Some(l), _) if l.is_zero() => return MathExpr::UnaryOp { operator: UnaryOperator::Minus, operand: Box::new(right) },
                (_, Some(r)) if r.is_zero() => return left,
                _ => (),
            }
        }
        BinaryOperator::Div => {
            match (vl, vr) {
                (Some(l), Some(r)) if !r.is_zero() => return MathExpr::with_value(l/r),
                (Some(l), None) if l.is_zero() => return MathExpr::Number { value: 0f64 },
                _ => (),
            }
        }
        _ => (),
    };
    MathExpr::BinaryOp {
        operator: operator,
        left: Box::new(left),
        right: Box::new(right),
    }
}
