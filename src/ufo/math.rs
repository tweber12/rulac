use serde::{Deserialize, Serialize};

#[derive(Serialize)]
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

#[derive(Serialize, Deserialize)]
pub enum LorentzTensor {
    #[serde(rename = "C")]
    ChargeConjugation,
    Epsilon,
    Gamma,
    Gamma5,
    #[serde(rename = "Identity")]
    KroneckerDelta,
    Metric,
    #[serde(rename = "P")]
    Momentum,
    ProjM,
    ProjP,
    Sigma,
}

#[derive(Serialize, Deserialize)]
pub enum ColorTensor {
    #[serde(rename = "Identity")]
    KroneckerDelta,
    #[serde(rename = "T")]
    FundamentalRep,
    #[serde(rename = "f")]
    StructureConstant,
    #[serde(rename = "d")]
    SymmetricTensor,
    Epsilon,
    EpsilonBar,
    #[serde(rename = "T6")]
    SextetRep,
    #[serde(rename = "K6")]
    SextetClebschGordan,
    #[serde(rename = "K6Bar")]
    AntiSextetClebschGordan,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ComparisonOperator {
    Equals,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Constant {
    Pi,
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum MathExpr {
    Number {
        value: f64,
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
        tensor: LorentzTensor,
        indices: Vec<MathExpr>,
    },
    ColorTensor {
        tensor: ColorTensor,
        indices: Vec<MathExpr>,
    },
    Comparison {
        left: Box<MathExpr>,
        operators: Vec<ComparisonOperator>,
        comparators: Vec<MathExpr>,
    },
}
