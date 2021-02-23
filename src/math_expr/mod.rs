mod eval;
mod number;
pub mod parse;

use num_traits::identities::Zero;
use serde::{Deserialize, Serialize};
use std::ops;

use crate::math_expr::eval::eval_expr;
pub use eval::{EvalContext, EvalError};
pub use number::Number;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum Function {
    Abs,
    Cos,
    Sin,
    Tan,
    ASin,
    ACos,
    ATan,
    Sqrt,
    Log,
    Complex,
    ComplexConjugate,
    RealPart,
    ImaginaryPart,
    Other(String),
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

#[derive(Clone, Debug, PartialEq)]
pub struct Comparison<T: Tensor> {
    values: Vec<MathExpr<T>>,
    operators: Vec<ComparisonOperator>,
}
impl<T: Tensor> Comparison<T> {
    pub fn apply<F>(&self, fun: &mut F) -> Comparison<T>
    where
        F: FnMut(&MathExpr<T>) -> MathExpr<T>,
    {
        Comparison {
            operators: self.operators.clone(),
            values: self.values.iter().map(|x| fun(x)).collect(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ComparisonOperator {
    Equals,
    NotEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MathExpr<T: Tensor> {
    Number {
        value: Number,
    },
    Call {
        function: Function,
        args: Vec<MathExpr<T>>,
    },
    Variable {
        name: String,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<MathExpr<T>>,
    },
    BinaryOp {
        operator: BinaryOperator,
        left: Box<MathExpr<T>>,
        right: Box<MathExpr<T>>,
    },
    Conditional {
        // The Comparison doesn't need to be boxed, it is however 32 bytes large, so that using it
        // unboxed more than doubles the (minimum) size of MathExpr from 24 bytes to 56 bytes
        condition: Box<Comparison<T>>,
        if_true: Box<MathExpr<T>>,
        if_false: Box<MathExpr<T>>,
    },
    Tensor {
        tensor: T,
    },
    Sum {
        expr: Box<MathExpr<T>>,
        index: T::Indices,
    },
    ExternalComponent {
        component: T::ExternalComponent,
    },
}
impl<T: Tensor> MathExpr<T> {
    pub fn eval(&self, context: &EvalContext) -> Result<Number, EvalError> {
        eval_expr(self, context)
    }
    pub fn apply_on_subexpressions<F>(&self, fun: &mut F) -> MathExpr<T>
    where
        F: FnMut(&MathExpr<T>) -> MathExpr<T>,
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
            MathExpr::Conditional {
                condition,
                if_true,
                if_false,
            } => MathExpr::Conditional {
                condition: Box::new(condition.apply(fun)),
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
    pub fn constant_propagation(&self) -> MathExpr<T> {
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
    pub fn extract_number(&self) -> Option<Number> {
        match self {
            MathExpr::Number { value } => Some(*value),
            _ => None,
        }
    }
}

macro_rules! impl_binary_op_expr {
    ($trait:ident, $op:ident) => {
        impl<T: Tensor> ops::$trait<MathExpr<T>> for MathExpr<T> {
            type Output = MathExpr<T>;
            fn $op(self, other: MathExpr<T>) -> MathExpr<T> {
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

impl<T: Tensor> ops::Neg for MathExpr<T> {
    type Output = MathExpr<T>;
    fn neg(self) -> MathExpr<T> {
        MathExpr::UnaryOp {
            operator: UnaryOperator::Minus,
            operand: Box::new(self),
        }
    }
}

impl<T: Tensor> std::iter::Sum for MathExpr<T> {
    fn sum<I>(iter: I) -> MathExpr<T>
    where
        I: Iterator<Item = MathExpr<T>>,
    {
        iter.fold(
            MathExpr::Number {
                value: Number::from(0),
            },
            ops::Add::add,
        )
    }
}

impl<T: Tensor> Default for MathExpr<T> {
    fn default() -> MathExpr<T> {
        MathExpr::Number {
            value: Number::from(0),
        }
    }
}

fn constant_propagation_binary<T: Tensor>(
    operator: BinaryOperator,
    left: MathExpr<T>,
    right: MathExpr<T>,
) -> MathExpr<T> {
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

pub trait Tensor: Clone + std::fmt::Debug + PartialEq {
    type Indices: TensorIndex;
    type ExternalComponent: Clone + std::fmt::Debug + PartialEq;
    fn parse(
        name: &str,
        indices: &mut parse::IndexParser<Self::Indices>,
    ) -> Result<Option<Self>, parse::ConversionError>;
}

pub trait TensorIndex: Copy + std::fmt::Debug + PartialEq {
    fn normalize(self) -> Self {
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct NoTensor {}
impl Tensor for NoTensor {
    type Indices = NoIndices;
    type ExternalComponent = NoComponents;
    fn parse(
        _name: &str,
        _indices: &mut parse::IndexParser<NoIndices>,
    ) -> Result<Option<NoTensor>, parse::ConversionError> {
        Ok(None)
    }
}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct NoIndices {}
impl TensorIndex for NoIndices {}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct NoComponents {}

pub type MathExprPlain = MathExpr<NoTensor>;
