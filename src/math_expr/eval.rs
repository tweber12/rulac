use crate::math_expr::parse::{parse_math, ParseError};
use crate::math_expr::{
    BinaryOperator, Comparison, ComparisonOperator, Function, MathExpr, MathExprPlain, Number,
    Tensor, UnaryOperator,
};
use crate::ufo::parameters::ExternalParameters;
use crate::ufo::{Parameter, UfoMath, UfoModel};
use num_complex::Complex64;
use std::collections::HashMap;

pub struct EvalContext {
    variables: HashMap<String, Number>,
}
impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext {
            variables: HashMap::new(),
        }
    }
    pub fn for_model(
        parameters: &ExternalParameters,
        model: &UfoModel,
    ) -> Result<EvalContext, EvalError> {
        let mut context = EvalContext::new();
        for (name, value) in parameters.data.values().flat_map(|block| block.iter()) {
            context.add_variable(name.clone(), *value);
        }
        let internal = model.parameters.values().filter_map(|p| match p {
            Parameter::Internal { name, expr, .. } => Some((name, expr)),
            Parameter::External { .. } => None,
        });
        context.add_computed_variables(internal)?;
        Ok(context)
    }
    pub fn add_variable<N: Into<Number>>(&mut self, name: String, value: N) {
        self.variables.insert(name, value.into());
    }
    pub fn add_computed_variables<'a, I: Iterator<Item = (&'a String, &'a UfoMath)>>(
        &mut self,
        variables: I,
    ) -> Result<(), EvalError> {
        let mut added = 0;
        let mut remaining = Vec::new();
        let mut undefined = Vec::new();
        for variable in variables {
            let (name, expr) = variable;
            match eval_ufo_math(expr, self) {
                Ok(n) => {
                    added += 1;
                    self.add_variable(name.clone(), n);
                }
                Err(EvalError::UndefinedVariables(s)) => {
                    remaining.push(variable);
                    undefined.extend(s);
                }
                _ => panic!("Math error"),
            }
        }
        if added == 0 && !undefined.is_empty() {
            Err(EvalError::UndefinedVariables(undefined))
        } else if remaining.is_empty() {
            Ok(())
        } else {
            self.add_computed_variables(remaining.into_iter())
        }
    }
    fn lookup(&self, variable: &str) -> Option<Number> {
        self.variables.get(variable).copied()
    }
}

#[derive(Debug)]
pub enum EvalError {
    ParseError(ParseError),
    UndefinedVariables(Vec<String>),
    ComplexAbs,
    InvalidReOrIm,
    TensorInExpr,
    SumInExpr,
    EmptyComparison,
}
impl From<ParseError> for EvalError {
    fn from(err: ParseError) -> EvalError {
        EvalError::ParseError(err)
    }
}

pub fn eval_ufo_math(ufo_math: &UfoMath, context: &EvalContext) -> Result<Number, EvalError> {
    let UfoMath(str) = ufo_math;
    let expr: MathExprPlain = parse_math(str)?;
    eval_expr(&expr, context)
}

pub fn eval_expr<T: Tensor>(
    expr: &MathExpr<T>,
    context: &EvalContext,
) -> Result<Number, EvalError> {
    match expr {
        MathExpr::Number { value } => Ok(*value),
        MathExpr::Variable { name } => context
            .lookup(name)
            .ok_or_else(|| EvalError::UndefinedVariables(vec![name.clone()])),
        MathExpr::BinaryOp {
            operator,
            left,
            right,
        } => eval_binary_op(*operator, left, right, context),
        MathExpr::UnaryOp { operator, operand } => eval_unary_op(*operator, operand, context),
        MathExpr::Call { function, args } => eval_call(function, args, context),
        MathExpr::Sum { .. } => Err(EvalError::SumInExpr),
        MathExpr::Tensor { .. } => Err(EvalError::TensorInExpr),
        MathExpr::Conditional {
            condition,
            if_true,
            if_false,
        } => eval_conditional(condition, if_true, if_false, context),
        _ => unimplemented!(),
    }
}

fn eval_binary_op<T: Tensor>(
    operator: BinaryOperator,
    left: &MathExpr<T>,
    right: &MathExpr<T>,
    context: &EvalContext,
) -> Result<Number, EvalError> {
    let left = eval_expr(left, context)?;
    let right = eval_expr(right, context)?;
    let number = match operator {
        BinaryOperator::Add => left + right,
        BinaryOperator::Sub => left - right,
        BinaryOperator::Mul => left * right,
        BinaryOperator::Div => left / right,
        BinaryOperator::Pow => left.pow(right),
    };
    Ok(number)
}

fn eval_unary_op<T: Tensor>(
    operator: UnaryOperator,
    operand: &MathExpr<T>,
    context: &EvalContext,
) -> Result<Number, EvalError> {
    let operand = eval_expr(operand, context)?;
    match operator {
        UnaryOperator::Plus => Ok(operand),
        UnaryOperator::Minus => Ok(-operand),
    }
}

fn eval_call<T: Tensor>(
    function: &Function,
    args: &[MathExpr<T>],
    context: &EvalContext,
) -> Result<Number, EvalError> {
    let args: Vec<_> = args
        .iter()
        .map(|e| eval_expr(e, context))
        .collect::<Result<_, _>>()?;
    let number = match function {
        Function::Abs => return args[0].abs().ok_or(EvalError::ComplexAbs),
        Function::Cos => args[0].cos(),
        Function::Sin => args[0].sin(),
        Function::Tan => args[0].tan(),
        Function::ACos => args[0].acos(),
        Function::ASin => args[0].asin(),
        Function::ATan => args[0].atan(),
        Function::Sqrt => args[0].sqrt(),
        Function::Log => args[0].ln(),
        Function::Complex => {
            let re = to_complex_arg(args[0])?;
            let im = to_complex_arg(args[1])?;
            Number::Complex(Complex64::new(re, im))
        }
        Function::RealPart => args[0].re(),
        Function::ImaginaryPart => args[0].im(),
        Function::ComplexConjugate => args[0].complex_conjugate(),
        Function::Other(other) => unimplemented!("Function {} not implemented", other),
    };
    Ok(number)
}

fn to_complex_arg(number: Number) -> Result<f64, EvalError> {
    match number {
        Number::Integer(i) => Ok(i as f64),
        Number::Real(f) => Ok(f),
        Number::Complex(_) => Err(EvalError::InvalidReOrIm),
    }
}

fn eval_conditional<T: Tensor>(
    comparison: &Comparison<T>,
    if_true: &MathExpr<T>,
    if_false: &MathExpr<T>,
    context: &EvalContext,
) -> Result<Number, EvalError> {
    if eval_condition(comparison, context)? {
        eval_expr(if_true, context)
    } else {
        eval_expr(if_false, context)
    }
}

fn eval_condition<T: Tensor>(
    comparison: &Comparison<T>,
    context: &EvalContext,
) -> Result<bool, EvalError> {
    let mut values = comparison.values.iter();
    let mut last = eval_expr(values.next().ok_or(EvalError::EmptyComparison)?, context)?;
    for (op, value) in comparison.operators.iter().zip(values) {
        let value = eval_expr(value, context)?;
        match op {
            ComparisonOperator::Equals if last != value => return Ok(false),
            ComparisonOperator::NotEqual if last == value => return Ok(false),
            _ => last = value,
        }
    }
    Ok(true)
}

#[cfg(test)]
mod test {
    use crate::math_expr::eval::{eval_ufo_math, EvalContext};
    use crate::math_expr::Number;
    use crate::ufo::parameters::ExternalParameters;
    use crate::ufo::{UfoMath, UfoModel};
    use num_complex::Complex64;
    use std::f64::consts::{PI, SQRT_2};

    #[test]
    fn one_plus_one() {
        assert_eq!(
            eval_ufo_math(&UfoMath("1+1".to_string()), &EvalContext::new()).unwrap(),
            Number::from(2)
        );
    }

    #[test]
    fn three_minus_five() {
        assert_eq!(
            eval_ufo_math(&UfoMath("3-5".to_string()), &EvalContext::new()).unwrap(),
            Number::from(-2)
        );
    }

    #[test]
    fn eight_times_seven() {
        assert_eq!(
            eval_ufo_math(&UfoMath("8*7".to_string()), &EvalContext::new()).unwrap(),
            Number::from(56)
        );
    }

    #[test]
    fn three_divided_by_2_integer() {
        assert_eq!(
            eval_ufo_math(&UfoMath("3/2".to_string()), &EvalContext::new()).unwrap(),
            Number::from(1)
        );
    }

    #[test]
    fn three_divided_by_2_float() {
        assert_eq!(
            eval_ufo_math(&UfoMath("3/2.".to_string()), &EvalContext::new()).unwrap(),
            Number::from(1.5)
        );
    }

    #[test]
    fn two_to_three() {
        assert_eq!(
            eval_ufo_math(&UfoMath("2**3".to_string()), &EvalContext::new()).unwrap(),
            Number::from(8)
        );
    }

    #[test]
    fn cos_half() {
        assert_eq!(
            eval_ufo_math(&UfoMath("cmath.cos(1/2.)".to_string()), &EvalContext::new()).unwrap(),
            Number::from(0.5_f64.cos())
        );
    }

    #[test]
    fn one_equal_one() {
        assert_eq!(
            eval_ufo_math(
                &UfoMath("4 if 1==1 else -4".to_string()),
                &EvalContext::new()
            )
            .unwrap(),
            Number::from(4)
        );
    }

    #[test]
    fn one_equal_two() {
        assert_eq!(
            eval_ufo_math(
                &UfoMath("4 if 1==2 else -4".to_string()),
                &EvalContext::new()
            )
            .unwrap(),
            Number::from(-4)
        );
    }

    #[test]
    fn one_not_equal_two() {
        assert_eq!(
            eval_ufo_math(
                &UfoMath("4 if 1!=2 else -4".to_string()),
                &EvalContext::new()
            )
            .unwrap(),
            Number::from(4)
        );
    }

    #[test]
    fn one_equal_one_not_equal_one() {
        assert_eq!(
            eval_ufo_math(
                &UfoMath("4 if 1==1!=1 else -4".to_string()),
                &EvalContext::new()
            )
            .unwrap(),
            Number::from(-4)
        );
    }

    #[test]
    fn load_parameters() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let parameters =
            ExternalParameters::load("tests/models_json/sm_mg5/parameters.toml").unwrap();
        let context = EvalContext::for_model(&parameters, &model).unwrap();
        // External
        let mz = 91.188_f64;
        let aewm1 = 132.50698_f64;
        let gf = 0.0000116639_f64;
        let aws = Complex64::new(0.808, 0.);
        let lamws = 0.2253_f64;
        let etaws = 0.341_f64;
        let rhows = 0.132_f64;
        let ymb = 4.2_f64;
        // Internal
        let aew = 1. / aewm1;
        let mw = (mz.powi(2) / 2.
            + (mz.powi(4) / 4. - (aew * PI * mz.powi(2)) / (gf * SQRT_2)).sqrt())
        .sqrt();
        let sw = (1. - mw.powi(2) / mz.powi(2)).sqrt();
        let ckm1x3 = aws * lamws.powi(3) * (-(etaws * Complex64::i()) + rhows);
        let ee = 2. * aew.sqrt() * PI.sqrt();
        let vev = (2. * mw * sw) / ee;
        let yb = (ymb * SQRT_2) / vev;
        let i4x13 = ckm1x3 * yb;
        assert_eq!(context.lookup("aEW").unwrap(), Number::from(aew));
        assert_eq!(context.lookup("MW").unwrap(), Number::from(mw));
        assert_eq!(context.lookup("sw").unwrap(), Number::from(sw));
        assert_eq!(context.lookup("CKM1x3").unwrap(), Number::from(ckm1x3));
        assert_eq!(context.lookup("I4x13").unwrap(), Number::from(i4x13));
    }
}
