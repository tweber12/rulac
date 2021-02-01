use crate::math_expr::lorentz::LorentzTensor;
use crate::math_expr::parse::{parse_math_alias, ParseError, ParseMode};
use crate::math_expr::MathExpr;
use serde::Deserialize;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::Read;
use std::path::Path;

#[derive(Clone, Debug, PartialEq)]
pub struct Propagators {
    spin_zero: Propagator,
    spin_one_half: Propagator,
    spin_one_massive: Propagator,
    spin_one_massless: Propagator,
    spin_three_half_massive: Propagator,
    spin_three_half_massless: Propagator,
    spin_two_massive: Propagator,
    spin_two_massless: Propagator,
    pub custom: HashMap<String, Propagator>,
}
impl Propagators {
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Propagators, PropagatorsError> {
        let mut contents = Vec::new();
        fs::File::open(path)?.read_to_end(&mut contents)?;
        let propagators: PropagatorsStore = toml::from_slice(&contents)?;
        Ok(Propagators {
            spin_zero: Propagator::new_basic(propagators.spin_zero)?,
            spin_one_half: Propagator::new_fermion(propagators.spin_one_half)?,
            spin_one_massive: Propagator::new_basic(propagators.spin_one_massive)?,
            spin_one_massless: Propagator::new_basic(propagators.spin_one_massless)?,
            spin_three_half_massive: Propagator::new_fermion(propagators.spin_three_half_massive)?,
            spin_three_half_massless: Propagator::new_fermion(
                propagators.spin_three_half_massless,
            )?,
            spin_two_massive: Propagator::new_basic(propagators.spin_two_massive)?,
            spin_two_massless: Propagator::new_basic(propagators.spin_two_massless)?,
            custom: HashMap::new(),
        })
    }
    pub fn spin_zero(&self, _mass: f64) -> &Propagator {
        &self.spin_zero
    }
    pub fn spin_one_half(&self, _mass: f64) -> &Propagator {
        &self.spin_one_half
    }
    pub fn spin_one(&self, mass: f64) -> &Propagator {
        if mass == 0f64 {
            &self.spin_one_massless
        } else {
            &self.spin_one_massive
        }
    }
    pub fn spin_three_half(&self, mass: f64) -> &Propagator {
        if mass == 0f64 {
            &self.spin_three_half_massless
        } else {
            &self.spin_three_half_massive
        }
    }
    pub fn spin_two(&self, mass: f64) -> &Propagator {
        if mass == 0f64 {
            &self.spin_two_massless
        } else {
            &self.spin_two_massive
        }
    }
}

/// A full propagator including different expressions for incoming and outgoing particles.
/// This distinction is necessary for fermions and is how UFO encodes custom particle
/// propagators.
#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct Propagator {
    pub incoming: BasicPropagator,
    pub outgoing: BasicPropagator,
}
impl Propagator {
    fn new_basic(stored: PropagatorStore) -> Result<Propagator, PropagatorsError> {
        let propagator = BasicPropagator::from_stored(stored)?;
        Ok(Propagator {
            incoming: propagator.clone(),
            outgoing: propagator,
        })
    }
    fn new_fermion(stored: PropagatorStore) -> Result<Propagator, PropagatorsError> {
        let incoming = BasicPropagator::from_stored(stored)?;
        let outgoing = incoming.reverse_momentum();
        Ok(Propagator { incoming, outgoing })
    }
}

/// All indices that connect to stuff going to the 'left' of the propagator should be based on
/// `1`, while the ones connected on the right should be based on `2`.
/// Based on means here that the left ones for spin two are 1001 and 2001, while the right ones
/// are 1002 and 2002.
/// The particle index used for the momentum is irrelevant, as long as it's still a number. Any
/// momentum appearing in the propagator is assumed to refer to the propagating particle, i.e.
/// `P(1,1)` and `P(329,1)` are treated as identical.
/// The mass of the particle can be refered to by the variable `mass`.
#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct BasicPropagator {
    pub numerator: MathExpr,
    pub denominator: MathExpr,
}
impl BasicPropagator {
    fn from_stored(stored: PropagatorStore) -> Result<BasicPropagator, PropagatorsError> {
        let numerator = normalize_momentum(&parse_math_alias(
            &stored.numerator,
            ParseMode::Lorentz,
            stored.aliases.clone(),
        )?);
        let denominator = normalize_momentum(&parse_math_alias(
            &stored.denominator,
            ParseMode::Normal,
            stored.aliases,
        )?);
        Ok(BasicPropagator {
            numerator,
            denominator,
        })
    }
    fn reverse_momentum(&self) -> BasicPropagator {
        BasicPropagator {
            numerator: reverse_momentum(&self.numerator),
            denominator: self.denominator.clone(),
        }
    }
}

#[derive(Debug)]
pub enum PropagatorsError {
    IoError(std::io::Error),
    Toml(toml::de::Error),
    ParseMath(ParseError),
}
impl From<std::io::Error> for PropagatorsError {
    fn from(err: std::io::Error) -> PropagatorsError {
        PropagatorsError::IoError(err)
    }
}
impl From<toml::de::Error> for PropagatorsError {
    fn from(err: toml::de::Error) -> PropagatorsError {
        PropagatorsError::Toml(err)
    }
}
impl From<ParseError> for PropagatorsError {
    fn from(err: ParseError) -> PropagatorsError {
        PropagatorsError::ParseMath(err)
    }
}
impl fmt::Display for PropagatorsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropagatorsError::IoError(err) => write!(f, "Failed to load propagators file: {}", err),
            PropagatorsError::Toml(err) => write!(f, "Failed to parse propagators file: {}", err),
            PropagatorsError::ParseMath(err) => write!(
                f,
                "Failed to convert math expression in propagators file: {}",
                err
            ),
        }
    }
}
impl std::error::Error for PropagatorsError {}

#[derive(Clone, Debug, PartialEq, Deserialize)]
struct PropagatorsStore {
    spin_zero: PropagatorStore,
    spin_one_half: PropagatorStore,
    spin_one_massive: PropagatorStore,
    spin_one_massless: PropagatorStore,
    spin_three_half_massive: PropagatorStore,
    spin_three_half_massless: PropagatorStore,
    spin_two_massive: PropagatorStore,
    spin_two_massless: PropagatorStore,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
struct PropagatorStore {
    numerator: String,
    denominator: String,
    #[serde(default)]
    aliases: HashMap<String, i64>,
}

fn normalize_momentum(expr: &MathExpr) -> MathExpr {
    match expr {
        MathExpr::LorentzTensor { lorentz } => match lorentz {
            LorentzTensor::Momentum { mu1, particle: _ } => MathExpr::LorentzTensor {
                lorentz: LorentzTensor::Momentum {
                    particle: 0,
                    mu1: *mu1,
                },
            },
            _ => expr.clone(),
        },
        _ => expr.apply_on_subexpressions(&mut |e| reverse_momentum(e)),
    }
}

fn reverse_momentum(expr: &MathExpr) -> MathExpr {
    match expr {
        MathExpr::LorentzTensor { lorentz } => match lorentz {
            LorentzTensor::Momentum { .. } => -(expr.clone()),
            _ => expr.clone(),
        },
        _ => expr.apply_on_subexpressions(&mut |e| reverse_momentum(e)),
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn load() {
        super::Propagators::load("models/common/propagators.toml").unwrap();
    }
}
