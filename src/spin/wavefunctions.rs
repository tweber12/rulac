use crate::math_expr::parse::{
    deserialize_math_array_four, deserialize_math_expr, deserialize_math_map,
};
use crate::math_expr::{MathExprPlain, Number};
use crate::ufo::{FermionKind, Particle, Spin};
use num_traits::Zero;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::path::Path;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Polarization(i64);
impl Polarization {
    fn flip(&self) -> Polarization {
        Polarization(self.0)
    }
}
impl<'de> Deserialize<'de> for Polarization {
    fn deserialize<D>(deserializer: D) -> Result<Polarization, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let string: &str = Deserialize::deserialize(deserializer)?;
        let int: i64 = i64::from_str(string).map_err(serde::de::Error::custom)?;
        Ok(Polarization(int))
    }
}

pub enum Beam {
    Pos,
    Neg,
    None,
}
impl Beam {
    fn expr(&self) -> MathExprPlain {
        let i = match self {
            Beam::Pos => 1,
            Beam::Neg => -1,
            Beam::None => 0,
        };
        MathExprPlain::Number {
            value: Number::from(i),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Wavefunctions {
    #[serde(deserialize_with = "deserialize_math_expr")]
    spin_zero: MathExprPlain,
    spin_one_half: SpinOneHalfStored,
    spin_one: SpinOneStored,
}
impl Wavefunctions {
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Wavefunctions, WavefunctionError> {
        let mut contents = Vec::new();
        fs::File::open(path).unwrap().read_to_end(&mut contents)?;
        toml::from_slice(&contents).map_err(WavefunctionError::from)
    }
    pub fn get_incoming(
        &self,
        particle: &Particle,
        mass: Number,
        polarization: Polarization,
    ) -> Wavefunction {
        self.get(
            particle.spin,
            mass,
            particle.fermion_kind(),
            ParticleDirection::Incoming,
            polarization,
        )
    }

    pub fn get_outgoing(
        &self,
        particle: &Particle,
        mass: Number,
        polarization: Polarization,
    ) -> Wavefunction {
        self.get(
            particle.spin,
            mass,
            particle.fermion_kind(),
            ParticleDirection::Outgoing,
            polarization,
        )
    }
    fn get(
        &self,
        spin: Spin,
        mass: Number,
        fermion_kind: FermionKind,
        direction: ParticleDirection,
        polarization: Polarization,
    ) -> Wavefunction {
        match spin {
            Spin::Zero => Wavefunction::Scalar(self.spin_zero.clone()),
            Spin::OneHalf => self
                .spin_one_half
                .get(mass, direction, fermion_kind, polarization),
            Spin::One => self.spin_one.get(direction, polarization),
            _ => panic!("BUG: All other spins should have been handled in Builder!"),
        }
    }
}

pub type Definitions = HashMap<String, MathExprPlain>;

#[derive(Clone, Debug, PartialEq)]
pub enum Wavefunction {
    Scalar(MathExprPlain),
    Vector {
        definitions: Definitions,
        components: Box<[MathExprPlain; 4]>,
    },
    Tensor {
        definitions: Definitions,
        components: Box<[[MathExprPlain; 4]; 4]>,
    },
}

#[derive(Debug)]
pub enum WavefunctionError {
    IoError(std::io::Error),
    Toml(toml::de::Error),
}
impl From<std::io::Error> for WavefunctionError {
    fn from(err: std::io::Error) -> WavefunctionError {
        WavefunctionError::IoError(err)
    }
}
impl From<toml::de::Error> for WavefunctionError {
    fn from(err: toml::de::Error) -> WavefunctionError {
        WavefunctionError::Toml(err)
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Hash, Eq, PartialEq, PartialOrd, Ord)]
enum ParticleDirection {
    #[serde(rename = "in")]
    Incoming,
    #[serde(rename = "out")]
    Outgoing,
}

#[derive(Debug, Clone, Deserialize)]
struct SpinOneHalfStored {
    massive: SpinOneHalfMassiveStored,
    massless: SpinOneHalfMasslessStored,
}
impl SpinOneHalfStored {
    fn get(
        &self,
        mass: Number,
        direction: ParticleDirection,
        fermion_kind: FermionKind,
        polarization: Polarization,
    ) -> Wavefunction {
        if mass.is_zero() {
            self.massless.get(direction, fermion_kind, polarization)
        } else {
            self.massive.get(direction, fermion_kind, polarization)
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
struct SpinOneHalfMassiveStored {
    #[serde(deserialize_with = "deserialize_math_map")]
    definitions: HashMap<String, MathExprPlain>,
    #[serde(flatten)]
    spinors:
        HashMap<FermionKind, HashMap<ParticleDirection, HashMap<Polarization, WavefunctionStored>>>,
}
impl SpinOneHalfMassiveStored {
    fn get(
        &self,
        direction: ParticleDirection,
        fermion_kind: FermionKind,
        polarization: Polarization,
    ) -> Wavefunction {
        let wf = &self.spinors[&fermion_kind][&direction][&polarization];
        Wavefunction::Vector {
            definitions: self.definitions.clone(),
            components: Box::new(wf.components.clone()),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
struct SpinOneHalfMasslessStored {
    #[serde(deserialize_with = "deserialize_math_map")]
    definitions: HashMap<String, MathExprPlain>,
    #[serde(flatten)]
    spinors: HashMap<ParticleDirection, HashMap<Polarization, WavefunctionStored>>,
}
impl SpinOneHalfMasslessStored {
    fn get(
        &self,
        direction: ParticleDirection,
        fermion_kind: FermionKind,
        polarization: Polarization,
    ) -> Wavefunction {
        let wf = match fermion_kind {
            FermionKind::Fermion => &self.spinors[&direction][&polarization],
            FermionKind::AntiFermion => &self.spinors[&direction][&polarization.flip()],
            FermionKind::None => unimplemented!("Majorana fermions are not supported yet!"),
        };
        Wavefunction::Vector {
            definitions: self.definitions.clone(),
            components: Box::new(wf.components.clone()),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
struct SpinOneStored {
    #[serde(deserialize_with = "deserialize_math_map")]
    definitions: HashMap<String, MathExprPlain>,
    #[serde(rename = "in")]
    incoming: HashMap<Polarization, WavefunctionStored>,
}
impl SpinOneStored {
    fn get(&self, _direction: ParticleDirection, polarization: Polarization) -> Wavefunction {
        let wf = self.incoming[&polarization].clone();
        Wavefunction::Vector {
            definitions: self.definitions.clone(),
            components: Box::new(wf.components),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(transparent)]
struct WavefunctionStored {
    #[serde(deserialize_with = "deserialize_math_array_four")]
    components: [MathExprPlain; 4],
}

#[cfg(test)]
mod test {
    #[test]
    fn read() {
        super::Wavefunctions::load("models/common/wavefunctions.toml").unwrap();
    }
}
