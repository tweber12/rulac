pub mod propagators;
mod tensor_components;
mod vertex_structures;
pub mod wavefunctions;

pub use tensor_components::{SpinComponentsError, SpinTensorComponents};
pub use vertex_structures::{StructureBuilder, VertexStructure};

use crate::math_expr::parse;
use crate::math_expr::{MathExpr, Tensor, TensorIndex};
use crate::util::Combinations;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

const N_COMPONENTS: usize = 4;

pub type LorentzExpr = MathExpr<LorentzTensor>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LorentzIndex(i64);
impl From<i64> for LorentzIndex {
    fn from(index: i64) -> LorentzIndex {
        LorentzIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SpinorIndex(i64);
impl From<i64> for SpinorIndex {
    fn from(index: i64) -> SpinorIndex {
        SpinorIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SpinIndex {
    Lorentz { index: LorentzIndex },
    Spinor { index: SpinorIndex },
}
impl TensorIndex for SpinIndex {}
impl From<LorentzIndex> for SpinIndex {
    fn from(index: LorentzIndex) -> SpinIndex {
        SpinIndex::Lorentz { index }
    }
}
impl From<SpinorIndex> for SpinIndex {
    fn from(index: SpinorIndex) -> SpinIndex {
        SpinIndex::Spinor { index }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "tensor")]
pub enum LorentzTensor {
    #[serde(rename = "C")]
    ChargeConjugation {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    Epsilon {
        mu1: LorentzIndex,
        mu2: LorentzIndex,
        mu3: LorentzIndex,
        mu4: LorentzIndex,
    },
    Gamma {
        mu1: LorentzIndex,
        i2: SpinorIndex,
        i3: SpinorIndex,
    },
    Gamma5 {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    #[serde(rename = "Identity")]
    KroneckerDelta {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    Metric {
        mu1: LorentzIndex,
        mu2: LorentzIndex,
    },
    #[serde(rename = "P")]
    Momentum {
        mu1: LorentzIndex,
        particle: i64,
    },
    ProjM {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    ProjP {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    Sigma {
        mu1: LorentzIndex,
        mu2: LorentzIndex,
        i3: SpinorIndex,
        i4: SpinorIndex,
    },
}
impl LorentzTensor {
    fn get_component(&self, components: &SpinTensorComponents, indices: &Indices) -> LorentzExpr {
        let number = match *self {
            LorentzTensor::ChargeConjugation { i1, i2 } => {
                components.charge_conjugation.get(indices[i1], indices[i2])
            }
            LorentzTensor::Epsilon { mu1, mu2, mu3, mu4 } => {
                components
                    .epsilon
                    .get(indices[mu1], indices[mu2], indices[mu3], indices[mu4])
            }
            LorentzTensor::Gamma { mu1, i2, i3 } => {
                components.gamma.get(indices[mu1], indices[i2], indices[i3])
            }
            LorentzTensor::Gamma5 { i1, i2 } => components.gamma5.get(indices[i1], indices[i2]),
            LorentzTensor::KroneckerDelta { i1, i2 } => {
                components.identity.get(indices[i1], indices[i2])
            }
            LorentzTensor::Metric { mu1, mu2 } => components.metric.get(indices[mu1], indices[mu2]),
            LorentzTensor::ProjM { i1, i2 } => components.proj_m.get(indices[i1], indices[i2]),
            LorentzTensor::ProjP { i1, i2 } => components.proj_p.get(indices[i1], indices[i2]),
            LorentzTensor::Sigma { mu1, mu2, i3, i4 } => {
                components
                    .sigma
                    .get(indices[mu1], indices[mu2], indices[i3], indices[i4])
            }
            LorentzTensor::Momentum { particle, mu1 } => {
                return LorentzExpr::ExternalComponent {
                    component: ExternalComponent::Momentum(particle, indices[mu1]),
                }
            }
        };
        LorentzExpr::Number { value: number }
    }
}
impl Tensor for LorentzTensor {
    type Indices = SpinIndex;
    type ExternalComponent = ExternalComponent;
    fn parse(
        name: &str,
        indices: &mut parse::IndexParser<SpinIndex>,
    ) -> Result<Option<LorentzTensor>, parse::ConversionError> {
        let lorentz = match name {
            "ProjP" => LorentzTensor::ProjP {
                i1: indices.next_index()?,
                i2: indices.next_index()?,
            },
            "ProjM" => LorentzTensor::ProjM {
                i1: indices.next_index()?,
                i2: indices.next_index()?,
            },
            "Gamma" => LorentzTensor::Gamma {
                mu1: indices.next_index()?,
                i2: indices.next_index()?,
                i3: indices.next_index()?,
            },
            "Identity" => LorentzTensor::KroneckerDelta {
                i1: indices.next_index()?,
                i2: indices.next_index()?,
            },
            "Gamma5" => LorentzTensor::Gamma5 {
                i1: indices.next_index()?,
                i2: indices.next_index()?,
            },
            "P" => LorentzTensor::Momentum {
                mu1: indices.next_index()?,
                particle: if indices.n_args() > 1 {
                    indices.next_integer()?
                } else {
                    0
                },
            },
            "Metric" => LorentzTensor::Metric {
                mu1: indices.next_index()?,
                mu2: indices.next_index()?,
            },
            "Epsilon" => LorentzTensor::Epsilon {
                mu1: indices.next_index()?,
                mu2: indices.next_index()?,
                mu3: indices.next_index()?,
                mu4: indices.next_index()?,
            },
            "Sigma" => LorentzTensor::Sigma {
                mu1: indices.next_index()?,
                mu2: indices.next_index()?,
                i3: indices.next_index()?,
                i4: indices.next_index()?,
            },
            _ => return Ok(None),
        };
        Ok(Some(lorentz))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExternalComponent {
    Scalar(usize),
    Vector(usize, u8),
    Tensor(usize, u8, u8),
    Momentum(i64, u8),
}

#[derive(Debug, Clone)]
struct Indices {
    indices: HashMap<SpinIndex, u8>,
}
impl Indices {
    fn new() -> Indices {
        Indices {
            indices: HashMap::new(),
        }
    }
    fn set_index<T: Into<SpinIndex>>(&mut self, index: T, value: usize) {
        self.indices.insert(index.into(), value as u8);
    }
    fn unset_index<T: Into<SpinIndex>>(&mut self, index: T) {
        self.indices.remove(&index.into());
    }
}
impl<T: Into<SpinIndex>> std::ops::Index<T> for Indices {
    type Output = u8;
    fn index(&self, idx: T) -> &u8 {
        self.indices.index(&idx.into())
    }
}

fn expand_sums(
    expr: &LorentzExpr,
    components: &SpinTensorComponents,
    indices: &mut Indices,
) -> LorentzExpr {
    match expr {
        LorentzExpr::Sum { ref expr, index } => {
            let terms = (0..N_COMPONENTS).map(|i| {
                indices.set_index(*index, i);
                expand_sums(expr, components, indices)
            });
            let val: LorentzExpr = terms.sum();
            indices.unset_index(*index);
            val
        }
        LorentzExpr::Tensor { tensor } => tensor.get_component(components, indices),
        _ => expr.apply_on_subexpressions(&mut |e| expand_sums(&e, components, indices)),
    }
}

struct IndexIter {
    indices: Vec<SpinIndex>,
    external: Indices,
    internal: Combinations<Vec<u8>>,
}
impl<'a> IndexIter {
    pub fn new(indices: &[SpinIndex]) -> IndexIter {
        IndexIter::new_internal(indices.to_vec(), Indices::new())
    }
    pub fn new_split(lorentz: &[LorentzIndex], spinor: &[SpinorIndex]) -> IndexIter {
        let mut indices: Vec<SpinIndex> = Vec::new();
        indices.extend(lorentz.iter().map(|&i| SpinIndex::from(i)));
        indices.extend(spinor.iter().map(|&i| SpinIndex::from(i)));
        IndexIter::new_internal(indices, Indices::new())
    }
    pub fn with_external(indices: &[SpinIndex], external: Indices) -> IndexIter {
        IndexIter::new_internal(indices.to_vec(), external)
    }
    pub fn new_internal(indices: Vec<SpinIndex>, external: Indices) -> IndexIter {
        let range: Vec<_> = (0..(N_COMPONENTS as u8)).collect();
        let internal = vec![range; indices.len()];
        IndexIter {
            indices: indices.to_vec(),
            external,
            internal: Combinations::new(internal),
        }
    }
}
impl Iterator for IndexIter {
    type Item = (Indices, Vec<u8>);
    fn next(&mut self) -> Option<(Indices, Vec<u8>)> {
        self.internal.next().map(|values| {
            let mut indices = self.external.clone();
            for (i, v) in self.indices.iter().zip(values.iter()) {
                indices.set_index(*i, *v as usize);
            }
            (indices, values)
        })
    }
}

pub fn deserialize_lorentz_expr<'de, D>(
    deserializer: D,
) -> Result<MathExpr<LorentzTensor>, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let expr = String::deserialize(deserializer)?;
    let math = parse::parse_math(&expr).unwrap();
    Ok(math)
}
