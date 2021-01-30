mod tensor_components;

pub use tensor_components::{SpinComponentsError, SpinTensorComponents};

use super::{BinaryOperator, IndexRange, MathExpr, Number, SummationIndex};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LorentzIndex(i64);
impl From<i64> for LorentzIndex {
    fn from(index: i64) -> LorentzIndex {
        LorentzIndex(index)
    }
}
impl IndexRange for LorentzIndex {
    fn range() -> std::ops::Range<u8> {
        0..4
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SpinorIndex(i64);
impl From<i64> for SpinorIndex {
    fn from(index: i64) -> SpinorIndex {
        SpinorIndex(index)
    }
}
impl IndexRange for SpinorIndex {
    fn range() -> std::ops::Range<u8> {
        1..5
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
    fn get_component(&self, components: &SpinTensorComponents, indices: &Indices) -> Number {
        match *self {
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
            LorentzTensor::Momentum { .. } => unimplemented! {},
        }
    }
}

#[derive(Debug, Clone)]
struct Indices {
    indices: HashMap<SummationIndex, u8>,
}
impl Indices {
    fn new() -> Indices {
        Indices {
            indices: HashMap::new(),
        }
    }
    fn set_index<T: Into<SummationIndex>>(&mut self, index: T, value: u8) {
        self.indices.insert(index.into(), value);
    }
    fn unset_index<T: Into<SummationIndex>>(&mut self, index: T) {
        self.indices.remove(&index.into());
    }
}
impl<T: Into<SummationIndex>> std::ops::Index<T> for Indices {
    type Output = u8;
    fn index(&self, idx: T) -> &u8 {
        self.indices.index(&idx.into())
    }
}

fn expand_sums(
    expr: &MathExpr,
    components: &SpinTensorComponents,
    indices: &mut Indices,
) -> MathExpr {
    match expr {
        MathExpr::Sum { ref expr, index } => {
            let mut terms = index.range().map(|i| {
                indices.set_index(*index, i);
                expand_sums(expr, components, indices)
            });
            let mut val = terms
                .next()
                .expect("BUG: There has to be at least one value for an index!");
            for t in terms {
                val = MathExpr::BinaryOp {
                    operator: BinaryOperator::Add,
                    left: Box::new(val),
                    right: Box::new(t),
                };
            }
            indices.unset_index(*index);
            val
        }
        MathExpr::LorentzTensor { lorentz } => {
            let value = lorentz.get_component(components, indices);
            MathExpr::Number { value }
        }
        _ => expr.apply_on_subexpressions(&mut |e| expand_sums(&e, components, indices)),
    }
}

struct IndexIter {
    internal: Option<Box<IndexIter>>,
    current: Vec<u8>,
    indices: Indices,
    index: SummationIndex,
    iter: std::ops::Range<u8>,
}
impl IndexIter {
    pub fn new(lorentz: &[LorentzIndex], spinor: &[SpinorIndex]) -> IndexIter {
        let mut indices = lorentz
            .iter()
            .map(|&i| SummationIndex::from(i))
            .chain(spinor.iter().map(|&i| i.into()));
        let mut iter = match indices.next() {
            Some(i) => IndexIter::leaf(i),
            None => return IndexIter::empty(),
        };
        for i in indices {
            iter = IndexIter::node(iter, i);
        }
        iter
    }
    fn empty() -> IndexIter {
        IndexIter {
            internal: None,
            current: Vec::new(),
            indices: Indices::new(),
            index: SummationIndex::from(SpinorIndex(0)),
            iter: 0..0,
        }
    }
    fn leaf(index: SummationIndex) -> IndexIter {
        IndexIter {
            internal: None,
            current: Vec::new(),
            indices: Indices::new(),
            index,
            iter: index.range(),
        }
    }
    fn node(internal: IndexIter, index: SummationIndex) -> IndexIter {
        IndexIter {
            internal: Some(Box::new(internal)),
            current: Vec::new(),
            indices: Indices::new(),
            index,
            iter: 0..0,
        }
    }
}
impl Iterator for IndexIter {
    type Item = (Indices, Vec<u8>);
    fn next(&mut self) -> Option<(Indices, Vec<u8>)> {
        if let Some(i2) = self.iter.next() {
            let mut new = self.current.clone();
            new.push(i2);
            let mut indices = self.indices.clone();
            indices.set_index(self.index, i2);
            return Some((indices, new));
        }
        if let Some((indices, i1)) = self.internal.as_mut().and_then(|i| i.next()) {
            self.current = i1;
            self.indices = indices;
            self.iter = self.index.range();
            return self.next();
        }
        None
    }
}
