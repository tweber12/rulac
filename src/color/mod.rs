use crate::math_expr::parse;
use crate::math_expr::{MathExpr, NoComponents, Tensor, TensorIndex};
use serde::{Deserialize, Serialize};

pub type ColorExpr = MathExpr<ColorTensor>;

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
impl Tensor for ColorTensor {
    type Indices = ColorIndex;
    type ExternalComponent = NoComponents;
    fn parse(
        name: &str,
        indices: &mut parse::IndexParser<ColorIndex>,
    ) -> Result<Option<ColorTensor>, parse::ConversionError> {
        let color = match name {
            "f" => ColorTensor::StructureConstant {
                a1: indices.next_index()?,
                a2: indices.next_index()?,
                a3: indices.next_index()?,
            },
            "d" => ColorTensor::SymmetricTensor {
                a1: indices.next_index()?,
                a2: indices.next_index()?,
                a3: indices.next_index()?,
            },
            "T" => ColorTensor::FundamentalRep {
                a1: indices.next_index()?,
                i2: indices.next_index()?,
                jb3: indices.next_index()?,
            },
            "Epsilon" => ColorTensor::Epsilon {
                i1: indices.next_index()?,
                i2: indices.next_index()?,
                i3: indices.next_index()?,
            },
            "EpsilonBar" => ColorTensor::EpsilonBar {
                ib1: indices.next_index()?,
                ib2: indices.next_index()?,
                ib3: indices.next_index()?,
            },
            "T6" => ColorTensor::SextetRep {
                a1: indices.next_index()?,
                alpha2: indices.next_index()?,
                betab3: indices.next_index()?,
            },
            "K6" => ColorTensor::SextetClebschGordan {
                alpha1: indices.next_index()?,
                ib2: indices.next_index()?,
                jb3: indices.next_index()?,
            },
            "K6Bar" => ColorTensor::AntiSextetClebschGordan {
                alphab1: indices.next_index()?,
                i2: indices.next_index()?,
                j3: indices.next_index()?,
            },
            "Identity" => ColorTensor::KroneckerDelta {
                i1: indices.next_index()?,
                jb2: indices.next_index()?,
            },
            _ => return Ok(None),
        };
        Ok(Some(color))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FundamentalIndex(i64);
impl From<i64> for FundamentalIndex {
    fn from(index: i64) -> FundamentalIndex {
        FundamentalIndex(index)
    }
}
impl TensorIndex for FundamentalIndex {
    fn number_of_values() -> u8 {
        4
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AdjointIndex(i64);
impl From<i64> for AdjointIndex {
    fn from(index: i64) -> AdjointIndex {
        AdjointIndex(index)
    }
}
impl TensorIndex for AdjointIndex {
    fn number_of_values() -> u8 {
        8
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SextetIndex(i64);
impl From<i64> for SextetIndex {
    fn from(index: i64) -> SextetIndex {
        SextetIndex(index)
    }
}
impl TensorIndex for SextetIndex {
    fn number_of_values() -> u8 {
        6
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ColorIndex {
    Fundamental { index: FundamentalIndex },
    Adjoint { index: AdjointIndex },
    Sextet { index: SextetIndex },
}
impl From<FundamentalIndex> for ColorIndex {
    fn from(index: FundamentalIndex) -> ColorIndex {
        ColorIndex::Fundamental { index }
    }
}
impl From<SextetIndex> for ColorIndex {
    fn from(index: SextetIndex) -> ColorIndex {
        ColorIndex::Sextet { index }
    }
}
impl From<AdjointIndex> for ColorIndex {
    fn from(index: AdjointIndex) -> ColorIndex {
        ColorIndex::Adjoint { index }
    }
}
