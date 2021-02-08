use crate::math_expr::parse;
use crate::math_expr::{MathExpr, NoComponents, Tensor, TensorIndex};
use serde::{Deserialize, Serialize};

pub type ColorExpr = MathExpr<ColorTensor>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "tensor")]
pub enum ColorTensor {
    #[serde(rename = "Identity")]
    KroneckerDelta {
        i1: TripletIndex,
        jb2: AntiTripletIndex,
    },
    #[serde(rename = "T")]
    FundamentalRep {
        a1: OctetIndex,
        i2: TripletIndex,
        jb3: AntiTripletIndex,
    },
    #[serde(rename = "f")]
    StructureConstant {
        a1: OctetIndex,
        a2: OctetIndex,
        a3: OctetIndex,
    },
    #[serde(rename = "d")]
    SymmetricTensor {
        a1: OctetIndex,
        a2: OctetIndex,
        a3: OctetIndex,
    },
    Epsilon {
        i1: TripletIndex,
        i2: TripletIndex,
        i3: TripletIndex,
    },
    EpsilonBar {
        ib1: AntiTripletIndex,
        ib2: AntiTripletIndex,
        ib3: AntiTripletIndex,
    },
    #[serde(rename = "T6")]
    SextetRep {
        a1: OctetIndex,
        alpha2: SextetIndex,
        betab3: AntiSextetIndex,
    },
    #[serde(rename = "K6")]
    SextetClebschGordan {
        alpha1: SextetIndex,
        ib2: AntiTripletIndex,
        jb3: AntiTripletIndex,
    },
    #[serde(rename = "K6Bar")]
    AntiSextetClebschGordan {
        alphab1: AntiSextetIndex,
        i2: TripletIndex,
        j3: TripletIndex,
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
pub struct TripletIndex(i64);
impl From<i64> for TripletIndex {
    fn from(index: i64) -> TripletIndex {
        TripletIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AntiTripletIndex(i64);
impl From<i64> for AntiTripletIndex {
    fn from(index: i64) -> AntiTripletIndex {
        AntiTripletIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct OctetIndex(i64);
impl From<i64> for OctetIndex {
    fn from(index: i64) -> OctetIndex {
        OctetIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SextetIndex(i64);
impl From<i64> for SextetIndex {
    fn from(index: i64) -> SextetIndex {
        SextetIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AntiSextetIndex(i64);
impl From<i64> for AntiSextetIndex {
    fn from(index: i64) -> AntiSextetIndex {
        AntiSextetIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ColorIndex {
    Triplet { index: TripletIndex },
    AntiTriplet { index: AntiTripletIndex },
    Sextet { index: SextetIndex },
    AntiSextet { index: AntiSextetIndex },
    Octet { index: OctetIndex },
}
impl TensorIndex for ColorIndex {
    fn normalize(self) -> ColorIndex {
        match self {
            ColorIndex::AntiTriplet {
                index: AntiTripletIndex(i),
            } => ColorIndex::Triplet {
                index: TripletIndex(i),
            },
            ColorIndex::AntiSextet {
                index: AntiSextetIndex(i),
            } => ColorIndex::Sextet {
                index: SextetIndex(i),
            },
            _ => self,
        }
    }
}
impl From<TripletIndex> for ColorIndex {
    fn from(index: TripletIndex) -> ColorIndex {
        ColorIndex::Triplet { index }
    }
}
impl From<AntiTripletIndex> for ColorIndex {
    fn from(index: AntiTripletIndex) -> ColorIndex {
        ColorIndex::AntiTriplet { index }
    }
}
impl From<SextetIndex> for ColorIndex {
    fn from(index: SextetIndex) -> ColorIndex {
        ColorIndex::Sextet { index }
    }
}
impl From<AntiSextetIndex> for ColorIndex {
    fn from(index: AntiSextetIndex) -> ColorIndex {
        ColorIndex::AntiSextet { index }
    }
}
impl From<OctetIndex> for ColorIndex {
    fn from(index: OctetIndex) -> ColorIndex {
        ColorIndex::Octet { index }
    }
}
