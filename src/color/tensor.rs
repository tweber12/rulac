use crate::math_expr::parse;
use crate::math_expr::{MathExpr, NoComponents, Tensor, TensorIndex};
use serde::{Deserialize, Serialize};

pub type ColorExpr = MathExpr<ColorTensor>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "tensor")]
pub enum ColorTensor {
    #[serde(rename = "Identity")]
    KroneckerDelta {
        i1: UndefinedIndex,
        jb2: UndefinedIndex,
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
    KroneckerTriplet {
        i1: TripletIndex,
        jb2: AntiTripletIndex,
    },
}
impl ColorTensor {
    pub fn new_fundamental(a1: OctetIndex, i2: TripletIndex, jb3: AntiTripletIndex) -> ColorTensor {
        ColorTensor::FundamentalRep { a1, i2, jb3 }
    }
    pub fn new_sextet(
        alpha1: SextetIndex,
        ib2: AntiTripletIndex,
        jb3: AntiTripletIndex,
    ) -> ColorTensor {
        ColorTensor::SextetClebschGordan { alpha1, ib2, jb3 }
    }
    pub fn new_anti_sextet(
        alphab1: AntiSextetIndex,
        i2: TripletIndex,
        j3: TripletIndex,
    ) -> ColorTensor {
        ColorTensor::AntiSextetClebschGordan { alphab1, i2, j3 }
    }
    pub fn new_kronecker_triplet<I, J>(i1: I, jb2: J) -> ColorTensor
    where
        I: Into<TripletIndex>,
        J: Into<AntiTripletIndex>,
    {
        ColorTensor::KroneckerTriplet {
            i1: i1.into(),
            jb2: jb2.into(),
        }
    }
    pub fn has_normalized_index(&self, index: ColorIndex) -> bool {
        let index = index.normalize();
        match self {
            ColorTensor::FundamentalRep { a1, i2, jb3 } => check_indices3(index, *a1, *i2, *jb3),
            ColorTensor::StructureConstant { a1, a2, a3 } => check_indices3(index, *a1, *a2, *a3),
            ColorTensor::SymmetricTensor { a1, a2, a3 } => check_indices3(index, *a1, *a2, *a3),
            ColorTensor::SextetRep { a1, alpha2, betab3 } => {
                check_indices3(index, *a1, *alpha2, *betab3)
            }
            ColorTensor::SextetClebschGordan { alpha1, ib2, jb3 } => {
                check_indices3(index, *alpha1, *ib2, *jb3)
            }
            ColorTensor::AntiSextetClebschGordan { alphab1, i2, j3 } => {
                check_indices3(index, *alphab1, *i2, *j3)
            }
            ColorTensor::Epsilon { i1, i2, i3 } => check_indices3(index, *i1, *i2, *i3),
            ColorTensor::EpsilonBar { ib1, ib2, ib3 } => check_indices3(index, *ib1, *ib2, *ib3),
            ColorTensor::KroneckerDelta { i1, jb2 } => check_indices2(index, *i1, *jb2),
            ColorTensor::KroneckerTriplet { i1, jb2 } => check_indices2(index, *i1, *jb2),
        }
    }
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
pub struct TripletIndex(pub i64);
impl TripletIndex {
    pub fn bar(self) -> AntiTripletIndex {
        let TripletIndex(i) = self;
        AntiTripletIndex(i)
    }
}
impl From<i64> for TripletIndex {
    fn from(index: i64) -> TripletIndex {
        TripletIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AntiTripletIndex(pub i64);
impl AntiTripletIndex {
    pub fn bar(self) -> TripletIndex {
        let AntiTripletIndex(i) = self;
        TripletIndex(i)
    }
}
impl From<i64> for AntiTripletIndex {
    fn from(index: i64) -> AntiTripletIndex {
        AntiTripletIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct OctetIndex(pub i64);
impl From<i64> for OctetIndex {
    fn from(index: i64) -> OctetIndex {
        OctetIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SextetIndex(pub i64);
impl SextetIndex {
    pub fn bar(self) -> AntiSextetIndex {
        let SextetIndex(i) = self;
        AntiSextetIndex(i)
    }
}
impl From<i64> for SextetIndex {
    fn from(index: i64) -> SextetIndex {
        SextetIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AntiSextetIndex(pub i64);
impl AntiSextetIndex {
    pub fn bar(self) -> SextetIndex {
        let AntiSextetIndex(i) = self;
        SextetIndex(i)
    }
}
impl From<i64> for AntiSextetIndex {
    fn from(index: i64) -> AntiSextetIndex {
        AntiSextetIndex(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct UndefinedIndex(pub i64);
impl From<i64> for UndefinedIndex {
    fn from(index: i64) -> UndefinedIndex {
        UndefinedIndex(index)
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
    Undefined { index: UndefinedIndex },
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
impl From<UndefinedIndex> for ColorIndex {
    fn from(index: UndefinedIndex) -> ColorIndex {
        ColorIndex::Undefined { index }
    }
}

fn check_indices2<S, T>(index: ColorIndex, i1: S, i2: T) -> bool
where
    S: Into<ColorIndex>,
    T: Into<ColorIndex>,
{
    index == i1.into().normalize() || index == i2.into().normalize()
}

fn check_indices3<S, T, U>(index: ColorIndex, i1: S, i2: T, i3: U) -> bool
where
    S: Into<ColorIndex>,
    T: Into<ColorIndex>,
    U: Into<ColorIndex>,
{
    index == i1.into().normalize()
        || index == i2.into().normalize()
        || index == i3.into().normalize()
}
