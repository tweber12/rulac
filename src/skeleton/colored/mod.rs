pub mod colorizer;

use crate::color::flow::ColorMultiLine;
use crate::skeleton::uncolored::Id;
use std::collections::HashMap;

pub struct Skeleton {
    pub levels: Vec<Level>,
    pub last_level: Vec<BoneFragment>,
}
impl Skeleton {}

pub struct Level {
    pub bones: HashMap<ColorId, Bone>,
}

pub enum Bone {
    External { particle: usize, flipped: bool },
    Internal { fragments: Vec<BoneFragment> },
}

pub struct BoneFragment {
    pub constituents: Vec<ColorId>,
    pub structures: Vec<BoneStructure>,
}

pub struct BoneStructure {
    pub coupling: String,
    pub factor: f64,
    pub lorentz_structure: String,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ColorId {
    pub internal: Id,
    pub color_flow: ColorMultiLine,
}
impl ColorId {
    fn new(internal: Id, color_flow: ColorMultiLine) -> ColorId {
        ColorId {
            internal,
            color_flow,
        }
    }
}
