pub mod colorizer;

use crate::color::flow::ColorMultiLine;
use crate::skeleton::uncolored::Id;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Skeleton {
    pub first_level: HashMap<ColorId, External>,
    pub levels: Vec<Level>,
    pub last_level: Vec<BoneFragment>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Level {
    pub bones: HashMap<ColorId, Bone>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct External {
    pub particle: usize,
    pub flipped: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Bone {
    pub fragments: Vec<BoneFragment>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoneFragment {
    pub constituents: Vec<ColorId>,
    pub structures: Vec<BoneStructure>,
}

#[derive(Clone, Debug, PartialEq)]
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
