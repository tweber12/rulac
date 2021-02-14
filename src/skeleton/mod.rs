pub mod colored;
pub mod uncolored;

pub use colored::{Bone, BoneFragment, BoneStructure, ColorId, Level, Skeleton};

use crate::color::flow::ColorFlow;
use crate::ufo::{PdgCode, UfoModel};
use colored::colorizer::Colorizer;
use uncolored::UncoloredSkeleton;

pub struct Builder<'a> {
    colorizer: Colorizer<'a>,
}
impl<'a> Builder<'a> {
    pub fn new(
        incoming: &[PdgCode],
        outgoing: &[PdgCode],
        model: &'a UfoModel,
    ) -> Option<Builder<'a>> {
        UncoloredSkeleton::new(incoming, outgoing, model).map(|s| Builder {
            colorizer: Colorizer::new(s, model),
        })
    }
    pub fn get_skeleton(&self, color_flow: &ColorFlow) -> Skeleton {
        self.colorizer.generate(color_flow)
    }
}
