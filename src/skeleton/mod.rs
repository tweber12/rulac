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
    pub fn get_skeleton(&self, color_flow: &ColorFlow) -> Option<Skeleton> {
        self.colorizer.generate(color_flow)
    }
}

#[cfg(test)]
mod test {
    use crate::color::flow::ColorFlows;
    use crate::ufo::{PdgCode, UfoModel};

    fn generate_all(incoming: &[i64], outgoing: &[i64], model: &UfoModel) {
        let incoming: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let outgoing: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let builder = super::Builder::new(&incoming, &outgoing, model).unwrap();
        let flows = ColorFlows::new(&incoming, &outgoing, model);
        for flow in flows.iter() {
            builder.get_skeleton(flow).is_some();
        }
    }

    fn generate_one(
        incoming: &[i64],
        outgoing: &[i64],
        model: &UfoModel,
        flow: &ColorFlow,
        exists: bool,
    ) {
        let incoming: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let outgoing: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let builder = super::Builder::new(&incoming, &outgoing, model).unwrap();
        let result = builder.get_skeleton(flow);
        if exists {
            assert!(result.is_some())
        } else {
            assert!(result.is_none())
        }
    }

    #[test]
    fn test_epem_epem_generate() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        generate_all(&[-11, 11], &[-11, 11], &model);
    }

    #[test]
    fn test_gg_bbx_generate() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        generate_all(&[21, 21], &[5, -5], &model);
    }

    #[test]
    fn test_gg_epvemumvmxbbxgg_generate() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        generate_all(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21, 21], &model);
    }

    use crate::color::flow::{AntiColorLine, ColorFlow, ColorLine, ColorMultiLine};

    #[test]
    fn test_gg_bbx_f1() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(1)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(2)),
                ColorMultiLine::Triplet(ColorLine(3)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f2() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(1)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(3)),
                ColorMultiLine::Triplet(ColorLine(2)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f3() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(2)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(1)),
                ColorMultiLine::Triplet(ColorLine(3)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &flow, false);
    }

    #[test]
    fn test_gg_bbx_f4() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(2)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(3)),
                ColorMultiLine::Triplet(ColorLine(1)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f5() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(3)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(1)),
                ColorMultiLine::Triplet(ColorLine(2)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f6() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(3)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(2)),
                ColorMultiLine::Triplet(ColorLine(1)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &flow, true);
    }
}
