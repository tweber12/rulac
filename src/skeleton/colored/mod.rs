mod colorizer;

use crate::color::flow::ColorMultiLine;
use crate::skeleton::uncolored::Id;
use crate::skeleton::{Skeleton, SkeletonId};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ColorId(Id, ColorMultiLine);
impl SkeletonId for ColorId {
    fn level(&self) -> usize {
        let ColorId(id, _) = self;
        id.level()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoneStructure {
    pub coupling: String,
    pub factor: f64,
    pub lorentz_structure: String,
}

pub type ColoredSkeleton = Skeleton<ColorId, Vec<BoneStructure>>;

pub use colorizer::colorize;

#[cfg(test)]
mod test {
    use crate::color::flow::{AntiColorLine, ColorFlow, ColorFlows, ColorLine, ColorMultiLine};
    use crate::math_expr::EvalContext;
    use crate::skeleton::colored::colorize;
    use crate::skeleton::uncolored::{build_skeleton, CouplingOrderRestriction};
    use crate::ufo::{PdgCode, UfoModel};

    fn generate_all(incoming: &[i64], outgoing: &[i64], model: &UfoModel, context: &EvalContext) {
        let incoming: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let outgoing: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = build_skeleton(
            &incoming,
            &outgoing,
            model,
            context,
            &CouplingOrderRestriction::all_orders(),
        )
        .unwrap();
        let flows = ColorFlows::new(&incoming, &outgoing, model);
        for flow in flows.iter() {
            println!("{:?}", colorize(&skeleton, model, flow).is_some());
        }
    }

    fn generate_one(
        incoming: &[i64],
        outgoing: &[i64],
        model: &UfoModel,
        context: &EvalContext,
        flow: &ColorFlow,
        exists: bool,
    ) {
        let incoming: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let outgoing: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = build_skeleton(
            &incoming,
            &outgoing,
            model,
            context,
            &CouplingOrderRestriction::all_orders(),
        )
        .unwrap();
        let result = colorize(&skeleton, model, flow);
        if exists {
            assert!(result.is_some())
        } else {
            assert!(result.is_none())
        }
    }

    #[test]
    fn test_epem_epem_generate() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        generate_all(&[-11, 11], &[-11, 11], &model, &context);
    }

    #[test]
    fn test_gg_bbx_generate() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        generate_all(&[21, 21], &[5, -5], &model, &context);
    }

    #[test]
    fn test_gg_epvemumvmxbbxgg_generate() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        generate_all(
            &[21, 21],
            &[-11, 12, 13, -14, 5, -5, 21, 21],
            &model,
            &context,
        );
    }

    #[test]
    fn test_gg_epvemumvmxbbxggg_generate() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        generate_all(
            &[21, 21],
            &[-11, 12, 13, -14, 5, -5, 21, 21, 21],
            &model,
            &context,
        );
    }

    #[test]
    fn test_gg_bbx_f0() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(1)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(2)),
                ColorMultiLine::Triplet(ColorLine(3)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &context, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f1() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(1)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(3)),
                ColorMultiLine::Triplet(ColorLine(2)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &context, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f2() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(2)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(1)),
                ColorMultiLine::Triplet(ColorLine(3)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &context, &flow, false);
    }

    #[test]
    fn test_gg_bbx_f3() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(2)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(3)),
                ColorMultiLine::Triplet(ColorLine(1)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &context, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f4() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(3)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(1)),
                ColorMultiLine::Triplet(ColorLine(2)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &context, &flow, true);
    }

    #[test]
    fn test_gg_bbx_f5() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let context = EvalContext::from_model(&model).unwrap();
        let flow = ColorFlow {
            components: vec![
                ColorMultiLine::Octet(ColorLine(1), AntiColorLine(3)),
                ColorMultiLine::Octet(ColorLine(2), AntiColorLine(2)),
                ColorMultiLine::Triplet(ColorLine(1)),
                ColorMultiLine::AntiTriplet(AntiColorLine(3)),
            ],
        };
        generate_one(&[21, 21], &[5, -5], &model, &context, &flow, true);
    }
}
