mod builder;
mod coupling_orders;
mod vertex_checker;
mod vertex_list;

use crate::math_expr::EvalContext;
use crate::skeleton::uncolored::builder::SkeletonBuilder;
use crate::skeleton::{Skeleton, SkeletonId};
use crate::ufo;
use std::ops::Add;

pub use coupling_orders::CouplingOrderRestriction;

pub type UncoloredSkeleton = Skeleton<Id, String>;

pub fn build_skeleton(
    incoming: &[ufo::PdgCode],
    outgoing: &[ufo::PdgCode],
    model: &ufo::UfoModel,
    context: &EvalContext,
    order: &CouplingOrderRestriction,
) -> Option<UncoloredSkeleton> {
    SkeletonBuilder::build(incoming, outgoing, model, context, &order)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Id {
    pdg_code: ufo::PdgCode,
    internal: InternalId,
    order: Vec<u8>,
}
impl Id {
    pub fn from_external(pdg_code: ufo::PdgCode, n: u8) -> Id {
        Id {
            pdg_code,
            internal: InternalId::from_external(n),
            order: Vec::new(),
        }
    }
}
impl Id {
    pub fn is_independent(&self, other: &Id) -> bool {
        self.internal.is_independent(other.internal)
    }
}
impl SkeletonId for Id {
    fn level(&self) -> usize {
        self.internal.level()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct InternalId(u64);
impl InternalId {
    pub fn from_external(n: u8) -> InternalId {
        if n > 63 {
            panic!("Processes with more than 64 external particles are currently not supported!");
        }
        InternalId(1 << n)
    }
    pub fn is_independent(self, InternalId(other): InternalId) -> bool {
        let InternalId(s) = self;
        (s & other) == 0
    }
    pub fn contains_external(self, n: u8) -> bool {
        InternalId::from_external(n).is_independent(self)
    }
    pub fn level(self) -> usize {
        let InternalId(mut id) = self;
        let mut l = 0;
        while id != 0 {
            if id & 1_u64 != 0 {
                l += 1;
            }
            id >>= 1;
        }
        l
    }
}
impl Add<InternalId> for InternalId {
    type Output = InternalId;
    fn add(self, InternalId(other): InternalId) -> InternalId {
        let InternalId(s) = self;
        // This implementation might bend the rules of what adding means a bit, but `or`ing the
        // two together is the desired behaviour
        #[allow(clippy::suspicious_arithmetic_impl)]
        InternalId(s | other)
    }
}

#[cfg(test)]
mod test {
    use crate::math_expr::EvalContext;
    use crate::skeleton::uncolored::coupling_orders::CouplingOrderRestriction;
    use crate::ufo::param_card::ParamCard;
    use crate::ufo::{PdgCode, UfoModel};
    use std::sync::Once;

    static mut MODEL: Option<UfoModel> = None;
    static mut CONTEXT: Option<EvalContext> = None;
    static mut CONTEXT_RESTRICTED: Option<EvalContext> = None;
    static INIT: Once = Once::new();

    fn initialize() {
        INIT.call_once(|| unsafe {
            let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
            let restriction = ParamCard::read(&"tests/models/sm_mg5/restrict_default.dat").unwrap();
            CONTEXT = Some(EvalContext::from_model(&model).unwrap());
            CONTEXT_RESTRICTED =
                Some(EvalContext::from_restricted_model(&model, &restriction).unwrap());
            MODEL = Some(model);
        });
    }

    fn count_graphs(
        incoming: &[i64],
        outgoing: &[i64],
        model: &UfoModel,
        context: &EvalContext,
        order: &CouplingOrderRestriction,
    ) -> u64 {
        let inc: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let out: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = super::build_skeleton(&inc, &out, model, context, order).unwrap();
        // skeleton.pretty_print();
        skeleton.count_graphs()
    }

    fn verify_ordering(
        incoming: &[i64],
        outgoing: &[i64],
        model: &UfoModel,
        context: &EvalContext,
        order: &CouplingOrderRestriction,
    ) {
        // Verify that particles appear in each Fragment in the same order as in the corresponding
        // vertex
        let inc: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let out: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = super::build_skeleton(&inc, &out, model, context, order).unwrap();
        for level in skeleton.internal.iter() {
            for bone in level.values() {
                for fragment in bone.fragments.iter() {
                    let vertex = &model.vertices[&fragment.content];
                    assert_eq!(
                        vertex.particles[fragment.outgoing],
                        model.anti_pdg_code(bone.pdg_code)
                    );
                    println!(
                        "{:?} -> {:?}; {:?}",
                        vertex.particles, fragment.constituents, fragment.outgoing
                    );
                    for (i, c) in fragment.constituents.iter().enumerate() {
                        let pdg_code = c.pdg_code;
                        if i < fragment.outgoing {
                            assert_eq!(vertex.particles[i], pdg_code);
                        } else {
                            assert_eq!(vertex.particles[i + 1], pdg_code);
                        }
                    }
                }
            }
        }
    }

    mod unrestricted {
        use crate::skeleton::uncolored::coupling_orders::CouplingOrderRestriction;

        fn count_graphs(incoming: &[i64], outgoing: &[i64]) -> u64 {
            unsafe {
                super::initialize();
                super::count_graphs(
                    incoming,
                    outgoing,
                    super::MODEL.as_ref().unwrap(),
                    super::CONTEXT.as_ref().unwrap(),
                    &CouplingOrderRestriction::all_orders(),
                )
            }
        }

        fn verify_ordering(incoming: &[i64], outgoing: &[i64]) {
            unsafe {
                super::initialize();
                super::verify_ordering(
                    incoming,
                    outgoing,
                    super::MODEL.as_ref().unwrap(),
                    super::CONTEXT.as_ref().unwrap(),
                    &CouplingOrderRestriction::all_orders(),
                );
            }
        }

        // The expected number of graphs was computed using MG5_aMC@NLO version 2.8.2
        // using the 'sm-full' model and including all QCD and QED contributions.

        #[test]
        fn test_emem_emem() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11],), 6);
        }

        #[test]
        fn test_ema_ema() {
            assert_eq!(count_graphs(&[11, 22], &[11, 22],), 2);
        }

        #[test]
        fn test_ema_emaa() {
            assert_eq!(count_graphs(&[11, 22], &[11, 22, 22],), 6);
        }

        #[test]
        fn test_emem_emema() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11, 22],), 24);
        }

        #[test]
        fn test_emem_ememepem() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11, -11, 11],), 348);
        }

        #[test]
        fn test_epem_epemepem() {
            assert_eq!(count_graphs(&[-11, 11], &[-11, 11, -11, 11],), 348);
        }

        #[test]
        fn test_epem_epemepemepem() {
            assert_eq!(
                count_graphs(&[-11, 11], &[-11, 11, -11, 11, -11, 11],),
                54312
            );
        }

        #[test]
        fn test_uux_uux() {
            assert_eq!(count_graphs(&[2, -2], &[2, -2],), 6);
        }

        #[test]
        fn test_uux_gg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21],), 3);
        }

        #[test]
        fn test_uux_ggg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21, 21],), 16);
        }

        #[test]
        fn test_uux_gggg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21, 21, 21],), 123);
        }

        #[test]
        fn test_uux_ddxgg() {
            assert_eq!(count_graphs(&[2, -2], &[1, -1, 21, 21],), 108);
        }

        #[test]
        fn test_gg_gg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21],), 4);
        }

        #[test]
        fn test_gg_ggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21],), 25);
        }

        #[test]
        fn test_gg_gggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21, 21],), 220);
        }

        #[test]
        fn test_gg_gggggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21, 21, 21, 21],), 34300);
        }

        #[test]
        fn test_gg_epvemumvmxbbx() {
            assert_eq!(count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5],), 133);
        }

        #[test]
        fn test_gg_epvemumvmxbbxg() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21],),
                874
            );
        }

        #[test]
        fn test_gg_bbxa() {
            assert_eq!(count_graphs(&[21, 21], &[5, -5, 22],), 8);
        }

        #[test]
        fn test_epem_epvemumvmx() {
            assert_eq!(count_graphs(&[11, -11], &[-11, 12, 13, -14],), 24);
        }

        #[test]
        fn test_uux_wpwm() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24],), 5);
        }

        #[test]
        fn test_uux_wpwma() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 22],), 25);
        }

        #[test]
        fn test_uux_epvemumvmx() {
            assert_eq!(count_graphs(&[2, -2], &[-11, 12, 13, -14],), 11);
        }

        #[test]
        fn test_uux_epvemumvmxa() {
            assert_eq!(count_graphs(&[2, -2], &[-11, 12, 13, -14, 22],), 69);
        }

        #[test]
        fn test_uux_bbxa() {
            assert_eq!(count_graphs(&[2, -2], &[5, -5, 22],), 17);
        }

        #[test]
        fn test_uux_wpbtxa() {
            assert_eq!(count_graphs(&[2, -2], &[24, 5, -6, 22],), 92);
        }

        #[test]
        fn test_uux_wpwmbbxa() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 5, -5, 22],), 1909);
        }

        #[test]
        fn test_epem_wpwmza() {
            assert_eq!(count_graphs(&[11, -11], &[24, -24, 23, 22],), 117);
        }

        #[test]
        fn test_aa_wpwma() {
            assert_eq!(count_graphs(&[22, 22], &[24, -24, 22],), 12);
        }

        #[test]
        fn test_az_wpwm() {
            assert_eq!(count_graphs(&[22, 23], &[24, -24],), 3);
        }

        #[test]
        fn test_uux_wpwmza() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 23, 22],), 160);
        }

        #[test]
        fn test_uux_epvemumvmxbbxa() {
            assert_eq!(
                count_graphs(&[2, -2], &[-11, 12, 13, -14, 5, -5, 22],),
                5071
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxa() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 22],),
                1078
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxvtvtx() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 16, -16],),
                1746
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxag() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 22, 21],),
                7636
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxgg() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21, 21],),
                7777
            );
        }

        #[test]
        fn test_gg_ttxhhh() {
            assert_eq!(count_graphs(&[21, 21], &[6, -6, 25, 25, 25],), 266);
        }

        #[test]
        fn test_uux_usxuxs() {
            assert_eq!(count_graphs(&[2, -2], &[2, -3, 3, -2],), 178);
        }

        #[test]
        fn test_ordering_epem_epvemumvmx() {
            verify_ordering(&[11, -11], &[-11, 12, 13, -14])
        }

        #[test]
        fn test_ordering_gg_epvemumvmxbbxgg() {
            verify_ordering(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21, 21]);
        }
    }

    mod restricted_default {
        use crate::skeleton::uncolored::coupling_orders::CouplingOrderRestriction;

        fn count_graphs(incoming: &[i64], outgoing: &[i64]) -> u64 {
            unsafe {
                super::initialize();
                super::count_graphs(
                    incoming,
                    outgoing,
                    super::MODEL.as_ref().unwrap(),
                    super::CONTEXT_RESTRICTED.as_ref().unwrap(),
                    &CouplingOrderRestriction::all_orders(),
                )
            }
        }

        fn verify_ordering(incoming: &[i64], outgoing: &[i64]) {
            unsafe {
                super::initialize();
                super::verify_ordering(
                    incoming,
                    outgoing,
                    super::MODEL.as_ref().unwrap(),
                    super::CONTEXT_RESTRICTED.as_ref().unwrap(),
                    &CouplingOrderRestriction::all_orders(),
                );
            }
        }

        // The expected number of graphs was computed using MG5_aMC@NLO version 2.8.2
        // using the 'sm' model and including all QCD and QED contributions.

        #[test]
        fn test_emem_emem() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11]), 4);
        }

        #[test]
        fn test_ema_ema() {
            assert_eq!(count_graphs(&[11, 22], &[11, 22],), 2);
        }

        #[test]
        fn test_ema_emaa() {
            assert_eq!(count_graphs(&[11, 22], &[11, 22, 22],), 6);
        }

        #[test]
        fn test_emem_emema() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11, 22],), 16);
        }

        #[test]
        fn test_emem_ememepem() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11, -11, 11],), 144);
        }

        #[test]
        fn test_epem_epemepem() {
            assert_eq!(count_graphs(&[-11, 11], &[-11, 11, -11, 11],), 144);
        }

        #[test]
        fn test_epem_epemepemepem() {
            assert_eq!(
                count_graphs(&[-11, 11], &[-11, 11, -11, 11, -11, 11],),
                13896
            );
        }

        #[test]
        fn test_uux_uux() {
            assert_eq!(count_graphs(&[2, -2], &[2, -2],), 6);
        }

        #[test]
        fn test_uux_gg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21],), 3);
        }

        #[test]
        fn test_uux_ggg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21, 21],), 16);
        }

        #[test]
        fn test_uux_gggg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21, 21, 21],), 123);
        }

        #[test]
        fn test_uux_ddxgg() {
            assert_eq!(count_graphs(&[2, -2], &[1, -1, 21, 21],), 108);
        }

        #[test]
        fn test_gg_gg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21],), 4);
        }

        #[test]
        fn test_gg_ggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21],), 25);
        }

        #[test]
        fn test_gg_gggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21, 21],), 220);
        }

        #[test]
        fn test_gg_gggggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21, 21, 21, 21],), 34300);
        }

        #[test]
        fn test_gg_epvemumvmxbbx() {
            assert_eq!(count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5],), 87);
        }

        #[test]
        fn test_gg_epvemumvmxbbxg() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21],),
                558
            );
        }

        #[test]
        fn test_gg_bbxa() {
            assert_eq!(count_graphs(&[21, 21], &[5, -5, 22],), 8);
        }

        #[test]
        fn test_epem_epvemumvmx() {
            assert_eq!(count_graphs(&[11, -11], &[-11, 12, 13, -14],), 18);
        }

        #[test]
        fn test_uux_wpwm() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24],), 3);
        }

        #[test]
        fn test_uux_wpwma() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 22],), 15);
        }

        #[test]
        fn test_uux_epvemumvmx() {
            assert_eq!(count_graphs(&[2, -2], &[-11, 12, 13, -14],), 9);
        }

        #[test]
        fn test_uux_epvemumvmxa() {
            assert_eq!(count_graphs(&[2, -2], &[-11, 12, 13, -14, 22],), 55);
        }

        #[test]
        fn test_uux_bbxa() {
            assert_eq!(count_graphs(&[2, -2], &[5, -5, 22],), 12);
        }

        #[test]
        fn test_uux_wpbtxa() {
            assert_eq!(count_graphs(&[2, -2], &[24, 5, -6, 22],), 57);
        }

        #[test]
        fn test_uux_wpwmbbxa() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 5, -5, 22],), 625);
        }

        #[test]
        fn test_epem_wpwmza() {
            assert_eq!(count_graphs(&[11, -11], &[24, -24, 23, 22],), 90);
        }

        #[test]
        fn test_aa_wpwma() {
            assert_eq!(count_graphs(&[22, 22], &[24, -24, 22],), 12);
        }

        #[test]
        fn test_az_wpwm() {
            assert_eq!(count_graphs(&[22, 23], &[24, -24],), 3);
        }

        #[test]
        fn test_uux_wpwmza() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 23, 22],), 96);
        }

        #[test]
        fn test_uux_epvemumvmxbbxa() {
            assert_eq!(
                count_graphs(&[2, -2], &[-11, 12, 13, -14, 5, -5, 22],),
                2221
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxa() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 22],),
                690
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxvtvtx() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 16, -16],),
                1174
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxag() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 22, 21],),
                4764
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxgg() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21, 21],),
                4875
            );
        }

        #[test]
        fn test_gg_ttxhhh() {
            assert_eq!(count_graphs(&[21, 21], &[6, -6, 25, 25, 25],), 266);
        }

        #[test]
        fn test_uux_usxuxs() {
            assert_eq!(count_graphs(&[2, -2], &[2, -3, 3, -2],), 110);
        }

        #[test]
        fn test_ordering_epem_epvemumvmx() {
            verify_ordering(&[11, -11], &[-11, 12, 13, -14])
        }

        #[test]
        fn test_ordering_gg_epvemumvmxbbxgg() {
            verify_ordering(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21, 21]);
        }
    }

    mod restricted_default_minimum_order {
        use crate::skeleton::uncolored::coupling_orders::CouplingOrderRestriction;

        fn count_graphs(incoming: &[i64], outgoing: &[i64]) -> u64 {
            unsafe {
                super::initialize();
                let model = super::MODEL.as_ref().unwrap();
                super::count_graphs(
                    incoming,
                    outgoing,
                    model,
                    super::CONTEXT_RESTRICTED.as_ref().unwrap(),
                    &CouplingOrderRestriction::minimum_order(model),
                )
            }
        }

        fn verify_ordering(incoming: &[i64], outgoing: &[i64]) {
            unsafe {
                super::initialize();
                let model = super::MODEL.as_ref().unwrap();
                super::verify_ordering(
                    incoming,
                    outgoing,
                    model,
                    super::CONTEXT_RESTRICTED.as_ref().unwrap(),
                    &CouplingOrderRestriction::minimum_order(model),
                );
            }
        }

        // The expected number of graphs was computed using MG5_aMC@NLO version 2.8.2
        // using the 'sm' model in the default setup

        #[test]
        fn test_emem_emem() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11]), 4);
        }

        #[test]
        fn test_ema_ema() {
            assert_eq!(count_graphs(&[11, 22], &[11, 22],), 2);
        }

        #[test]
        fn test_ema_emaa() {
            assert_eq!(count_graphs(&[11, 22], &[11, 22, 22],), 6);
        }

        #[test]
        fn test_emem_emema() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11, 22],), 16);
        }

        #[test]
        fn test_emem_ememepem() {
            assert_eq!(count_graphs(&[11, 11], &[11, 11, -11, 11],), 144);
        }

        #[test]
        fn test_epem_epemepem() {
            assert_eq!(count_graphs(&[-11, 11], &[-11, 11, -11, 11],), 144);
        }

        #[test]
        fn test_epem_epemepemepem() {
            assert_eq!(
                count_graphs(&[-11, 11], &[-11, 11, -11, 11, -11, 11],),
                13896
            );
        }

        #[test]
        fn test_uux_uux() {
            assert_eq!(count_graphs(&[2, -2], &[2, -2],), 2);
        }

        #[test]
        fn test_uux_gg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21],), 3);
        }

        #[test]
        fn test_uux_ggg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21, 21],), 16);
        }

        #[test]
        fn test_uux_gggg() {
            assert_eq!(count_graphs(&[2, -2], &[21, 21, 21, 21],), 123);
        }

        #[test]
        fn test_uux_ddxgg() {
            assert_eq!(count_graphs(&[2, -2], &[1, -1, 21, 21],), 36);
        }

        #[test]
        fn test_gg_gg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21],), 4);
        }

        #[test]
        fn test_gg_ggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21],), 25);
        }

        #[test]
        fn test_gg_gggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21, 21],), 220);
        }

        #[test]
        fn test_gg_gggggg() {
            assert_eq!(count_graphs(&[21, 21], &[21, 21, 21, 21, 21, 21],), 34300);
        }

        #[test]
        fn test_gg_epvemumvmxbbx() {
            assert_eq!(count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5],), 87);
        }

        #[test]
        fn test_gg_epvemumvmxbbxg() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21],),
                558
            );
        }

        #[test]
        fn test_gg_bbxa() {
            assert_eq!(count_graphs(&[21, 21], &[5, -5, 22],), 8);
        }

        #[test]
        fn test_epem_epvemumvmx() {
            assert_eq!(count_graphs(&[11, -11], &[-11, 12, 13, -14],), 18);
        }

        #[test]
        fn test_uux_wpwm() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24],), 3);
        }

        #[test]
        fn test_uux_wpwma() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 22],), 15);
        }

        #[test]
        fn test_uux_epvemumvmx() {
            assert_eq!(count_graphs(&[2, -2], &[-11, 12, 13, -14],), 9);
        }

        #[test]
        fn test_uux_epvemumvmxa() {
            assert_eq!(count_graphs(&[2, -2], &[-11, 12, 13, -14, 22],), 55);
        }

        #[test]
        fn test_uux_bbxa() {
            assert_eq!(count_graphs(&[2, -2], &[5, -5, 22],), 4);
        }

        #[test]
        fn test_uux_wpbtxa() {
            assert_eq!(count_graphs(&[2, -2], &[24, 5, -6, 22],), 12);
        }

        #[test]
        fn test_uux_wpwmbbxa() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 5, -5, 22],), 126);
        }

        #[test]
        fn test_epem_wpwmza() {
            assert_eq!(count_graphs(&[11, -11], &[24, -24, 23, 22],), 90);
        }

        #[test]
        fn test_aa_wpwma() {
            assert_eq!(count_graphs(&[22, 22], &[24, -24, 22],), 12);
        }

        #[test]
        fn test_az_wpwm() {
            assert_eq!(count_graphs(&[22, 23], &[24, -24],), 3);
        }

        #[test]
        fn test_uux_wpwmza() {
            assert_eq!(count_graphs(&[2, -2], &[24, -24, 23, 22],), 96);
        }

        #[test]
        fn test_uux_epvemumvmxbbxa() {
            assert_eq!(count_graphs(&[2, -2], &[-11, 12, 13, -14, 5, -5, 22],), 366);
        }

        #[test]
        fn test_gg_epvemumvmxbbxa() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 22],),
                690
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxvtvtx() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 16, -16],),
                1174
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxag() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 22, 21],),
                4764
            );
        }

        #[test]
        fn test_gg_epvemumvmxbbxgg() {
            assert_eq!(
                count_graphs(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21, 21],),
                4875
            );
        }

        #[test]
        fn test_gg_ttxhhh() {
            assert_eq!(count_graphs(&[21, 21], &[6, -6, 25, 25, 25],), 266);
        }

        #[test]
        fn test_uux_usxuxs() {
            assert_eq!(count_graphs(&[2, -2], &[2, -3, 3, -2],), 14);
        }

        #[test]
        fn test_ordering_epem_epvemumvmx() {
            verify_ordering(&[11, -11], &[-11, 12, 13, -14])
        }

        #[test]
        fn test_ordering_gg_epvemumvmxbbxgg() {
            verify_ordering(&[21, 21], &[-11, 12, 13, -14, 5, -5, 21, 21]);
        }
    }

    mod restricted_default_fixed {
        use crate::skeleton::uncolored::coupling_orders::{CouplingOrderRestriction, OrderCount};
        use std::collections::HashMap;

        fn count_graphs(
            incoming: &[i64],
            outgoing: &[i64],
            qcd: OrderCount,
            qed: OrderCount,
        ) -> u64 {
            let mut orders = HashMap::new();
            orders.insert("QCD".to_string(), qcd);
            orders.insert("QED".to_string(), qed);
            unsafe {
                super::initialize();
                let model = super::MODEL.as_ref().unwrap();
                super::count_graphs(
                    incoming,
                    outgoing,
                    model,
                    super::CONTEXT_RESTRICTED.as_ref().unwrap(),
                    &CouplingOrderRestriction::fixed_order(orders),
                )
            }
        }

        // The expected number of graphs was computed using MG5_aMC@NLO version 2.8.2
        // using the 'sm' model in the default setup

        #[test]
        fn test_uux_uux_l20_l20() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2],
                    OrderCount::LessThanEqual(20),
                    OrderCount::LessThanEqual(20)
                ),
                6
            );
        }

        #[test]
        fn test_uux_uux_e0_l20() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2],
                    OrderCount::Exact(0),
                    OrderCount::LessThanEqual(20)
                ),
                4
            );
        }

        #[test]
        fn test_uux_uux_e0_e2() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2],
                    OrderCount::Exact(0),
                    OrderCount::Exact(2)
                ),
                4
            );
        }

        #[test]
        fn test_uux_uux_e2_e0() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2],
                    OrderCount::Exact(2),
                    OrderCount::Exact(0)
                ),
                2
            );
        }

        #[test]
        fn test_uux_uux_e2_l1() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2],
                    OrderCount::Exact(2),
                    OrderCount::LessThanEqual(1)
                ),
                2
            );
        }

        #[test]
        fn test_uux_uux_e2_l2() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2],
                    OrderCount::Exact(2),
                    OrderCount::LessThanEqual(2)
                ),
                2
            );
        }

        #[test]
        fn test_uux_uuxuux_l20_l20() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2, 2, -2],
                    OrderCount::LessThanEqual(20),
                    OrderCount::LessThanEqual(20)
                ),
                330
            );
        }

        #[test]
        fn test_uux_uuxuux_e0_l20() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2, 2, -2],
                    OrderCount::Exact(0),
                    OrderCount::LessThanEqual(20)
                ),
                144
            );
        }

        #[test]
        fn test_uux_uuxuux_l20_e0() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2, 2, -2],
                    OrderCount::LessThanEqual(20),
                    OrderCount::Exact(0)
                ),
                42
            );
        }

        #[test]
        fn test_uux_uuxuux_e0_e4() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2, 2, -2],
                    OrderCount::Exact(0),
                    OrderCount::Exact(4)
                ),
                144
            );
        }

        #[test]
        fn test_uux_uuxuux_e2_e2() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2, 2, -2],
                    OrderCount::Exact(2),
                    OrderCount::Exact(2)
                ),
                144
            );
        }

        #[test]
        fn test_uux_uuxuux_e4_e0() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2, 2, -2],
                    OrderCount::Exact(4),
                    OrderCount::Exact(0)
                ),
                42
            );
        }

        #[test]
        fn test_uux_uuxuux_l4_l2() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[2, -2, 2, -2],
                    OrderCount::LessThanEqual(4),
                    OrderCount::LessThanEqual(2)
                ),
                186
            );
        }

        #[test]
        fn test_uux_epvemumvmxbbxa_e2_e5() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[-11, 12, 13, -14, 5, -5, 22],
                    OrderCount::Exact(2),
                    OrderCount::Exact(5)
                ),
                366
            );
        }

        #[test]
        fn test_uux_epvemumvmxbbxa_e0_e7() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[-11, 12, 13, -14, 5, -5, 22],
                    OrderCount::Exact(0),
                    OrderCount::Exact(7)
                ),
                1855
            );
        }

        #[test]
        fn test_uux_epvemumvmxbbxg_e3_e4() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[-11, 12, 13, -14, 5, -5, 21],
                    OrderCount::Exact(3),
                    OrderCount::Exact(4)
                ),
                246
            );
        }

        #[test]
        fn test_uux_epvemumvmxbbxg_e1_e6() {
            assert_eq!(
                count_graphs(
                    &[2, -2],
                    &[-11, 12, 13, -14, 5, -5, 21],
                    OrderCount::Exact(1),
                    OrderCount::Exact(6)
                ),
                926
            );
        }
    }
}
