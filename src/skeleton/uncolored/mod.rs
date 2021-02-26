mod builder;
mod vertex_checker;
mod vertex_list;

use crate::math_expr::EvalContext;
use crate::ufo;
use builder::SkeletonBuilder;
use std::collections::HashMap;
use std::fmt;
use std::ops::Add;

#[derive(Clone, Debug)]
pub struct UncoloredSkeleton {
    pub first_level: HashMap<Id, External>,
    pub levels: Vec<Level>,
    pub last_level: Vec<BoneFragment>,
    pub last_level_index: usize,
}
impl UncoloredSkeleton {
    pub fn new(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &ufo::UfoModel,
        context: &EvalContext,
    ) -> Option<UncoloredSkeleton> {
        SkeletonBuilder::build(incoming, outgoing, model, context)
    }
    pub fn count_graphs(&self) -> usize {
        self.last_level.iter().map(|b| b.count_graphs(&self)).sum()
    }
    fn get_bone(&self, id: Id) -> &Bone {
        &self.levels[id.level() - 2].level[&id]
    }
    pub fn pretty_print(&self) {
        println!("First level = ");
        for (i, ex) in self.first_level.iter() {
            println!("{} = External({})", i, ex.particle);
        }
        for (i, l) in self.levels.iter().enumerate() {
            println!("Level {}:", i + 2);
            l.pretty_print();
        }
        println!("Last level =");
        for b in self.last_level.iter() {
            println!("\t{}", b);
        }
    }
}

#[derive(Clone, Debug)]
pub struct Level {
    pub level: HashMap<Id, Bone>,
}
impl Level {
    fn pretty_print(&self) {
        for (i, bs) in self.level.iter() {
            println!("{} =", i);
            bs.pretty_print();
            println!();
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct External {
    pub particle: usize,
    pub flipped: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bone {
    pub fragments: Vec<BoneFragment>,
}
impl Bone {
    fn count_graphs(&self, skeleton: &UncoloredSkeleton) -> usize {
        self.fragments
            .iter()
            .map(|bf| bf.count_graphs(skeleton))
            .sum()
    }
    fn pretty_print(&self) {
        for f in self.fragments.iter() {
            print!("{} ", f);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoneFragment {
    pub vertex: String,
    pub constituents: Vec<Id>,
    pub outgoing: usize,
}
impl BoneFragment {
    fn count_graphs(&self, skeleton: &UncoloredSkeleton) -> usize {
        self.constituents
            .iter()
            .map(|&id| {
                if id.level() != 1 {
                    skeleton.get_bone(id).count_graphs(skeleton)
                } else {
                    1
                }
            })
            .product()
    }
}
impl fmt::Display for BoneFragment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} [", self.vertex)?;
        for c in self.constituents.iter() {
            write!(f, "({})", c)?;
        }
        write!(f, "]")
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Id {
    pdg_code: ufo::PdgCode,
    internal: InternalId,
}
impl Id {
    pub fn level(self) -> usize {
        self.internal.level()
    }
    pub fn from_external(pdg_code: ufo::PdgCode, n: u8) -> Id {
        Id {
            pdg_code,
            internal: InternalId::from_external(n),
        }
    }
    pub fn is_independent(self, other: Id) -> bool {
        self.internal.is_independent(other.internal)
    }
}
impl std::cmp::PartialOrd for Id {
    fn partial_cmp(&self, other: &Id) -> Option<std::cmp::Ordering> {
        self.internal.partial_cmp(&other.internal)
    }
}
impl std::cmp::Ord for Id {
    fn cmp(&self, other: &Id) -> std::cmp::Ordering {
        self.internal.cmp(&other.internal)
    }
}
impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let InternalId(i, l) = self.internal;
        write!(f, "{}: {},{}", self.pdg_code, i, l)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct InternalId(u64, usize);
impl InternalId {
    pub fn from_external(n: u8) -> InternalId {
        if n > 63 {
            panic!("Processes with more than 64 external particles are currently not supported!");
        }
        InternalId(1 << n, 1)
    }
    pub fn is_independent(self, InternalId(other, _): InternalId) -> bool {
        let InternalId(s, _) = self;
        (s & other) == 0
    }
    pub fn contains_external(self, n: u8) -> bool {
        InternalId::from_external(n).is_independent(self)
    }
    pub fn level(self) -> usize {
        let InternalId(_, l) = self;
        l
    }
}
impl Add<InternalId> for InternalId {
    type Output = InternalId;
    fn add(self, InternalId(other, lo): InternalId) -> InternalId {
        let InternalId(s, l) = self;
        InternalId(s | other, l + lo)
    }
}

#[cfg(test)]
mod test {
    use crate::math_expr::EvalContext;
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
    ) -> usize {
        let inc: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let out: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = super::UncoloredSkeleton::new(&inc, &out, model, context).unwrap();
        // skeleton.pretty_print();
        skeleton.count_graphs()
    }

    fn verify_ordering(
        incoming: &[i64],
        outgoing: &[i64],
        model: &UfoModel,
        context: &EvalContext,
    ) {
        // Verify that particles appear in each Fragment in the same order as in the corresponding
        // vertex
        let inc: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let out: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = super::UncoloredSkeleton::new(&inc, &out, model, context).unwrap();
        for level in skeleton.levels.iter() {
            for (id, bone) in level.level.iter() {
                for fragment in bone.fragments.iter() {
                    let vertex = &model.vertices[&fragment.vertex];
                    assert_eq!(
                        vertex.particles[fragment.outgoing],
                        model.anti_pdg_code(id.pdg_code)
                    );
                    for (i, c) in fragment.constituents.iter().enumerate() {
                        if i < fragment.outgoing {
                            assert_eq!(vertex.particles[i], c.pdg_code);
                        } else {
                            assert_eq!(vertex.particles[i + 1], c.pdg_code);
                        }
                    }
                }
            }
        }
    }

    mod unrestricted {
        fn count_graphs(incoming: &[i64], outgoing: &[i64]) -> usize {
            unsafe {
                super::initialize();
                super::count_graphs(
                    incoming,
                    outgoing,
                    super::MODEL.as_ref().unwrap(),
                    super::CONTEXT.as_ref().unwrap(),
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
        fn count_graphs(incoming: &[i64], outgoing: &[i64]) -> usize {
            unsafe {
                super::initialize();
                super::count_graphs(
                    incoming,
                    outgoing,
                    super::MODEL.as_ref().unwrap(),
                    super::CONTEXT_RESTRICTED.as_ref().unwrap(),
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
}
