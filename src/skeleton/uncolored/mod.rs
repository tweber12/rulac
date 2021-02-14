mod builder;
mod vertex_list;

use crate::ufo;
use builder::SkeletonBuilder;
use std::collections::HashMap;
use std::fmt;
use std::ops::Add;

#[derive(Clone, Debug)]
pub struct UncoloredSkeleton {
    pub levels: Vec<Level>,
    pub last_level: Vec<BoneFragment>,
    pub last_level_index: usize,
}
impl UncoloredSkeleton {
    pub fn new(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &ufo::UfoModel,
    ) -> Option<UncoloredSkeleton> {
        SkeletonBuilder::build(incoming, outgoing, model)
    }
    pub fn count_graphs(&self) -> usize {
        self.last_level.iter().map(|b| b.count_graphs(&self)).sum()
    }
    fn get_bone(&self, id: Id) -> &Bone {
        &self.levels[id.level() - 1].level[&id]
    }
    pub fn pretty_print(&self) {
        for (i, l) in self.levels.iter().enumerate() {
            println!("Level {}:", i + 1);
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
pub enum Bone {
    External { particle: usize, flipped: bool },
    Internal { fragments: Vec<BoneFragment> },
}
impl Bone {
    fn count_graphs(&self, skeleton: &UncoloredSkeleton) -> usize {
        match self {
            Bone::External { .. } => 1,
            Bone::Internal { fragments } => {
                fragments.iter().map(|bf| bf.count_graphs(skeleton)).sum()
            }
        }
    }
    fn pretty_print(&self) {
        match self {
            Bone::External { particle, .. } => print!("External({})", particle),
            Bone::Internal { fragments } => {
                for f in fragments {
                    print!("{} ", f);
                }
            }
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
                let n: usize = skeleton.get_bone(id).count_graphs(skeleton);
                n
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
    use crate::ufo::{PdgCode, UfoModel};
    use std::sync::Once;

    static mut MODEL: Option<UfoModel> = None;
    static INIT: Once = Once::new();

    fn initialize() {
        INIT.call_once(|| unsafe {
            MODEL = Some(UfoModel::load("tests/models_json/sm_mg5").unwrap());
        });
    }

    fn count_graphs(incoming: &[i64], outgoing: &[i64], model: &UfoModel) -> usize {
        let inc: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let out: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = super::UncoloredSkeleton::new(&inc, &out, model).unwrap();
        // skeleton.pretty_print();
        skeleton.count_graphs()
    }

    fn verify_ordering(incoming: &[i64], outgoing: &[i64], model: &UfoModel) {
        let inc: Vec<_> = incoming.iter().map(|i| PdgCode(*i)).collect();
        let out: Vec<_> = outgoing.iter().map(|i| PdgCode(*i)).collect();
        let skeleton = super::UncoloredSkeleton::new(&inc, &out, model).unwrap();
        for level in skeleton.levels.iter() {
            for (id, bone) in level.level.iter() {
                let fragments = match bone {
                    super::Bone::External { .. } => continue,
                    super::Bone::Internal { fragments } => fragments,
                };
                for fragment in fragments.iter() {
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

    // The expected number of graphs was computed using MG5_aMC@NLO version 2.8.2
    // using the 'sm-full' model and including all QCD and QED contributions.

    #[test]
    fn test_emem_emem() {
        initialize();
        let n = unsafe { count_graphs(&[11, 11], &[11, 11], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 6);
    }

    #[test]
    fn test_ema_ema() {
        initialize();
        let n = unsafe { count_graphs(&[11, 22], &[11, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 2);
    }

    #[test]
    fn test_ema_emaa() {
        initialize();
        let n = unsafe { count_graphs(&[11, 22], &[11, 22, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 6);
    }

    #[test]
    fn test_emem_emema() {
        initialize();
        let n = unsafe { count_graphs(&[11, 11], &[11, 11, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 24);
    }

    #[test]
    fn test_emem_ememepem() {
        initialize();
        let n = unsafe { count_graphs(&[11, 11], &[11, 11, -11, 11], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 348);
    }

    #[test]
    fn test_epem_epemepem() {
        initialize();
        let n = unsafe { count_graphs(&[-11, 11], &[-11, 11, -11, 11], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 348);
    }

    #[test]
    fn test_epem_epemepemepem() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[-11, 11],
                &[-11, 11, -11, 11, -11, 11],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 54312);
    }

    #[test]
    fn test_uux_uux() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[2, -2], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 6);
    }

    #[test]
    fn test_uux_gg() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[21, 21], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 3);
    }

    #[test]
    fn test_uux_ggg() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[21, 21, 21], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 16);
    }

    #[test]
    fn test_uux_gggg() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[21, 21, 21, 21], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 123);
    }

    #[test]
    fn test_uux_ddxgg() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[1, -1, 21, 21], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 108);
    }

    #[test]
    fn test_gg_gg() {
        initialize();
        let n = unsafe { count_graphs(&[21, 21], &[21, 21], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 4);
    }

    #[test]
    fn test_gg_ggg() {
        initialize();
        let n = unsafe { count_graphs(&[21, 21], &[21, 21, 21], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 25);
    }

    #[test]
    fn test_gg_gggg() {
        initialize();
        let n = unsafe { count_graphs(&[21, 21], &[21, 21, 21, 21], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 220);
    }

    #[test]
    fn test_gg_gggggg() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[21, 21],
                &[21, 21, 21, 21, 21, 21],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 34300);
    }

    #[test]
    fn test_gg_epvemumvmxbbx() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[21, 21],
                &[-11, 12, 13, -14, 5, -5],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 133);
    }

    #[test]
    fn test_gg_epvemumvmxbbxg() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[21, 21],
                &[-11, 12, 13, -14, 5, -5, 21],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 874);
    }

    #[test]
    fn test_gg_bbxa() {
        initialize();
        let n = unsafe { count_graphs(&[21, 21], &[5, -5, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 8);
    }

    #[test]
    fn test_epem_epvemumvmx() {
        initialize();
        let n = unsafe { count_graphs(&[11, -11], &[-11, 12, 13, -14], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 24);
    }

    #[test]
    fn test_uux_wpwm() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[24, -24], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 5);
    }

    #[test]
    fn test_uux_wpwma() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[24, -24, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 25);
    }

    #[test]
    fn test_uux_epvemumvmx() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[-11, 12, 13, -14], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 11);
    }

    #[test]
    fn test_uux_epvemumvmxa() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[-11, 12, 13, -14, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 69);
    }

    #[test]
    fn test_uux_bbxa() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[5, -5, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 17);
    }

    #[test]
    fn test_uux_wpbtxa() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[24, 5, -6, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 92);
    }

    #[test]
    fn test_uux_wpwmbbxa() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[24, -24, 5, -5, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 1909);
    }

    #[test]
    fn test_epem_wpwmza() {
        initialize();
        let n = unsafe { count_graphs(&[11, -11], &[24, -24, 23, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 117);
    }

    #[test]
    fn test_aa_wpwma() {
        initialize();
        let n = unsafe { count_graphs(&[22, 22], &[24, -24, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 12);
    }

    #[test]
    fn test_az_wpwm() {
        initialize();
        let n = unsafe { count_graphs(&[22, 23], &[24, -24], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 3);
    }

    #[test]
    fn test_uux_wpwmza() {
        initialize();
        let n = unsafe { count_graphs(&[2, -2], &[24, -24, 23, 22], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 160);
    }

    #[test]
    fn test_uux_epvemumvmxbbxa() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[2, -2],
                &[-11, 12, 13, -14, 5, -5, 22],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 5071);
    }

    #[test]
    fn test_gg_epvemumvmxbbxa() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[21, 21],
                &[-11, 12, 13, -14, 5, -5, 22],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 1078);
    }

    #[test]
    fn test_gg_epvemumvmxbbxvtvtx() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[21, 21],
                &[-11, 12, 13, -14, 5, -5, 16, -16],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 1746);
    }

    #[test]
    fn test_gg_epvemumvmxbbxag() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[21, 21],
                &[-11, 12, 13, -14, 5, -5, 22, 21],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 7636);
    }

    #[test]
    fn test_gg_epvemumvmxbbxgg() {
        initialize();
        let n = unsafe {
            count_graphs(
                &[21, 21],
                &[-11, 12, 13, -14, 5, -5, 21, 21],
                MODEL.as_ref().unwrap(),
            )
        };
        assert_eq!(n, 7777);
    }

    #[test]
    fn test_gg_ttxhhh() {
        initialize();
        let n = unsafe { count_graphs(&[21, 21], &[6, -6, 25, 25, 25], MODEL.as_ref().unwrap()) };
        assert_eq!(n, 266);
    }

    #[test]

    fn test_ordering_epem_epvemumvmx() {
        initialize();
        unsafe { verify_ordering(&[11, -11], &[-11, 12, 13, -14], MODEL.as_ref().unwrap()) };
    }

    #[test]
    fn test_ordering_gg_epvemumvmxbbxgg() {
        initialize();
        unsafe {
            verify_ordering(
                &[21, 21],
                &[-11, 12, 13, -14, 5, -5, 21, 21],
                MODEL.as_ref().unwrap(),
            );
        };
    }
}
