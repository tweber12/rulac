use crate::skeleton::uncolored::vertex_list::{VertexLeaf, VertexList, VertexListParticle};
use crate::skeleton::uncolored::{
    Bone, BoneFragment, External, Id, InternalId, Level, UncoloredSkeleton,
};
use crate::ufo;
use std::collections::{HashMap, HashSet};

pub struct SkeletonBuilder<'a> {
    external: ExternalLevel,
    levels: Vec<InternalLevel>,
    model: &'a ufo::UfoModel,
    vertices: VertexList<'a>,
    last: Id,
}
impl<'a> SkeletonBuilder<'a> {
    pub fn build(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &ufo::UfoModel,
    ) -> Option<UncoloredSkeleton> {
        let mut builder = SkeletonBuilder::new(incoming, outgoing, model);
        let max_level = incoming.len() + outgoing.len() - 1;
        for ilevel in 2..=max_level {
            builder.construct_level(ilevel);
        }
        builder.remove_unused();
        builder.convert_levels()
    }

    fn new(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &'a ufo::UfoModel,
    ) -> SkeletonBuilder<'a> {
        let max_level = incoming.len() + outgoing.len() - 1;
        let last = Id {
            pdg_code: model.anti_pdg_code(incoming[0]),
            internal: InternalId((2 << (max_level - 1)) - 1, max_level),
        };
        SkeletonBuilder {
            external: ExternalLevel::new(&incoming[1..], outgoing, model),
            levels: Vec::new(),
            vertices: VertexList::with_filter(&model.vertices, &|v: &ufo::Vertex| {
                v.particles.iter().all(|p| {
                    let particle = &model.particles[p];
                    !particle.goldstoneboson && particle.ghost_number == 0
                })
            }),
            model,
            last,
        }
    }

    fn construct_level(&mut self, ilevel: usize) {
        let mut builder = InternalLevel::new(ilevel as u8);
        if ilevel == 2 {
            builder.combine(&self.external, &self.external, &self.vertices, &self.model);
        } else {
            builder.combine(
                &self.external,
                &self.levels[ilevel - 3],
                &self.vertices,
                &self.model,
            );
        }
        for i in 2..=(ilevel / 2) {
            builder.combine(
                &self.levels[i - 2],
                &self.levels[ilevel - i - 2],
                &self.vertices,
                &self.model,
            );
        }
        self.levels.push(builder);
    }

    fn remove_unused(&mut self) {
        let mut used = HashSet::new();
        used.insert(self.last);
        let mut iter = self.levels.iter_mut();
        while let Some(level) = iter.next_back() {
            level.remove_unused(&mut used);
        }
    }

    fn convert_levels(mut self) -> Option<UncoloredSkeleton> {
        let mut last_level = self
            .levels
            .pop()
            .expect("BUG: There must be more than one level");
        let last_level = last_level.particles.remove(&self.last)?.fragments;
        let levels = self
            .levels
            .into_iter()
            .map(|level| level.convert())
            .collect();
        Some(UncoloredSkeleton {
            first_level: self.external.convert(),
            levels,
            last_level,
            last_level_index: 0,
        })
    }
}

trait LevelBuilder {
    type Particle;
    fn level_number(&self) -> u8;
    fn particles(&self) -> &HashMap<Id, Self::Particle>;
    fn incomplete(&self) -> &HashMap<ufo::PdgCode, Vec<Incomplete>>;
}

#[derive(Clone, Debug)]
struct ExternalLevel {
    particles: HashMap<Id, External>,
    incomplete: HashMap<ufo::PdgCode, Vec<Incomplete>>,
}
impl ExternalLevel {
    fn new(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &ufo::UfoModel,
    ) -> ExternalLevel {
        let mut particles = HashMap::new();
        for (i, &p) in incoming.iter().enumerate() {
            particles.insert(
                Id::from_external(p, i as u8),
                External {
                    particle: i,
                    flipped: false,
                },
            );
        }
        for (i, &p) in outgoing.iter().enumerate() {
            let anti = model.anti_pdg_code(p);
            let i = i + incoming.len();
            particles.insert(
                Id::from_external(anti, i as u8),
                External {
                    particle: i,
                    flipped: true,
                },
            );
        }
        ExternalLevel {
            particles,
            incomplete: HashMap::new(),
        }
    }

    fn convert(self) -> HashMap<Id, External> {
        self.particles
    }
}
impl LevelBuilder for ExternalLevel {
    type Particle = External;
    fn level_number(&self) -> u8 {
        1
    }
    fn particles(&self) -> &HashMap<Id, External> {
        &self.particles
    }
    fn incomplete(&self) -> &HashMap<ufo::PdgCode, Vec<Incomplete>> {
        &self.incomplete
    }
}

#[derive(Clone, Debug)]
struct InternalLevel {
    ilevel: u8,
    particles: HashMap<Id, Bone>,
    incomplete: HashMap<ufo::PdgCode, Vec<Incomplete>>,
}
impl InternalLevel {
    fn new(ilevel: u8) -> InternalLevel {
        InternalLevel {
            ilevel,
            particles: HashMap::new(),
            incomplete: HashMap::new(),
        }
    }

    fn combine<L, R>(&mut self, left: &L, right: &R, vertices: &VertexList, model: &ufo::UfoModel)
    where
        L: LevelBuilder,
        R: LevelBuilder,
    {
        for &left_id in left.particles().keys() {
            let vl = &vertices.contents[&left_id.pdg_code];
            for &right_id in right.particles().keys() {
                self.add_complete_matches(left_id, right_id, &vl, model);
            }
            if let Some(incomplete) = right.incomplete().get(&left_id.pdg_code) {
                self.add_incomplete_matches(left_id, incomplete, model);
            }
        }
        if left.level_number() != right.level_number() {
            for &right_id in right.particles().keys() {
                if let Some(incomplete) = left.incomplete().get(&right_id.pdg_code) {
                    self.add_incomplete_matches(right_id, incomplete, model);
                }
            }
        }
    }

    fn add_complete_matches(
        &mut self,
        left_id: Id,
        right_id: Id,
        vertices: &VertexListParticle,
        model: &ufo::UfoModel,
    ) {
        if !left_id.is_independent(right_id) {
            return;
        }
        if left_id.level() == right_id.level() && left_id.internal > right_id.internal {
            return;
        }
        let vs = match vertices.contents.get(&right_id.pdg_code) {
            Some(v) => v,
            None => return,
        };
        for v in vs.iter() {
            let combination = specialize_vertex(v, left_id, right_id, model);
            self.insert_combination(combination);
        }
    }

    fn add_incomplete_matches(
        &mut self,
        left_id: Id,
        incomplete: &[Incomplete],
        model: &ufo::UfoModel,
    ) {
        for inc in incomplete {
            let max = *inc
                .constituents
                .last()
                .expect("BUG: The list of constituents of an incomplete vertex cannot be empty!");
            if !left_id.internal.is_independent(inc.internal) || left_id < max {
                continue;
            }
            let combination = inc.add_particle(left_id, model);
            self.insert_combination(combination);
        }
    }

    fn insert_combination(&mut self, combination: Combination) {
        match combination {
            Combination::Complete { id, fragment } => self.insert_fragment(id, fragment),
            Combination::Incomplete { pdg, incomplete } => self.insert_incomplete(pdg, incomplete),
        }
    }

    fn insert_fragment(&mut self, id: Id, fragment: BoneFragment) {
        let bone = self.particles.entry(id).or_insert_with(|| Bone {
            fragments: Vec::new(),
        });
        bone.fragments.push(fragment);
    }

    fn insert_incomplete(&mut self, pdg: ufo::PdgCode, incomplete: Incomplete) {
        self.incomplete.entry(pdg).or_default().push(incomplete);
    }

    fn convert(self) -> Level {
        Level {
            level: self.particles,
        }
    }

    fn remove_unused(&mut self, used: &mut HashSet<Id>) {
        self.particles.retain(|i, v| {
            if !used.contains(i) {
                return false;
            }
            for f in v.fragments.iter() {
                used.extend(f.constituents.iter());
            }
            true
        });
    }
}
impl LevelBuilder for InternalLevel {
    type Particle = Bone;
    fn level_number(&self) -> u8 {
        self.ilevel
    }
    fn particles(&self) -> &HashMap<Id, Bone> {
        &self.particles
    }
    fn incomplete(&self) -> &HashMap<ufo::PdgCode, Vec<Incomplete>> {
        &self.incomplete
    }
}

enum Combination {
    Incomplete {
        pdg: ufo::PdgCode,
        incomplete: Incomplete,
    },
    Complete {
        id: Id,
        fragment: BoneFragment,
    },
}

#[derive(Clone, Debug)]
struct Incomplete {
    internal: InternalId,
    vertex: String,
    constituents: Vec<Id>,
    remaining: Vec<ufo::PdgCode>,
}
impl Incomplete {
    fn add_particle(&self, particle_id: Id, model: &ufo::UfoModel) -> Combination {
        let mut constituents = self.constituents.clone();
        constituents.push(particle_id);
        let internal = particle_id.internal + self.internal;
        if self.remaining.len() == 1 {
            let (constituents, outgoing) = order_constituents(&self.vertex, constituents, model);
            let fragment = BoneFragment {
                vertex: self.vertex.clone(),
                constituents,
                outgoing,
            };
            let id = Id {
                pdg_code: model.anti_pdg_code(self.remaining[0]),
                internal,
            };
            Combination::Complete { id, fragment }
        } else {
            let incomplete = Incomplete {
                internal,
                vertex: self.vertex.clone(),
                constituents,
                remaining: self.remaining[1..].to_owned(),
            };
            Combination::Incomplete {
                pdg: self.remaining[0],
                incomplete,
            }
        }
    }
}

fn specialize_vertex(vertex: &VertexLeaf, p1: Id, p2: Id, model: &ufo::UfoModel) -> Combination {
    let internal = p1.internal + p2.internal;
    let mut constituents = vec![p1, p2];
    match vertex {
        VertexLeaf::Complete(c) => {
            let (constituents, outgoing) = order_constituents(&c.vertex.name, constituents, model);
            let fragment = BoneFragment {
                vertex: c.vertex.name.clone(),
                constituents,
                outgoing,
            };
            let id = Id {
                pdg_code: model.anti_pdg_code(c.out),
                internal,
            };
            Combination::Complete { id, fragment }
        }
        VertexLeaf::Incomplete(c) => {
            constituents.sort();
            let incomplete = Incomplete {
                internal,
                vertex: c.vertex.name.clone(),
                remaining: c.remaining[1..].to_owned(),
                constituents,
            };
            Combination::Incomplete {
                pdg: c.remaining[0],
                incomplete,
            }
        }
    }
}

fn order_constituents(
    vertex: &str,
    constituents: Vec<Id>,
    model: &ufo::UfoModel,
) -> (Vec<Id>, usize) {
    let particles = &model.vertices[vertex].particles;
    order_constituents_internal(particles, constituents)
}
fn order_constituents_internal(
    particles: &[ufo::PdgCode],
    constituents: Vec<Id>,
) -> (Vec<Id>, usize) {
    let mut taken = vec![false; constituents.len()];
    let mut ordered = Vec::with_capacity(constituents.len());
    let mut outgoing = None;
    for (i, p) in particles.iter().enumerate() {
        let mut found = false;
        for (&id, t) in constituents.iter().zip(taken.iter_mut()) {
            if *t || id.pdg_code != *p {
                continue;
            }
            ordered.push(id);
            *t = true;
            found = true;
            break;
        }
        if !found {
            assert_eq!(outgoing, None);
            outgoing = Some(i);
        }
    }
    let outgoing = match outgoing {
        Some(o) => o,
        None => unreachable!("BUG: There has to be an outgoing particle!"),
    };
    (ordered, outgoing)
}

#[cfg(test)]
mod test {
    use crate::skeleton::uncolored::{Id, InternalId};
    use crate::ufo::PdgCode;

    #[test]
    fn order_constituents_v69() {
        let particles = vec![PdgCode(23), PdgCode(23), PdgCode(25)];
        let constituents = vec![
            Id {
                pdg_code: PdgCode(23),
                internal: InternalId(8, 1),
            },
            Id {
                pdg_code: PdgCode(23),
                internal: InternalId(6, 2),
            },
        ];
        let (cs, out) = super::order_constituents_internal(&particles, constituents.clone());
        assert_eq!(out, 2);
        assert_eq!(cs, constituents);
    }

    #[test]
    fn order_constituents_v69_disordered() {
        let particles = vec![PdgCode(23), PdgCode(23), PdgCode(25)];
        let constituents = vec![
            Id {
                pdg_code: PdgCode(25),
                internal: InternalId(8, 1),
            },
            Id {
                pdg_code: PdgCode(23),
                internal: InternalId(6, 2),
            },
        ];
        let (cs, out) = super::order_constituents_internal(&particles, constituents.clone());
        assert_eq!(out, 1);
        assert_eq!(cs, vec![constituents[1], constituents[0]]);
    }
}
