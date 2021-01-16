use crate::skeleton::vertex_list::{VertexLeaf, VertexList, VertexListParticle};
use crate::skeleton::{Bone, Id, InternalId, Level, MultiMap, UncoloredSkeleton};
use crate::ufo;

pub struct SkeletonBuilder<'a> {
    left_out: ufo::PdgCode,
    levels: Vec<LevelBuilder>,
    model: &'a ufo::UfoModel,
    vertices: VertexList<'a>,
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
        builder.convert_levels()
    }

    fn new(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &'a ufo::UfoModel,
    ) -> SkeletonBuilder<'a> {
        SkeletonBuilder {
            left_out: incoming[0],
            levels: vec![LevelBuilder::external(&incoming[1..], outgoing, model)],
            vertices: VertexList::with_filter(&model.vertices, &|v: &ufo::Vertex| {
                v.particles.iter().all(|p| {
                    let particle = &model.particles[p];
                    !particle.goldstoneboson && particle.ghost_number == 0
                })
            }),
            model,
        }
    }

    fn construct_level(&mut self, ilevel: usize) {
        let mut builder = LevelBuilder::new(ilevel as u8);
        for i in 1..=(ilevel / 2) {
            builder.combine(
                &self.levels[i - 1],
                &self.levels[ilevel - i - 1],
                &self.vertices,
                &self.model,
            );
        }
        self.levels.push(builder);
    }

    fn convert_levels(mut self) -> Option<UncoloredSkeleton> {
        let max_level = self.levels.len();
        let left_out = Id {
            pdg_code: self.left_out,
            internal: InternalId(0, 1),
        };
        let last = Id {
            pdg_code: self.model.anti_pdg_code(self.left_out),
            internal: InternalId((2 << max_level - 1) - 1, max_level),
        };
        self.levels[0].particles.insert(left_out, Bone::External);
        let last_level = self.levels[max_level - 1]
            .particles
            .contents
            .remove(&last)?;
        let levels = self
            .levels
            .into_iter()
            .map(|level| level.convert())
            .take(max_level - 1)
            .collect();
        Some(UncoloredSkeleton { levels, last_level })
    }
}

struct LevelBuilder {
    ilevel: u8,
    particles: MultiMap<Id, Bone>,
    incomplete: MultiMap<ufo::PdgCode, Incomplete>,
}
impl LevelBuilder {
    fn new(ilevel: u8) -> LevelBuilder {
        LevelBuilder {
            ilevel,
            particles: MultiMap::new(),
            incomplete: MultiMap::new(),
        }
    }

    fn external(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &ufo::UfoModel,
    ) -> LevelBuilder {
        let mut particles = MultiMap::new();
        for (i, &p) in incoming.iter().enumerate() {
            particles.insert(Id::from_external(p, i as u8), Bone::External);
        }
        for (i, &p) in outgoing.iter().enumerate() {
            let anti = model.anti_pdg_code(p);
            particles.insert(
                Id::from_external(anti, (i + incoming.len()) as u8),
                Bone::External,
            );
        }
        LevelBuilder {
            ilevel: 1,
            particles,
            incomplete: MultiMap::new(),
        }
    }

    fn combine(
        &mut self,
        left: &LevelBuilder,
        right: &LevelBuilder,
        vertices: &VertexList,
        model: &ufo::UfoModel,
    ) {
        for (&left_id, _) in left.particles.iter_multi() {
            let vl = &vertices.contents[&left_id.pdg_code];
            for (&right_id, _) in right.particles.iter_multi() {
                self.add_complete_matches(left_id, right_id, &vl, model);
            }
            if let Some(incomplete) = right.incomplete.get(&left_id.pdg_code) {
                self.add_incomplete_matches(left_id, incomplete, model);
            }
        }
        if left.ilevel != right.ilevel {
            for (&right_id, _) in right.particles.iter_multi() {
                if let Some(incomplete) = left.incomplete.get(&right_id.pdg_code) {
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
        let internal = left_id.internal + right_id.internal;
        let mut constituents = vec![left_id, right_id];
        for v in vs.iter() {
            match v {
                &VertexLeaf::Complete(ref c) => {
                    let bone = Bone::Combination {
                        vertex: c.vertex.name.clone(),
                        constituents: constituents.clone(),
                    };
                    let id = Id {
                        pdg_code: model.anti_pdg_code(c.out),
                        internal,
                    };
                    self.particles.insert(id, bone);
                }
                &VertexLeaf::Incomplete(ref c) => {
                    constituents.sort();
                    let incomplete = Incomplete {
                        internal,
                        vertex: c.vertex.name.clone(),
                        remaining: c.remaining[1..].to_owned(),
                        constituents: constituents.clone(),
                    };
                    self.incomplete.insert(c.remaining[0], incomplete);
                }
            }
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
            let mut constituents = inc.constituents.clone();
            constituents.push(left_id);
            let internal = left_id.internal + inc.internal;
            if inc.remaining.len() == 1 {
                let bone = Bone::Combination {
                    vertex: inc.vertex.clone(),
                    constituents,
                };
                let id = Id {
                    pdg_code: model.anti_pdg_code(inc.remaining[0]),
                    internal,
                };
                self.particles.insert(id, bone);
            } else {
                let incomplete = Incomplete {
                    internal,
                    vertex: inc.vertex.clone(),
                    constituents,
                    remaining: inc.remaining[1..].to_owned(),
                };
                self.incomplete.insert(inc.remaining[0], incomplete);
            }
        }
    }

    fn convert(self) -> Level {
        Level {
            level: self.particles,
        }
    }
}

struct Incomplete {
    internal: InternalId,
    vertex: String,
    constituents: Vec<Id>,
    remaining: Vec<ufo::PdgCode>,
}
