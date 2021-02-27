use crate::math_expr::EvalContext;
use crate::skeleton::uncolored::coupling_orders::CouplingOrderRestriction;
use crate::skeleton::uncolored::vertex_checker::VertexChecker;
use crate::skeleton::uncolored::vertex_list::{
    VertexLeaf, VertexLeafState, VertexList, VertexListParticle,
};
use crate::skeleton::uncolored::{Id, InternalId, UncoloredSkeleton};
use crate::skeleton::External;
use crate::ufo::{PdgCode, UfoModel};
use crate::{skeleton, ufo};
use std::collections::hash_map::RandomState;
use std::collections::HashMap;

type Bone = skeleton::Bone<Id, String>;
type BoneFragment = skeleton::BoneFragment<Id, String>;

pub struct SkeletonBuilder<'a> {
    first_level: FirstLevel,
    levels: Vec<Level>,
    model: &'a ufo::UfoModel,
    vertex_checker: VertexChecker<'a>,
    order_restriction: &'a CouplingOrderRestriction,
    vertices: VertexList<'a>,
    last: Id,
}
impl<'a> SkeletonBuilder<'a> {
    pub fn build(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &ufo::UfoModel,
        context: &'a EvalContext,
        order_restriction: &'a CouplingOrderRestriction,
    ) -> Option<UncoloredSkeleton> {
        let mut builder =
            SkeletonBuilder::new(incoming, outgoing, model, context, order_restriction);
        let max_level = incoming.len() + outgoing.len() - 1;
        for ilevel in 2..=max_level {
            builder.construct_level(ilevel);
        }
        let last = builder.extract_last_level();
        if last.fragments.is_empty() {
            return None;
        }
        let mut skeleton = builder.convert(last);
        skeleton.remove_unused();
        Some(skeleton)
    }

    fn new(
        incoming: &[ufo::PdgCode],
        outgoing: &[ufo::PdgCode],
        model: &'a ufo::UfoModel,
        context: &'a EvalContext,
        order_restriction: &'a CouplingOrderRestriction,
    ) -> SkeletonBuilder<'a> {
        let max_level = incoming.len() + outgoing.len() - 1;
        let last = Id {
            pdg_code: incoming[0], //model.anti_pdg_code(incoming[0]),
            internal: InternalId((2 << (max_level - 1)) - 1),
            order: Default::default(),
        };
        let vertex_checker = VertexChecker::new(context, model);
        SkeletonBuilder {
            first_level: FirstLevel::new(&incoming[1..], outgoing, model),
            levels: Vec::new(),
            vertices: VertexList::with_filter(&model.vertices, &|v: &ufo::Vertex| {
                v.particles.iter().all(|p| {
                    let particle = &model.particles[p];
                    !particle.goldstoneboson && particle.ghost_number == 0
                })
            }),
            model,
            vertex_checker,
            order_restriction,
            last,
        }
    }

    fn construct_level(&mut self, ilevel: usize) {
        let mut builder = LevelBuilder::new(
            ilevel as u8,
            &self.vertices,
            &self.model,
            &self.order_restriction,
            &mut self.vertex_checker,
        );
        for i in 1..=(ilevel / 2) {
            let left = i;
            let right = ilevel - i;
            match (left, right) {
                (1, 1) => builder.combine(&self.first_level, &self.first_level),
                (1, r) => builder.combine(&self.first_level, &self.levels[r - 2]),
                (l, r) => builder.combine(&self.levels[l - 2], &self.levels[r - 2]),
            }
        }
        self.levels.push(builder.into_level())
    }

    fn extract_last_level(&mut self) -> Bone {
        let last_level = self.levels.pop().map(|l| l.complete).unwrap_or_default();
        let last_level: Vec<_> = last_level
            .into_iter()
            .filter(|(id, _)| id.pdg_code == self.last.pdg_code)
            .collect();
        let mut fragments = Vec::new();
        for bone in self.order_restriction.select_last_level(last_level) {
            fragments.extend(bone.fragments)
        }
        Bone {
            pdg_code: self.last.pdg_code,
            id: self.last.internal,
            fragments,
        }
    }

    fn convert(self, last: Bone) -> UncoloredSkeleton {
        skeleton::Skeleton {
            external: self.first_level.convert(),
            internal: self.levels.into_iter().map(|l| l.convert()).collect(),
            left_out: 0,
            last,
        }
    }
}

trait AnyLevel {
    type Item;
    fn level_number(&self) -> u8;
    fn complete(&self) -> &HashMap<Id, Self::Item>;
    fn incomplete(&self) -> &HashMap<PdgCode, Vec<Incomplete>>;
}

struct FirstLevel {
    complete: HashMap<Id, External>,
    incomplete: HashMap<PdgCode, Vec<Incomplete>>,
}
impl FirstLevel {
    fn new(incoming: &[PdgCode], outgoing: &[PdgCode], model: &UfoModel) -> FirstLevel {
        let complete = incoming
            .iter()
            .map(|p| (p, true))
            .chain(outgoing.iter().map(|p| (p, false)))
            .enumerate()
            .map(|(i, (pdg, flipped))| new_external(i, *pdg, flipped, model))
            .collect();
        FirstLevel {
            complete,
            incomplete: HashMap::new(),
        }
    }

    fn convert(self) -> HashMap<Id, External> {
        self.complete
    }
}
impl AnyLevel for FirstLevel {
    type Item = External;
    fn level_number(&self) -> u8 {
        1
    }

    fn complete(&self) -> &HashMap<Id, External> {
        &self.complete
    }

    fn incomplete(&self) -> &HashMap<PdgCode, Vec<Incomplete>, RandomState> {
        &self.incomplete
    }
}

struct Level {
    level_number: u8,
    complete: HashMap<Id, Bone>,
    incomplete: HashMap<PdgCode, Vec<Incomplete>>,
}
impl Level {
    fn convert(self) -> HashMap<Id, Bone> {
        self.complete
    }
}

impl AnyLevel for Level {
    type Item = Bone;

    fn level_number(&self) -> u8 {
        self.level_number
    }

    fn complete(&self) -> &HashMap<Id, Self::Item, RandomState> {
        &self.complete
    }

    fn incomplete(&self) -> &HashMap<PdgCode, Vec<Incomplete>, RandomState> {
        &self.incomplete
    }
}

fn new_external(
    particle: usize,
    pdg_code: PdgCode,
    flipped: bool,
    model: &UfoModel,
) -> (Id, External) {
    let pdg_code = if flipped {
        model.anti_pdg_code(pdg_code)
    } else {
        pdg_code
    };
    let id = Id::from_external(pdg_code, particle as u8);
    let internal = id.internal;
    let external = External {
        particle,
        flipped,
        pdg_code,
        id: internal,
    };
    (id, external)
}

struct LevelBuilder<'a, 'b> {
    ilevel: u8,
    complete: HashMap<Id, Bone>,
    incomplete: HashMap<ufo::PdgCode, Vec<Incomplete>>,
    vertices: &'b VertexList<'a>,
    model: &'a ufo::UfoModel,
    order_checker: &'b CouplingOrderRestriction,
    checker: &'b mut VertexChecker<'a>,
}
impl<'a, 'b> LevelBuilder<'a, 'b> {
    fn new(
        ilevel: u8,
        vertices: &'b VertexList<'a>,
        model: &'a ufo::UfoModel,
        order_checker: &'b CouplingOrderRestriction,
        checker: &'b mut VertexChecker<'a>,
    ) -> LevelBuilder<'a, 'b> {
        LevelBuilder {
            ilevel,
            vertices,
            model,
            order_checker,
            checker,
            complete: HashMap::new(),
            incomplete: HashMap::new(),
        }
    }

    fn combine<L, R>(&mut self, left: &L, right: &R)
    where
        L: AnyLevel,
        R: AnyLevel,
    {
        let same_level = left.level_number() == right.level_number();
        for left_id in left.complete().keys() {
            let vl = &self.vertices.contents[&left_id.pdg_code];
            for right_id in right.complete().keys() {
                self.add_complete_matches(&left_id, &right_id, &vl, same_level);
            }
            if let Some(incomplete) = right.incomplete().get(&left_id.pdg_code) {
                self.add_incomplete_matches(&left_id, incomplete);
            }
        }
        if !same_level {
            for right_id in right.complete().keys() {
                if let Some(incomplete) = left.incomplete().get(&right_id.pdg_code) {
                    self.add_incomplete_matches(&right_id, incomplete);
                }
            }
        }
    }

    fn add_complete_matches(
        &mut self,
        left_id: &Id,
        right_id: &Id,
        vertices: &VertexListParticle,
        same_level: bool,
    ) {
        if !left_id.is_independent(right_id) {
            return;
        }
        if same_level && left_id.internal > right_id.internal {
            return;
        }
        let vs = match vertices.contents.get(&right_id.pdg_code) {
            Some(v) => v,
            None => return,
        };
        for v in vs.iter() {
            if !self.checker.check_vertex(v.vertex) {
                continue;
            }
            let order = match self.order_checker.add_vertex(
                &left_id.order,
                &right_id.order,
                &v.vertex,
                &self.model,
            ) {
                Some(order) => order,
                None => continue,
            };
            let combination = specialize_vertex(v, left_id, right_id, &self.model, order);
            self.insert_combination(combination);
        }
    }

    fn add_incomplete_matches(&mut self, left_id: &Id, incomplete: &[Incomplete]) {
        for inc in incomplete {
            let max =
                inc.constituents.last().map(|id| id.internal).expect(
                    "BUG: The list of constituents of an incomplete vertex cannot be empty!",
                );
            if !left_id.internal.is_independent(inc.internal) || left_id.internal < max {
                continue;
            }
            let order = match self.order_checker.add_particle(&inc.order, &left_id.order) {
                Some(order) => order,
                None => continue,
            };
            let combination = inc.add_particle(left_id, order, &self.model);
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
        let bone = self.complete.entry(id).or_insert_with_key(|id| Bone {
            pdg_code: id.pdg_code,
            id: id.internal,
            fragments: Vec::new(),
        });
        bone.fragments.push(fragment);
    }

    fn insert_incomplete(&mut self, pdg: ufo::PdgCode, incomplete: Incomplete) {
        self.incomplete.entry(pdg).or_default().push(incomplete);
    }

    fn into_level(self) -> Level {
        Level {
            level_number: self.ilevel,
            complete: self.complete,
            incomplete: self.incomplete,
        }
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
    order: Vec<u8>,
    vertex: String,
    constituents: Vec<Id>,
    remaining: Vec<ufo::PdgCode>,
}
impl Incomplete {
    fn add_particle(&self, particle_id: &Id, order: Vec<u8>, model: &ufo::UfoModel) -> Combination {
        let mut constituents = self.constituents.clone();
        constituents.push(particle_id.clone());
        let internal = particle_id.internal + self.internal;
        if self.remaining.len() == 1 {
            let (constituents, outgoing) = order_constituents(&self.vertex, constituents, model);
            let fragment = BoneFragment {
                content: self.vertex.clone(),
                constituents,
                outgoing,
            };
            let id = Id {
                pdg_code: model.anti_pdg_code(self.remaining[0]),
                internal,
                order,
            };
            Combination::Complete { id, fragment }
        } else {
            let incomplete = Incomplete {
                internal,
                vertex: self.vertex.clone(),
                constituents,
                remaining: self.remaining[1..].to_owned(),
                order,
            };
            Combination::Incomplete {
                pdg: self.remaining[0],
                incomplete,
            }
        }
    }
}

fn specialize_vertex(
    vertex: &VertexLeaf,
    p1: &Id,
    p2: &Id,
    model: &ufo::UfoModel,
    order: Vec<u8>,
) -> Combination {
    let internal = p1.internal + p2.internal;
    let mut constituents = vec![p1.clone(), p2.clone()];
    match &vertex.state {
        VertexLeafState::Complete(out) => {
            let (constituents, outgoing) =
                order_constituents(&vertex.vertex.name, constituents, model);
            let fragment = BoneFragment {
                content: vertex.vertex.name.clone(),
                constituents,
                outgoing,
            };
            let id = Id {
                pdg_code: model.anti_pdg_code(*out),
                internal,
                order,
            };
            Combination::Complete { id, fragment }
        }
        VertexLeafState::Incomplete(remaining) => {
            constituents.sort_by_key(|id| id.internal);
            let incomplete = Incomplete {
                internal,
                vertex: vertex.vertex.name.clone(),
                remaining: remaining[1..].to_owned(),
                constituents,
                order,
            };
            Combination::Incomplete {
                pdg: remaining[0],
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
    mut constituents: Vec<Id>,
) -> (Vec<Id>, usize) {
    let mut sorted = 0;
    let mut outgoing = None;
    for (o, p) in particles.iter().enumerate() {
        let mut location = None;
        for (i, id) in constituents.iter().enumerate().skip(sorted) {
            if id.pdg_code == *p {
                location = Some(i);
                break;
            }
        }
        match location {
            Some(l) => {
                constituents.swap(sorted, l);
                sorted += 1;
            }
            None => {
                assert_eq!(outgoing, None);
                outgoing = Some(o);
            }
        }
    }
    let outgoing = match outgoing {
        Some(o) => o,
        None => unreachable!("BUG: There has to be an outgoing particle!"),
    };
    (constituents, outgoing)
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
                internal: InternalId(8),
                order: Vec::new(),
            },
            Id {
                pdg_code: PdgCode(23),
                internal: InternalId(6),
                order: Vec::new(),
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
                internal: InternalId(8),
                order: Vec::new(),
            },
            Id {
                pdg_code: PdgCode(23),
                internal: InternalId(6),
                order: Vec::new(),
            },
        ];
        let (cs, out) = super::order_constituents_internal(&particles, constituents.clone());
        assert_eq!(out, 1);
        assert_eq!(cs, vec![constituents[1].clone(), constituents[0].clone()]);
    }
}
