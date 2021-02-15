use crate::color::flow::vertex::{Match, VertexFlow, VertexFlows};
use crate::color::flow::{ColorFlow, ColorMultiLine};
use crate::skeleton::colored::{Bone, BoneFragment, BoneStructure, ColorId, Level, Skeleton};
use crate::skeleton::uncolored;
use crate::skeleton::uncolored::{Id, UncoloredSkeleton};
use crate::ufo::{UfoModel, Vertex};
use crate::util::Combinations;
use std::collections::{HashMap, HashSet};

pub struct Colorizer<'a> {
    model: &'a UfoModel,
    skeleton: UncoloredSkeleton,
}
impl<'a> Colorizer<'a> {
    pub fn new(skeleton: UncoloredSkeleton, model: &'a UfoModel) -> Colorizer<'a> {
        Colorizer { model, skeleton }
    }
    pub fn generate(&self, color_flow: &ColorFlow) -> Option<Skeleton> {
        let mut level_builder = LevelColorizer::new(
            color_flow,
            &self.skeleton.levels[0],
            &self.model,
            self.skeleton.last_level_index,
        );
        for level in &self.skeleton.levels[1..] {
            level_builder.convert_level(level);
        }
        let last_level = level_builder.convert_last_level(
            &self.skeleton.last_level,
            color_flow.components[self.skeleton.last_level_index],
        );
        if last_level.is_empty() {
            return None;
        }
        level_builder.remove_unused(&last_level);
        Some(Skeleton {
            levels: level_builder.levels,
            last_level,
        })
    }
}

struct LevelColorizer<'a> {
    model: &'a UfoModel,
    outgoing_colors: HashMap<Id, Vec<ColorMultiLine>>,
    levels: Vec<Level>,
}
impl<'a> LevelColorizer<'a> {
    fn new(
        color_flow: &ColorFlow,
        external: &uncolored::Level,
        model: &'a UfoModel,
        left_out: usize,
    ) -> LevelColorizer<'a> {
        let mut outgoing_colors = HashMap::new();
        let mut level_one = HashMap::new();
        for (&id, ex) in external.level.iter() {
            match *ex {
                uncolored::Bone::External { particle, flipped } => {
                    let particle = if particle >= left_out {
                        particle + 1
                    } else {
                        particle
                    };
                    let line = color_flow[particle];
                    if flipped {
                        outgoing_colors.insert(id, vec![line.invert()]);
                    } else {
                        outgoing_colors.insert(id, vec![line]);
                    }
                    level_one.insert(ColorId::new(id, line), Bone::External { particle, flipped });
                }
                _ => panic!("BUG: Composite skeleton object on level 1!"),
            }
        }
        LevelColorizer {
            outgoing_colors,
            model,
            levels: vec![Level { bones: level_one }],
        }
    }

    fn convert_level(&mut self, level: &uncolored::Level) -> Level {
        let mut colored = HashMap::new();
        for (id, bone) in level.level.iter() {
            for (color, colored_bone) in self.convert_bone(bone) {
                colored.insert(ColorId::new(*id, color), colored_bone);
                self.insert_outgoing_color(*id, color);
            }
        }
        Level { bones: colored }
    }

    fn convert_bone(&self, bone: &uncolored::Bone) -> Vec<(ColorMultiLine, Bone)> {
        let fragments = match bone {
            uncolored::Bone::External { .. } => {
                unreachable!("BUG: External skeleton object on level other than 1!")
            }
            uncolored::Bone::Internal { fragments } => fragments,
        };
        let mut bones: HashMap<ColorMultiLine, Vec<BoneFragment>> = HashMap::new();
        for fragment in fragments {
            for (c, f) in self.convert_fragment(fragment) {
                bones.entry(c).or_default().push(f);
            }
        }
        bones
            .into_iter()
            .map(|(c, fs)| (c, Bone::Internal { fragments: fs }))
            .collect()
    }

    fn convert_last_level(
        &self,
        fragments: &[uncolored::BoneFragment],
        color: ColorMultiLine,
    ) -> Vec<BoneFragment> {
        let mut converted = Vec::new();
        let anti = color.invert();
        for fragment in fragments {
            for (c, f) in self.convert_fragment(fragment) {
                if c == anti {
                    converted.push(f);
                } else if c == ColorMultiLine::Phantom {
                    match anti {
                        ColorMultiLine::Octet(l1, l2) if l1.invert() == l2 => converted.push(f),
                        _ => (),
                    }
                }
            }
        }
        converted
    }

    fn convert_fragment(
        &self,
        fragment: &uncolored::BoneFragment,
    ) -> HashMap<ColorMultiLine, BoneFragment> {
        if let Some(flows) = self.get_incoming_colors(&fragment.constituents) {
            FragmentColorizer::colorize(fragment, flows, &self.model)
        } else {
            HashMap::new()
        }
    }

    fn insert_outgoing_color(&mut self, id: Id, color: ColorMultiLine) {
        self.outgoing_colors.entry(id).or_default().push(color);
    }

    fn get_incoming_colors(&self, constituents: &[Id]) -> Option<Vec<Vec<ColorMultiLine>>> {
        let out = constituents
            .iter()
            .map(|c| self.outgoing_colors.get(c).cloned())
            .collect();
        out
    }

    fn remove_unused(&mut self, last_level: &[BoneFragment]) {
        let mut used: HashSet<ColorId> = HashSet::new();
        for f in last_level {
            used.extend(f.constituents.iter());
        }
        let mut iter = self.levels.iter_mut();
        while let Some(level) = iter.next_back() {
            level.bones.retain(|id, bone| {
                if !used.contains(id) {
                    return false;
                }
                match bone {
                    Bone::External { .. } => (),
                    Bone::Internal { fragments } => {
                        for f in fragments {
                            used.extend(f.constituents.iter());
                        }
                    }
                }
                true
            });
        }
    }
}

struct FragmentColorizer<'a> {
    constituents: &'a [Id],
    vertex: &'a Vertex,
    outgoing: usize,
    vertex_flows: Vec<VertexFlows>,
    colored: HashMap<ColorMultiLine, BoneFragment>,
}
impl<'a> FragmentColorizer<'a> {
    fn colorize(
        fragment: &uncolored::BoneFragment,
        incoming_colors: Vec<Vec<ColorMultiLine>>,
        model: &'a UfoModel,
    ) -> HashMap<ColorMultiLine, BoneFragment> {
        FragmentColorizer::new(fragment, model)
            .add_color(incoming_colors)
            .finalize()
    }

    fn new(fragment: &'a uncolored::BoneFragment, model: &'a UfoModel) -> FragmentColorizer<'a> {
        let vertex = &model.vertices[&fragment.vertex];
        let particle_colors = vertex.get_particle_colors(model);
        let vertex_flows: Vec<_> = vertex
            .color
            .iter()
            .map(|v| VertexFlows::from_structure(v, &particle_colors))
            .collect();
        FragmentColorizer {
            vertex,
            constituents: &fragment.constituents,
            outgoing: fragment.outgoing,
            vertex_flows,
            colored: HashMap::new(),
        }
    }

    fn add_color(mut self, incoming_colors: Vec<Vec<ColorMultiLine>>) -> FragmentColorizer<'a> {
        let combinations = Combinations::new(incoming_colors);
        for combination in combinations.map(ColorFlow::new) {
            self.add_color_for_combination(&combination);
        }
        self
    }

    fn add_color_for_combination(&mut self, combination: &ColorFlow) {
        let matches: Vec<_> = self
            .vertex_flows
            .iter()
            .map(|vf| vf.matches(combination, self.outgoing))
            .collect();
        if matches.is_empty() {
            return;
        }
        let constituents: Vec<_> = self
            .constituents
            .iter()
            .zip(combination.components.iter())
            .map(|(i, c)| ColorId {
                internal: *i,
                color_flow: *c,
            })
            .collect();
        for vc in self.vertex.couplings.iter() {
            self.add_fragment_for_matches(
                &constituents,
                &matches[vc.color_index],
                vc.lorentz_index,
                &vc.coupling,
            );
        }
    }

    fn add_fragment_for_matches(
        &mut self,
        constituents: &[ColorId],
        matches: &[Match],
        lorentz_index: usize,
        coupling: &str,
    ) {
        if matches.is_empty() {
            return;
        }
        let lorentz_structure = &self.vertex.lorentz[lorentz_index];
        for m in matches {
            self.insert_fragment(constituents, lorentz_structure, coupling, m);
        }
    }

    fn insert_fragment(
        &mut self,
        constituents: &[ColorId],
        lorentz_structure: &str,
        coupling: &str,
        color_match: &Match,
    ) {
        let fragment = self
            .colored
            .entry(color_match.outgoing)
            .or_insert_with(|| BoneFragment {
                constituents: constituents.to_vec(),
                structures: Vec::new(),
            });
        fragment.structures.push(BoneStructure {
            coupling: coupling.to_string(),
            factor: color_match.factor,
            lorentz_structure: lorentz_structure.to_string(),
        });
    }

    fn finalize(self) -> HashMap<ColorMultiLine, BoneFragment> {
        self.colored
    }
}
