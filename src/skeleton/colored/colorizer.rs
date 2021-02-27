use crate::color::flow::vertex::{get_vertex_flows, Match, VertexFlows};
use crate::color::flow::{ColorFlow, ColorMultiLine};
use crate::skeleton::colored::{BoneStructure, ColorId, ColoredSkeleton};
use crate::skeleton::uncolored::{Id, UncoloredSkeleton};
use crate::skeleton::{Bone, BoneFragment, External, InternalLevel};
use crate::ufo::{UfoModel, Vertex};
use crate::util::Combinations;
use std::collections::HashMap;

pub fn colorize(
    skeleton: &UncoloredSkeleton,
    model: &UfoModel,
    color_flow: &ColorFlow,
) -> Option<ColoredSkeleton> {
    let mut level_builder =
        Colorizer::new(color_flow, &skeleton.external, &model, skeleton.left_out);
    for level in &skeleton.internal {
        level_builder.convert_level(level);
    }
    let last_level =
        level_builder.convert_last_level(&skeleton.last, color_flow.components[skeleton.left_out]);
    if last_level.fragments.is_empty() {
        return None;
    }
    let mut skeleton = ColoredSkeleton {
        external: level_builder.external,
        internal: level_builder.internal,
        last: last_level,
        left_out: skeleton.left_out,
    };
    skeleton.remove_unused();
    Some(skeleton)
}

type UncoloredLevel = InternalLevel<Id, String>;
type ColoredLevel = InternalLevel<ColorId, Vec<BoneStructure>>;
type UncoloredBone = Bone<Id, String>;
type ColoredBone = Bone<ColorId, Vec<BoneStructure>>;
type UncoloredFragment = BoneFragment<Id, String>;
type ColoredFragment = BoneFragment<ColorId, Vec<BoneStructure>>;

struct Colorizer<'a> {
    model: &'a UfoModel,
    internal: Vec<ColoredLevel>,
    external: HashMap<ColorId, External>,
    outgoing_colors: HashMap<Id, Vec<ColorMultiLine>>,
}
impl<'a> Colorizer<'a> {
    fn new(
        color_flow: &ColorFlow,
        external: &HashMap<Id, External>,
        model: &'a UfoModel,
        left_out: usize,
    ) -> Colorizer<'a> {
        let mut outgoing_colors = HashMap::new();
        let mut first_level = HashMap::new();
        for (id, ex) in external.iter() {
            let particle = if ex.particle >= left_out {
                ex.particle + 1
            } else {
                ex.particle
            };
            let line = color_flow[particle];
            if ex.flipped {
                outgoing_colors.insert(id.clone(), vec![line.invert()]);
            } else {
                outgoing_colors.insert(id.clone(), vec![line]);
            }
            first_level.insert(ColorId(id.clone(), line), *ex);
        }
        Colorizer {
            external: first_level,
            internal: Vec::new(),
            outgoing_colors,
            model,
        }
    }

    fn convert_level(&mut self, level: &UncoloredLevel) -> ColoredLevel {
        let mut colored = HashMap::new();
        for (id, bone) in level.iter() {
            for (color, colored_bone) in self.convert_bone(bone) {
                colored.insert(ColorId(id.clone(), color), colored_bone);
                self.insert_outgoing_color(id.clone(), color);
            }
        }
        colored
    }

    fn convert_bone(&mut self, bone: &UncoloredBone) -> Vec<(ColorMultiLine, ColoredBone)> {
        let mut bones: HashMap<ColorMultiLine, Vec<ColoredFragment>> = HashMap::new();
        for fragment in bone.fragments.iter() {
            for (c, f) in self.convert_fragment(fragment) {
                bones.entry(c).or_default().push(f);
            }
        }
        bones
            .into_iter()
            .map(|(c, fs)| {
                (
                    c,
                    ColoredBone {
                        pdg_code: bone.pdg_code,
                        id: bone.id,
                        fragments: fs,
                    },
                )
            })
            .collect()
    }

    fn convert_last_level(&self, bone: &UncoloredBone, color: ColorMultiLine) -> ColoredBone {
        let mut converted = Vec::new();
        for fragment in bone.fragments.iter() {
            for (c, f) in self.convert_fragment(fragment) {
                if c == color {
                    converted.push(f);
                } else if c == ColorMultiLine::Phantom {
                    match color {
                        ColorMultiLine::Octet(l1, l2) if l1.invert() == l2 => converted.push(f),
                        _ => (),
                    }
                }
            }
        }
        ColoredBone {
            pdg_code: bone.pdg_code,
            id: bone.id,
            fragments: converted,
        }
    }

    fn convert_fragment(
        &self,
        fragment: &UncoloredFragment,
    ) -> HashMap<ColorMultiLine, ColoredFragment> {
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
}

struct FragmentColorizer<'a> {
    constituents: &'a [Id],
    vertex: &'a Vertex,
    outgoing: usize,
    vertex_flows: Vec<VertexFlows>,
    colored: HashMap<ColorMultiLine, ColoredFragment>,
}
impl<'a> FragmentColorizer<'a> {
    fn colorize(
        fragment: &UncoloredFragment,
        incoming_colors: Vec<Vec<ColorMultiLine>>,
        model: &'a UfoModel,
    ) -> HashMap<ColorMultiLine, ColoredFragment> {
        FragmentColorizer::new(fragment, model)
            .add_color(incoming_colors)
            .finalize()
    }

    fn new(fragment: &'a UncoloredFragment, model: &'a UfoModel) -> FragmentColorizer<'a> {
        let vertex = &model.vertices[&fragment.content];
        let particle_colors = vertex.get_particle_colors(model);
        let vertex_flows: Vec<_> = vertex
            .color
            .iter()
            .map(|v| get_vertex_flows(v.clone(), particle_colors.clone()))
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
            .map(|(i, c)| ColorId(i.clone(), *c))
            .collect();
        for vc in self.vertex.couplings.iter() {
            self.add_fragment_for_matches(
                &constituents,
                &matches[vc.color_index],
                vc.lorentz_index,
                &vc.coupling,
                self.outgoing,
            );
        }
    }

    fn add_fragment_for_matches(
        &mut self,
        constituents: &[ColorId],
        matches: &[Match],
        lorentz_index: usize,
        coupling: &str,
        outgoing: usize,
    ) {
        if matches.is_empty() {
            return;
        }
        let lorentz_structure = &self.vertex.lorentz[lorentz_index];
        for m in matches {
            self.insert_fragment(constituents, lorentz_structure, coupling, m, outgoing);
        }
    }

    fn insert_fragment(
        &mut self,
        constituents: &[ColorId],
        lorentz_structure: &str,
        coupling: &str,
        color_match: &Match,
        outgoing: usize,
    ) {
        let fragment =
            self.colored
                .entry(color_match.outgoing)
                .or_insert_with(|| ColoredFragment {
                    constituents: constituents.to_vec(),
                    content: Vec::new(),
                    outgoing,
                });
        fragment.content.push(BoneStructure {
            coupling: coupling.to_string(),
            factor: color_match.factor,
            lorentz_structure: lorentz_structure.to_string(),
        });
    }

    fn finalize(self) -> HashMap<ColorMultiLine, ColoredFragment> {
        self.colored
    }
}
