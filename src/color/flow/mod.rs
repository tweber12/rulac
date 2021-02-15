pub mod chain;
pub mod vertex;

use crate::color::tensor::MultiIndexLocation;
use crate::ufo::{Color, PdgCode, UfoModel};
use permutohedron::LexicalPermutation;
use std::ops::Index;

#[derive(Debug)]
pub struct ColorFlow {
    pub components: Vec<ColorMultiLine>,
}
impl ColorFlow {
    pub fn new(components: Vec<ColorMultiLine>) -> ColorFlow {
        ColorFlow { components }
    }
    pub fn iter(&self) -> impl Iterator<Item = &ColorMultiLine> {
        self.components.iter()
    }
    pub fn get_line_with_external(
        &self,
        index: usize,
        location: MultiIndexLocation,
        external: usize,
    ) -> ColorLine {
        let index = if index > external { index - 1 } else { index };
        self.components[index].get_line(location)
    }
    pub fn get_anti_line_with_external(
        &self,
        index: usize,
        location: MultiIndexLocation,
        external: usize,
    ) -> AntiColorLine {
        let index = if index >= external { index - 1 } else { index };
        self.components[index].get_anti_line(location)
    }
}
impl Index<usize> for ColorFlow {
    type Output = ColorMultiLine;
    fn index(&self, index: usize) -> &ColorMultiLine {
        self.components.index(index)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ColorMultiLine {
    Singlet,
    Triplet(ColorLine),
    AntiTriplet(AntiColorLine),
    Sextet(ColorLine, ColorLine),
    AntiSextet(AntiColorLine, AntiColorLine),
    Octet(ColorLine, AntiColorLine),
    Phantom,
}
impl ColorMultiLine {
    pub fn invert(&self) -> ColorMultiLine {
        match self {
            ColorMultiLine::Triplet(line) => ColorMultiLine::AntiTriplet(line.invert()),
            ColorMultiLine::AntiTriplet(line) => ColorMultiLine::Triplet(line.invert()),
            ColorMultiLine::AntiSextet(l1, l2) => ColorMultiLine::Sextet(l2.invert(), l1.invert()),
            ColorMultiLine::Sextet(l1, l2) => ColorMultiLine::AntiSextet(l2.invert(), l1.invert()),
            ColorMultiLine::Octet(l1, l2) => ColorMultiLine::Octet(l2.invert(), l1.invert()),
            _ => *self,
        }
    }
    fn get_line(&self, location: MultiIndexLocation) -> ColorLine {
        match self {
            ColorMultiLine::Triplet(line) if location == MultiIndexLocation::Single => *line,
            ColorMultiLine::Sextet(line, _) if location == MultiIndexLocation::IndexOne => *line,
            ColorMultiLine::Sextet(_, line) if location == MultiIndexLocation::IndexTwo => *line,
            ColorMultiLine::Octet(line, _) if location == MultiIndexLocation::IndexOne => *line,
            _ => panic!("BUG: Mismatch of color lines!"),
        }
    }
    fn get_anti_line(&self, location: MultiIndexLocation) -> AntiColorLine {
        match self {
            ColorMultiLine::AntiTriplet(line) if location == MultiIndexLocation::Single => *line,
            ColorMultiLine::AntiSextet(line, _) if location == MultiIndexLocation::IndexOne => {
                *line
            }
            ColorMultiLine::AntiSextet(_, line) if location == MultiIndexLocation::IndexTwo => {
                *line
            }
            ColorMultiLine::Octet(_, line) if location == MultiIndexLocation::IndexTwo => *line,
            _ => panic!("BUG: Mismatch of color lines!"),
        }
    }
    pub fn from_flow_lines(lines: &[FlowLine]) -> ColorMultiLine {
        match lines {
            [] => ColorMultiLine::Singlet,
            [FlowLine::Phantom] => ColorMultiLine::Phantom,
            [FlowLine::Color { line, .. }] => ColorMultiLine::Triplet(*line),
            [FlowLine::AntiColor { line, .. }] => ColorMultiLine::AntiTriplet(*line),
            [FlowLine::Color { line: l, .. }, FlowLine::AntiColor { line: r, .. }] => {
                ColorMultiLine::Octet(*l, *r)
            }
            [FlowLine::AntiColor { line: l, .. }, FlowLine::Color { line: r, .. }] => {
                ColorMultiLine::Octet(*r, *l)
            }
            [FlowLine::Color { line: l, .. }, FlowLine::Color { line: r, .. }] => {
                if *l < *r {
                    ColorMultiLine::Sextet(*l, *r)
                } else {
                    ColorMultiLine::Sextet(*r, *l)
                }
            }
            [FlowLine::AntiColor { line: l, .. }, FlowLine::AntiColor { line: r, .. }] => {
                if *l < *r {
                    ColorMultiLine::AntiSextet(*l, *r)
                } else {
                    ColorMultiLine::AntiSextet(*r, *l)
                }
            }
            _ => panic!("BUG: Too many individual color lines for a Multiline!"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ColorLine(pub u8);
impl ColorLine {
    pub fn invert(self) -> AntiColorLine {
        let ColorLine(i) = self;
        AntiColorLine(i)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AntiColorLine(pub u8);
impl AntiColorLine {
    pub fn invert(self) -> ColorLine {
        let AntiColorLine(i) = self;
        ColorLine(i)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FlowLine {
    Color {
        line: ColorLine,
        location: MultiIndexLocation,
    },
    AntiColor {
        line: AntiColorLine,
        location: MultiIndexLocation,
    },
    Phantom,
}

#[derive(Debug)]
pub struct ColorFlows {
    flows: Vec<ColorFlow>,
}
impl ColorFlows {
    pub fn new(incoming: &[PdgCode], outgoing: &[PdgCode], model: &UfoModel) -> ColorFlows {
        let n_in = incoming.len();
        let colors: Vec<_> = incoming
            .iter()
            .map(|p| model.particles[p].color)
            .chain(outgoing.iter().map(|p| model.particles[p].color.bar()))
            .collect();
        let mut lines = Vec::new();
        for color in colors.iter() {
            match color {
                Color::Triplet => lines.push(ColorLine(lines.len() as u8 + 1)),
                Color::Sextet => {
                    lines.push(ColorLine(lines.len() as u8 + 1));
                    lines.push(ColorLine(lines.len() as u8 + 1));
                }
                Color::Octet => lines.push(ColorLine(lines.len() as u8 + 1)),
                _ => continue,
            }
        }
        let mut anti_lines: Vec<_> = lines.iter().map(|i| i.invert()).collect();
        let mut flows = vec![to_color_flow(&colors, &lines, &anti_lines, n_in)];
        while anti_lines.next_permutation() {
            flows.push(to_color_flow(&colors, &lines, &anti_lines, n_in));
        }
        ColorFlows { flows }
    }
    pub fn iter(&self) -> impl Iterator<Item = &ColorFlow> {
        self.flows.iter()
    }
}

/// Takes all colorflow lines that appear in the event, in the current order
/// and attaches them to the correct particles
fn to_color_flow(
    particles: &[Color],
    colors: &[ColorLine],
    anti_colors: &[AntiColorLine],
    n_in: usize,
) -> ColorFlow {
    let mut i_color = 0;
    let mut i_anti = 0;
    let mut next_color = || {
        let c = colors[i_color];
        i_color += 1;
        c
    };
    let mut next_anti = || {
        let c = anti_colors[i_anti];
        i_anti += 1;
        c
    };
    let flow = particles
        .iter()
        .map(|c| match c {
            Color::Singlet => ColorMultiLine::Singlet,
            Color::Triplet => ColorMultiLine::Triplet(next_color()),
            Color::AntiTriplet => ColorMultiLine::AntiTriplet(next_anti()),
            Color::Sextet => ColorMultiLine::Sextet(next_color(), next_color()),
            Color::AntiSextet => ColorMultiLine::AntiSextet(next_anti(), next_anti()),
            Color::Octet => ColorMultiLine::Octet(next_color(), next_anti()),
        })
        .enumerate()
        .map(|(i, c)| if i < n_in { c } else { c.invert() })
        .collect();
    ColorFlow { components: flow }
}
