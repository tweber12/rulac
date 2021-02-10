pub mod chain;
pub mod vertex;

use crate::ufo::{Color, PdgCode, UfoModel};
use permutohedron::LexicalPermutation;
use std::ops::Index;

pub struct ColorFlow {
    components: Vec<ColorMultiLine>,
}
impl ColorFlow {
    pub fn iter(&self) -> impl Iterator<Item = &ColorMultiLine> {
        self.components.iter()
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ColorLine(u8);
impl ColorLine {
    pub fn invert(self) -> AntiColorLine {
        let ColorLine(i) = self;
        AntiColorLine(i)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AntiColorLine(u8);
impl AntiColorLine {
    pub fn invert(self) -> ColorLine {
        let AntiColorLine(i) = self;
        ColorLine(i)
    }
}

pub struct ColorFlows {
    flows: Vec<ColorFlow>,
}
impl ColorFlows {
    pub fn new(particles: &[PdgCode], model: &UfoModel) -> ColorFlows {
        let particles: Vec<_> = particles.iter().map(|p| model.particles[p].color).collect();
        let mut colors = Vec::new();
        for color in particles.iter() {
            match color {
                Color::Triplet => colors.push(ColorLine(colors.len() as u8 + 1)),
                Color::Sextet => {
                    colors.push(ColorLine(colors.len() as u8 + 1));
                    colors.push(ColorLine(colors.len() as u8 + 1));
                }
                Color::Octet => colors.push(ColorLine(colors.len() as u8 + 1)),
                _ => continue,
            }
        }
        let mut anti_colors: Vec<_> = colors.iter().map(|i| i.invert()).collect();
        let mut flows = vec![to_color_flow(&particles, &colors, &anti_colors)];
        while anti_colors.next_permutation() {
            flows.push(to_color_flow(&particles, &colors, &anti_colors));
        }
        ColorFlows { flows }
    }
    pub fn iter(&self) -> impl Iterator<Item = &ColorFlow> {
        self.flows.iter()
    }
}

fn to_color_flow(
    particles: &[Color],
    colors: &[ColorLine],
    anti_colors: &[AntiColorLine],
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
        .collect();
    ColorFlow { components: flow }
}
