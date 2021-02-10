const TF: f64 = 0.5;
const N_COLORS: u8 = 3;

const INITIAL_OFFSET: i64 = -10000;
const OFFSET_LEFT: i64 = 1000;
const OFFSET_RIGHT: i64 = 2000;

use crate::color::tensor::{
    AntiSextetIndex, AntiTripletIndex, ColorIndex, ColorTensor, OctetIndex, SextetIndex,
    TripletIndex, UndefinedIndex,
};
use crate::color::ColorExpr;
use crate::math_expr::parse::parse_math;
use crate::math_expr::{BinaryOperator, Number};
use crate::ufo::Color;
use crate::ufo::UfoMath;

#[derive(Clone, Debug)]
pub struct ChainBuilder {
    pub chains: Vec<Chain>,
}
impl ChainBuilder {
    pub fn from_expr(structure: &UfoMath, external: &[Color]) -> ChainBuilder {
        ChainBuilder {
            chains: vec![Chain::from_expr(structure, external)],
        }
    }
    fn new() -> ChainBuilder {
        ChainBuilder { chains: Vec::new() }
    }

    fn add_chains(&mut self, other: ChainBuilder) {
        self.chains.extend(other.chains);
    }

    pub fn append_all(&mut self, tensor: ColorTensor) {
        for chain in self.chains.iter_mut() {
            chain.append(tensor);
        }
    }

    fn join_all(&mut self, new_chain: &Chain) {
        for chain in self.chains.iter_mut() {
            chain.join(new_chain);
        }
    }

    fn join_all_multi(&mut self, chains: &[Chain]) {
        if chains.len() == 1 {
            return self.join_all(&chains[0]);
        }
        let temp = self.clone();
        self.join_all(&chains[0]);
        for new_chain in &chains[1..] {
            let mut new = temp.clone();
            new.join_all(new_chain);
            self.add_chains(new);
        }
    }

    pub fn replace_tensors<F>(self, function: &F) -> ChainBuilder
    where
        F: Fn(ColorTensor, i64) -> Option<ChainBuilder>,
    {
        let mut out = ChainBuilder::new();
        for chain in self.chains {
            out.add_chains(chain.replace_tensors(function));
        }
        out
    }

    pub fn replace_indices<P, F>(mut self, index_predicate: &P, function: &F) -> ChainBuilder
    where
        P: Fn(&ColorIndex) -> bool,
        F: Fn(&[ColorTensor]) -> ChainBuilder,
    {
        let mut out = ChainBuilder::new();
        for chain in self.chains {
            out.add_chains(chain.replace_indices(index_predicate, function));
        }
        out
    }

    fn offset(&self) -> i64 {
        self.chains
            .iter()
            .map(|c| c.offset)
            .min()
            .unwrap_or(INITIAL_OFFSET)
    }

    fn replace_tensors_initial(mut self, particle_colors: &[Color]) -> ChainBuilder {
        self.replace_tensors(&|tensor, offset| match tensor {
            ColorTensor::KroneckerDelta { i1, jb2 } => {
                Some(replace_identity(i1, jb2, particle_colors, offset))
            }
            ColorTensor::StructureConstant { a1, a2, a3 } => {
                Some(replace_structure_constant(a1, a2, a3, offset))
            }
            ColorTensor::SymmetricTensor { a1, a2, a3 } => {
                Some(replace_symmetric_tensor(a1, a2, a3, offset))
            }
            ColorTensor::SextetRep { a1, alpha2, betab3 } => {
                Some(replace_sextet_rep(a1, alpha2, betab3, offset))
            }
            _ => None,
        })
    }

    fn replace_octet_indices(mut self) -> ChainBuilder {
        let chains = self.chains;
        self.chains = Vec::new();
        for chain in chains {
            self.add_chains(chain.replace_octet_indices());
        }
        self
    }

    fn add_external(mut self, particle_colors: &[Color]) -> ChainBuilder {
        for (i, c) in particle_colors.iter().enumerate() {
            let i = i as i64 + 1;
            match c {
                Color::Octet => {
                    let index = OctetIndex(i);
                    let tensor = ColorTensor::new_fundamental(
                        index,
                        TripletIndex(i + OFFSET_LEFT),
                        AntiTripletIndex(i + OFFSET_RIGHT),
                    );
                    self.append_all(tensor);
                    for chain in self.chains.iter_mut() {
                        chain.octet_indices.push(index);
                    }
                }
                Color::Sextet => {
                    let index = SextetIndex(i);
                    let tensor = ColorTensor::new_sextet(
                        index,
                        AntiTripletIndex(i + OFFSET_LEFT),
                        AntiTripletIndex(i + OFFSET_RIGHT),
                    );
                    self.append_all(tensor);
                    for chain in self.chains.iter_mut() {
                        chain.sextet_indices.push(index);
                    }
                }
                Color::AntiSextet => {
                    let index = AntiSextetIndex(i);
                    let tensor = ColorTensor::new_anti_sextet(
                        index,
                        TripletIndex(i + OFFSET_LEFT),
                        TripletIndex(i + OFFSET_RIGHT),
                    );
                    self.append_all(tensor);
                    for chain in self.chains.iter_mut() {
                        chain.sextet_indices.push(SextetIndex(i));
                    }
                }
                _ => continue,
            }
        }
        self
    }
}

#[derive(Clone, Debug)]
pub struct Chain {
    factor: Number,
    tensors: Vec<ColorTensor>,
    pub triplet_indices: Vec<TripletIndex>,
    pub sextet_indices: Vec<SextetIndex>,
    pub octet_indices: Vec<OctetIndex>,
    offset: i64,
}
impl Chain {
    fn new(offset: i64) -> Chain {
        Chain {
            factor: Number::from(1),
            tensors: Vec::new(),
            triplet_indices: Vec::new(),
            sextet_indices: Vec::new(),
            octet_indices: Vec::new(),
            offset,
        }
    }

    pub fn new_trace<T: Into<Number>>(offset: i64, indices: &[ColorIndex], factor: T) -> Chain {
        let mut chain = Chain::new(offset);
        chain.add_trace(indices);
        chain
    }

    pub fn new_delta_chain<T: Into<Number>>(
        indices: &[(TripletIndex, AntiTripletIndex)],
        factor: T,
    ) -> Chain {
        let tensors = indices
            .iter()
            .map(|(i, j)| ColorTensor::new_kronecker_triplet(*i, *j))
            .collect();
        Chain {
            factor: factor.into(),
            tensors,
            triplet_indices: Vec::new(),
            sextet_indices: Vec::new(),
            octet_indices: Vec::new(),
            offset: 0,
        }
    }

    fn join(&mut self, other: &Chain) {
        self.factor = self.factor * other.factor;
        self.tensors.extend(other.tensors.iter());
        self.triplet_indices.extend(other.triplet_indices.iter());
        self.sextet_indices.extend(other.sextet_indices.iter());
        self.octet_indices.extend(other.octet_indices.iter());
        self.offset = i64::min(self.offset, other.offset);
    }

    fn append(&mut self, tensor: ColorTensor) {
        self.tensors.push(tensor);
    }
    fn replace_tensors<F>(mut self, function: &F) -> ChainBuilder
    where
        F: Fn(ColorTensor, i64) -> Option<ChainBuilder>,
    {
        let tensors = self.tensors;
        self.tensors = Vec::new();
        let mut offset = self.offset;
        let mut out = ChainBuilder { chains: vec![self] };
        for tensor in tensors {
            match function(tensor, offset) {
                Some(new) => out.join_all_multi(&new.chains),
                None => out.append_all(tensor),
            }
            offset = out.offset();
        }
        out
    }

    fn replace_indices<P, F>(mut self, index_predicate: &P, function: &F) -> ChainBuilder
    where
        P: Fn(&ColorIndex) -> bool,
        F: Fn(&[ColorTensor]) -> ChainBuilder,
    {
        let mut indices: Vec<ColorIndex> = self
            .triplet_indices
            .iter()
            .map(|&i| i.into())
            .chain(self.sextet_indices.iter().map(|&i| i.into()))
            .chain(self.octet_indices.iter().map(|&i| i.into()))
            .collect();
        let mut out = ChainBuilder { chains: vec![self] };
        for index in indices.into_iter().filter(index_predicate) {
            let mut next = ChainBuilder::new();
            for chain in out.chains {
                next.add_chains(chain.replace_index(index, function));
            }
            out = next;
        }
        out
    }

    fn replace_index<F>(mut self, index: ColorIndex, function: &F) -> ChainBuilder
    where
        F: Fn(&[ColorTensor]) -> ChainBuilder,
    {
        let mut matches = Vec::new();
        let tensors = self.tensors;
        self.tensors = Vec::new();
        for tensor in tensors {
            if tensor.has_normalized_index(index) {
                matches.push(tensor);
            } else {
                self.tensors.push(tensor);
            }
        }
        let mut out = ChainBuilder { chains: vec![self] };
        out.join_all_multi(&function(&matches).chains);
        out
    }

    fn replace_octet_indices(mut self) -> ChainBuilder {
        let octet = self.octet_indices;
        self.octet_indices = Vec::new();
        let mut out = ChainBuilder { chains: vec![self] };
        for index in octet {
            let mut next = ChainBuilder::new();
            for chain in out.chains {
                next.add_chains(chain.replace_octet_index(index));
            }
            out = next;
        }
        out
    }

    fn replace_octet_index(mut self, index: OctetIndex) -> ChainBuilder {
        let mut matches = Vec::new();
        let tensors = self.tensors;
        self.tensors = Vec::new();
        for tensor in tensors {
            if let Some(indices) = has_octet_index(&tensor, index) {
                matches.push(indices);
            } else {
                self.tensors.push(tensor);
            }
        }
        let mut out = ChainBuilder { chains: vec![self] };
        match *matches {
            [l, r] => out.join_all_multi(&combine_octet(l, r).chains),
            _ => panic!("BUG: Found more than two occurences of index: {:?}", index),
        }
        out
    }

    fn summation_index<T>(&mut self) -> T
    where
        T: From<i64>,
    {
        let next = self.offset;
        self.offset -= 1;
        next.into()
    }

    fn add_trace(&mut self, indices: &[ColorIndex]) {
        let start: TripletIndex = self.summation_index();
        let mut sum = start;
        for (i, index) in indices.iter().enumerate() {
            let next: TripletIndex = if i != indices.len() - 1 {
                self.summation_index()
            } else {
                start
            };
            self.triplet_indices.push(next);
            match index {
                ColorIndex::Octet { index } => self.tensors.push(ColorTensor::FundamentalRep {
                    a1: *index,
                    i2: sum,
                    jb3: next.bar(),
                }),
                ColorIndex::Sextet { index } => {
                    self.tensors.push(ColorTensor::SextetClebschGordan {
                        alpha1: *index,
                        ib2: sum.bar(),
                        jb3: next.bar(),
                    })
                }
                ColorIndex::AntiSextet { index } => {
                    self.tensors.push(ColorTensor::AntiSextetClebschGordan {
                        alphab1: *index,
                        i2: sum,
                        j3: next,
                    })
                }
                _ => panic!("BUG: Unexpected color index in `add_trace`!"),
            }
            sum = next;
        }
    }

    fn from_expr(structure: &UfoMath, external: &[Color]) -> Chain {
        let UfoMath(structure) = structure;
        let expr: ColorExpr = parse_math(structure).unwrap();
        let mut structure = Chain {
            factor: Number::from(1),
            tensors: Vec::new(),
            triplet_indices: Vec::new(),
            sextet_indices: Vec::new(),
            octet_indices: Vec::new(),
            offset: INITIAL_OFFSET,
        };
        structure.add_expr(expr);
        // structure.validate(external);
        structure
    }

    fn add_expr(&mut self, expr: ColorExpr) {
        match expr {
            ColorExpr::BinaryOp {
                operator,
                left,
                right,
            } => {
                if operator != BinaryOperator::Mul {
                    panic!("REPLACE: Unsupported operator");
                }
                self.add_expr(*left);
                self.add_expr(*right);
            }
            ColorExpr::Tensor { tensor } => self.tensors.push(tensor),
            ColorExpr::Sum { expr, index } => {
                match index {
                    ColorIndex::Triplet { index } => self.triplet_indices.push(index),
                    ColorIndex::Sextet { index } => self.sextet_indices.push(index),
                    ColorIndex::Octet { index } => self.octet_indices.push(index),
                    _ => panic!("BUG: The color indices have not been normalized!"),
                }
                self.add_expr(*expr);
            }
            ColorExpr::Number {
                value: Number::Integer(i),
            } => {
                if i != 1 {
                    panic!("REPLACE: Unsupported number");
                }
                // Do nothing in this case, the color structure is trivial
            }
            _ => panic!("REPLACE: Unsupported expression"),
        }
    }
}

fn replace_identity(
    i1: UndefinedIndex,
    jb2: UndefinedIndex,
    particle_colors: &[Color],
    offset: i64,
) -> ChainBuilder {
    let UndefinedIndex(i1) = i1;
    let UndefinedIndex(i2) = jb2;
    if i1 < 0 || i2 < 0 {
        panic!("REPLACE: Summation indices in identity!");
    }
    let chain = match (
        particle_colors[i1 as usize - 1],
        particle_colors[i2 as usize - 1],
    ) {
        (Color::Triplet, Color::AntiTriplet) => {
            Chain::new_delta_chain(&[(TripletIndex::from(i1), AntiTripletIndex::from(i2))], 1)
        }
        (Color::AntiTriplet, Color::Triplet) => {
            Chain::new_delta_chain(&[(TripletIndex::from(i2), AntiTripletIndex::from(i1))], 1)
        }
        (Color::Octet, Color::Octet) => Chain::new_trace(
            offset,
            &[OctetIndex(i1).into(), OctetIndex(i2).into()],
            1f64 / TF,
        ),
        (Color::Sextet, Color::AntiSextet) => Chain::new_trace(
            offset,
            &[SextetIndex(i1).into(), AntiSextetIndex(i2).into()],
            1,
        ),
        (Color::AntiSextet, Color::Sextet) => Chain::new_trace(
            offset,
            &[AntiSextetIndex(i1).into(), SextetIndex(i2).into()],
            1,
        ),
        (a, b) => panic!("REPLACE: Unknown use of Identity!"),
    };
    ChainBuilder {
        chains: vec![chain],
    }
}

fn replace_structure_constant(
    a1: OctetIndex,
    a2: OctetIndex,
    a3: OctetIndex,
    offset: i64,
) -> ChainBuilder {
    ChainBuilder {
        chains: vec![
            Chain::new_trace(offset, &[a1.into(), a2.into(), a3.into()], 1),
            Chain::new_trace(offset, &[a2.into(), a1.into(), a3.into()], -1),
        ],
    }
}

fn replace_symmetric_tensor(
    a1: OctetIndex,
    a2: OctetIndex,
    a3: OctetIndex,
    offset: i64,
) -> ChainBuilder {
    ChainBuilder {
        chains: vec![
            Chain::new_trace(offset, &[a1.into(), a2.into(), a3.into()], 1),
            Chain::new_trace(offset, &[a2.into(), a1.into(), a3.into()], 1),
        ],
    }
}

fn replace_sextet_rep(
    a1: OctetIndex,
    alpha2: SextetIndex,
    betab3: AntiSextetIndex,
    offset: i64,
) -> ChainBuilder {
    ChainBuilder {
        chains: vec![Chain::new_trace(
            offset,
            &[alpha2.into(), a1.into(), betab3.into()],
            2,
        )],
    }
}

fn has_octet_index(
    tensor: &ColorTensor,
    index: OctetIndex,
) -> Option<(TripletIndex, AntiTripletIndex)> {
    match tensor {
        ColorTensor::FundamentalRep { a1, i2, jb3 } if *a1 == index => Some((*i2, *jb3)),
        ColorTensor::SextetRep { .. } => {
            panic!("BUG: All sextet rep matrices must be replaced before calling this function!")
        }
        ColorTensor::StructureConstant { .. } => {
            panic!("BUG: All structure constants must be replaced before calling this function!")
        }
        ColorTensor::SymmetricTensor { .. } => {
            panic!("BUG: All symmetric tensors must be replaced before calling this function!")
        }
        _ => None,
    }
}

fn combine_octet(
    (il, jl): (TripletIndex, AntiTripletIndex),
    (ir, jr): (TripletIndex, AntiTripletIndex),
) -> ChainBuilder {
    ChainBuilder {
        chains: vec![
            Chain::new_delta_chain(&[(il, jr), (ir, jl)], TF),
            Chain::new_delta_chain(&[(il, jl), (ir, jr)], -TF / (N_COLORS as f64)),
        ],
    }
}

fn combine_octet_tensors(tensors: &[ColorTensor]) -> ChainBuilder {
    match tensors {
        &[ColorTensor::FundamentalRep {
            i2: il, jb3: jl, ..
        }, ColorTensor::FundamentalRep {
            i2: ir, jb3: jr, ..
        }] => ChainBuilder {
            chains: vec![
                Chain::new_delta_chain(&[(il, jr), (ir, jl)], TF),
                Chain::new_delta_chain(&[(il, jl), (ir, jr)], -TF / (N_COLORS as f64)),
            ],
        },
        _ => panic!("BUG: Unsupported tensors in octet combination!"),
    }
}

fn replace_octet_indices(chains: ChainBuilder) -> ChainBuilder {
    chains.replace_indices(
        &|index| match index {
            ColorIndex::Octet { .. } => true,
            _ => false,
        },
        &combine_octet_tensors,
    )
}

#[cfg(test)]
mod test {
    use super::ChainBuilder;
    use crate::ufo::{Color, UfoMath};

    #[test]
    fn qqg() {
        let data = UfoMath("T(3,1,2)".to_string());
        let particle_colors = &[Color::Triplet, Color::AntiTriplet, Color::Octet];
        let chain = ChainBuilder::from_expr(&data, particle_colors)
            .replace_tensors_initial(particle_colors)
            .add_external(particle_colors)
            .replace_octet_indices();
        assert_eq!(chain.chains.len(), 2);
    }

    #[test]
    fn ggg() {
        let data = UfoMath("f(1,2,3)".to_string());
        let particle_colors = &[Color::Octet, Color::Octet, Color::Octet];
        let chain = ChainBuilder::from_expr(&data, particle_colors)
            .replace_tensors_initial(particle_colors)
            .add_external(particle_colors)
            .replace_octet_indices();
        // assert_eq!(chain.chains.len(), 7);
    }
}
