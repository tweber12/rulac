use crate::color::flow::chain::{Chain, ChainBuilder};
use crate::color::flow::{ColorFlow, ColorMultiLine, FlowLine};
use crate::color::tensor::{
    AntiSextetIndex, AntiTripletIndex, ColorIndex, ColorTensor, MultiIndexLocation, OctetIndex,
    SextetIndex, TripletIndex, UndefinedIndex,
};
use crate::ufo::{Color, UfoMath};
use num_rational::Rational32;
use num_traits::{ToPrimitive, Zero};

const INV_TF: i32 = 2;
const TF: Rational32 = Rational32::new_raw(1, INV_TF);
const INV_SQRT_TF: f64 = std::f64::consts::SQRT_2;
const N_COLORS: i32 = 3;

#[derive(Clone, Debug)]
pub struct VertexFlows {
    flows: Vec<VertexFlow>,
}
impl VertexFlows {
    pub fn from_structure(structure: &UfoMath, external: &[Color]) -> VertexFlows {
        let chains = ChainBuilder::from_expr(structure);
        let chains = replace_tensors_initial(chains, external);
        let chains = add_external(chains, external);
        let chains = replace_octet_indices(chains);
        let chains = replace_sextet_indices(chains);
        let chains = replace_epsilon_pairs(chains);
        let chains = replace_triplet_indices(chains);
        let chains = chains.reduce();
        let factor = get_external_factors(external);
        let flows = chains
            .chains
            .into_iter()
            .map(|chain| VertexFlow::from_chain(chain, factor))
            .collect();
        VertexFlows { flows }
    }
    pub fn matches(&self, flows: &ColorFlow, external: usize) -> Vec<Match> {
        let mut matches: Vec<_> = self
            .flows
            .iter()
            .filter_map(|flow| flow.matches(flows, external))
            .collect();
        matches.sort_by_key(|m| m.outgoing);
        let mut iter = matches.into_iter();
        let mut current = match iter.next() {
            Some(m) => m,
            None => return Vec::new(),
        };
        let mut out = Vec::new();
        for m in iter {
            if m.outgoing == current.outgoing {
                current.factor += m.factor;
            } else {
                if !current.factor.is_zero() {
                    out.push(current);
                }
                current = m;
            }
        }
        if !current.factor.is_zero() {
            out.push(current);
        }
        out
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Match {
    pub outgoing: ColorMultiLine,
    pub factor: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VertexFlow {
    factor: f64,
    deltas: Vec<Delta>,
}
impl VertexFlow {
    fn from_chain(chain: Chain, external: f64) -> VertexFlow {
        let mut deltas = Vec::new();
        for tensor in chain.tensors {
            match tensor {
                ColorTensor::KroneckerTriplet { i1, jb2 } => {
                    deltas.push(Delta::from_kronecker(i1, jb2))
                }
                _ => panic!("Unsupported tensor in vertex flow: {:?}", tensor),
            }
        }
        VertexFlow {
            deltas,
            factor: chain
                .factor
                .to_f64()
                .expect("BUG: Factor cannot be converted to f64")
                * external,
        }
    }

    fn matches(&self, flows: &ColorFlow, external: usize) -> Option<Match> {
        self.matches_internal(flows, external, true)
    }

    fn matches_internal(
        &self,
        flows: &ColorFlow,
        external: usize,
        check_phantom: bool,
    ) -> Option<Match> {
        if check_phantom && flows.components.contains(&ColorMultiLine::Phantom) {
            return self.matches_phantom(flows, external);
        }
        let mut out = Vec::new();
        for delta in self.deltas.iter() {
            if delta.has_external(external) {
                out.push(delta.get_color_out(flows, external));
            } else if !delta.matches(flows, external) {
                return None;
            }
        }
        Some(Match {
            outgoing: ColorMultiLine::from_flow_lines(&out),
            factor: self.factor,
        })
    }

    fn matches_phantom(&self, flows: &ColorFlow, external: usize) -> Option<Match> {
        let mut deltas = self.deltas.clone();
        for (i, f) in flows.components.iter().enumerate() {
            if *f == ColorMultiLine::Phantom {
                deltas = eliminate_phantom(deltas, i);
            }
        }
        if deltas.is_empty() {
            Some(Match {
                outgoing: ColorMultiLine::Phantom,
                factor: self.factor,
            })
        } else {
            VertexFlow {
                deltas,
                factor: self.factor,
            }
            .matches_internal(flows, external, false)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Delta {
    triplet: usize,
    anti: usize,
    triplet_location: MultiIndexLocation,
    anti_location: MultiIndexLocation,
}
impl Delta {
    fn has_external(&self, particle: usize) -> bool {
        self.triplet == particle || self.anti == particle
    }

    fn matches(&self, flows: &ColorFlow, external: usize) -> bool {
        let line = flows.get_line_with_external(self.triplet, self.triplet_location, external);
        let antiline = flows.get_anti_line_with_external(self.anti, self.anti_location, external);
        line.invert() == antiline
    }

    fn get_color_out(&self, flows: &ColorFlow, external: usize) -> FlowLine {
        if self.triplet == self.anti && self.triplet == external {
            FlowLine::Phantom
        } else if self.triplet == external {
            let line = flows.get_anti_line_with_external(self.anti, self.anti_location, external);
            FlowLine::AntiColor {
                line,
                location: self.triplet_location,
            }
        } else if self.anti == external {
            let line = flows.get_line_with_external(self.triplet, self.triplet_location, external);
            FlowLine::Color {
                line,
                location: self.anti_location,
            }
        } else {
            panic!("BUG: Outgoing color requested for internal delta!");
        }
    }

    fn from_kronecker(triplet: TripletIndex, anti: AntiTripletIndex) -> Delta {
        Delta {
            triplet: triplet.particle_index(),
            anti: anti.particle_index(),
            triplet_location: triplet.get_multi_location(),
            anti_location: anti.get_multi_location(),
        }
    }
}

fn eliminate_phantom(deltas: Vec<Delta>, index: usize) -> Vec<Delta> {
    let (phantom, mut deltas): (Vec<_>, Vec<_>) =
        deltas.into_iter().partition(|d| d.has_external(index));
    match &*phantom {
        [_delta] => (),
        [d1, d2] => deltas.push(join_deltas(d1.clone(), d2.clone(), index)),
        _ => unreachable!("BUG: Phantoms can only be included in one or two deltas!"),
    };
    deltas
}

fn join_deltas(d1: Delta, d2: Delta, on_index: usize) -> Delta {
    if d1.triplet == on_index {
        Delta {
            triplet: d2.triplet,
            triplet_location: d2.triplet_location,
            anti: d1.anti,
            anti_location: d1.anti_location,
        }
    } else {
        Delta {
            triplet: d1.triplet,
            triplet_location: d1.triplet_location,
            anti: d2.anti,
            anti_location: d2.anti_location,
        }
    }
}

fn replace_tensors_initial(chains: ChainBuilder, particle_colors: &[Color]) -> ChainBuilder {
    chains.replace_tensors(&|tensor, offset| match tensor {
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
            INV_TF,
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
        _ => panic!("REPLACE: Unknown use of Identity!"),
    };
    ChainBuilder::from_chain(chain)
}

fn replace_structure_constant(
    a1: OctetIndex,
    a2: OctetIndex,
    a3: OctetIndex,
    offset: i64,
) -> ChainBuilder {
    ChainBuilder::from_chains(&[
        Chain::new_trace(offset, &[a1.into(), a2.into(), a3.into()], INV_TF),
        Chain::new_trace(offset, &[a2.into(), a1.into(), a3.into()], -INV_TF),
    ])
}

fn replace_symmetric_tensor(
    a1: OctetIndex,
    a2: OctetIndex,
    a3: OctetIndex,
    offset: i64,
) -> ChainBuilder {
    ChainBuilder::from_chains(&[
        Chain::new_trace(offset, &[a1.into(), a2.into(), a3.into()], INV_TF),
        Chain::new_trace(offset, &[a2.into(), a1.into(), a3.into()], INV_TF),
    ])
}

fn replace_sextet_rep(
    a1: OctetIndex,
    alpha2: SextetIndex,
    betab3: AntiSextetIndex,
    offset: i64,
) -> ChainBuilder {
    ChainBuilder::from_chain(Chain::new_trace(
        offset,
        &[alpha2.into(), a1.into(), betab3.into()],
        2,
    ))
}

fn add_external(mut chains: ChainBuilder, particle_colors: &[Color]) -> ChainBuilder {
    for (i, c) in particle_colors.iter().enumerate() {
        let i = i as i64 + 1;
        match c {
            Color::Octet => {
                let index = OctetIndex(i);
                let tensor = ColorTensor::new_fundamental(
                    index,
                    TripletIndex::new_multi_index(i, MultiIndexLocation::IndexOne),
                    AntiTripletIndex::new_multi_index(i, MultiIndexLocation::IndexTwo),
                );
                chains.append_all(tensor);
                chains.add_index(index);
            }
            Color::Sextet => {
                let index = SextetIndex(i);
                let tensor = ColorTensor::new_sextet(
                    index,
                    AntiTripletIndex::new_multi_index(i, MultiIndexLocation::IndexOne),
                    AntiTripletIndex::new_multi_index(i, MultiIndexLocation::IndexTwo),
                );
                chains.append_all(tensor);
                chains.add_index(index);
            }
            Color::AntiSextet => {
                let index = AntiSextetIndex(i);
                let tensor = ColorTensor::new_anti_sextet(
                    index,
                    TripletIndex::new_multi_index(i, MultiIndexLocation::IndexOne),
                    TripletIndex::new_multi_index(i, MultiIndexLocation::IndexTwo),
                );
                chains.append_all(tensor);
                chains.add_index(index);
            }
            _ => continue,
        }
    }
    chains
}

fn get_external_factors(external: &[Color]) -> f64 {
    let n_octet = external
        .iter()
        .filter(|c| matches!(c, Color::Octet))
        .count();
    // Avoid unnecessary instabilities by excessive multiplication of sqrt(2)
    let mut factor = (INV_TF.pow(n_octet as u32 / 2)) as f64;
    if n_octet % 2 != 0 {
        factor *= INV_SQRT_TF;
    }
    factor
}

fn replace_octet_indices(chains: ChainBuilder) -> ChainBuilder {
    chains.replace_indices(
        &|index| matches!(index, ColorIndex::Octet { .. }),
        &combine_octet_tensors,
    )
}

fn combine_octet_tensors(tensors: &[ColorTensor]) -> ChainBuilder {
    match tensors {
        [ColorTensor::FundamentalRep {
            i2: il, jb3: jl, ..
        }, ColorTensor::FundamentalRep {
            i2: ir, jb3: jr, ..
        }] => ChainBuilder::from_chains(&[
            Chain::new_delta_chain(&[(*il, *jr), (*ir, *jl)], TF),
            Chain::new_delta_chain(&[(*il, *jl), (*ir, *jr)], TF / N_COLORS),
        ]),
        _ => panic!("BUG: Unsupported tensors in octet combination!"),
    }
}

fn replace_sextet_indices(chains: ChainBuilder) -> ChainBuilder {
    chains.replace_indices(
        &|index| matches!(index, ColorIndex::Sextet { .. }),
        &combine_sextet_tensors,
    )
}

fn combine_sextet_tensors(tensors: &[ColorTensor]) -> ChainBuilder {
    match tensors {
        [ColorTensor::AntiSextetClebschGordan { .. }, ColorTensor::SextetClebschGordan { .. }] => {
            // Only explicitly handle one order of tensors
            combine_sextet_tensors(&[tensors[1], tensors[0]])
        }
        [ColorTensor::SextetClebschGordan {
            ib2: il, jb3: jl, ..
        }, ColorTensor::AntiSextetClebschGordan { i2: ir, j3: jr, .. }] => {
            ChainBuilder::from_chains(&[
                Chain::new_delta_chain(&[(*ir, *il), (*jr, *jl)], Rational32::new(1, 2)),
                Chain::new_delta_chain(&[(*ir, *jl), (*jr, *il)], Rational32::new(1, 2)),
            ])
        }
        _ => panic!("BUG: Unsupported tensors in octet combination!"),
    }
}

fn replace_triplet_indices(chains: ChainBuilder) -> ChainBuilder {
    chains.replace_indices(
        &|index| matches!(index, ColorIndex::Triplet { .. }),
        &combine_triplet_tensors,
    )
}

fn combine_triplet_tensors(tensors: &[ColorTensor]) -> ChainBuilder {
    match tensors {
        [ColorTensor::KroneckerTriplet { .. }] => ChainBuilder::empty(),
        [ColorTensor::KroneckerTriplet { i1: il, jb2: jl }, ColorTensor::KroneckerTriplet { i1: ir, jb2: jr }] => {
            if *il == jr.bar() {
                ChainBuilder::from_chain(Chain::new_delta_chain(&[(*ir, *jl)], 1))
            } else {
                ChainBuilder::from_chain(Chain::new_delta_chain(&[(*il, *jr)], 1))
            }
        }
        [ColorTensor::Epsilon { .. }, ColorTensor::KroneckerTriplet { .. }] => {
            // Only handle one order of these
            combine_triplet_tensors(&[tensors[1], tensors[0]])
        }
        [ColorTensor::KroneckerTriplet { i1: il, jb2: jl }, ColorTensor::Epsilon {
            i1: ir1,
            i2: ir2,
            i3: ir3,
        }] => {
            let bar = jl.bar();
            let tensor = if bar == *ir1 {
                ColorTensor::new_epsilon(*il, *ir2, *ir3)
            } else if bar == *ir2 {
                ColorTensor::new_epsilon(*ir1, *il, *ir3)
            } else {
                ColorTensor::new_epsilon(*ir1, *ir2, *il)
            };
            ChainBuilder::from_tensor(tensor)
        }
        [ColorTensor::EpsilonBar { .. }, ColorTensor::KroneckerTriplet { .. }] => {
            // Only handle one order of these
            combine_triplet_tensors(&[tensors[1], tensors[0]])
        }
        [ColorTensor::KroneckerTriplet { i1: il, jb2: jl }, ColorTensor::EpsilonBar {
            ib1: ir1,
            ib2: ir2,
            ib3: ir3,
        }] => {
            let bar = il.bar();
            let tensor = if bar == *ir1 {
                ColorTensor::new_epsilon_bar(*jl, *ir2, *ir3)
            } else if bar == *ir2 {
                ColorTensor::new_epsilon_bar(*ir1, *jl, *ir3)
            } else {
                ColorTensor::new_epsilon_bar(*ir1, *ir2, *jl)
            };
            ChainBuilder::from_tensor(tensor)
        }
        _ => panic!(
            "BUG: Unsupported tensors in triplet combination!: {:?}",
            tensors
        ),
    }
}

fn replace_epsilon_pairs(chains: ChainBuilder) -> ChainBuilder {
    chains.combine_pairs(
        &|t| matches!(t, ColorTensor::Epsilon { .. }),
        &|t| matches!(t, ColorTensor::EpsilonBar { .. }),
        &combine_epsilons,
    )
}

fn combine_epsilons(eps: ColorTensor, bar: ColorTensor) -> ChainBuilder {
    match (eps, bar) {
        (ColorTensor::Epsilon { i1, i2, i3 }, ColorTensor::EpsilonBar { ib1, ib2, ib3 }) => {
            ChainBuilder::from_chains(&[
                Chain::new_delta_chain(&[(i1, ib1), (i2, ib2), (i3, ib3)], 1),
                Chain::new_delta_chain(&[(i1, ib2), (i2, ib3), (i3, ib1)], 1),
                Chain::new_delta_chain(&[(i1, ib3), (i2, ib1), (i3, ib2)], 1),
                Chain::new_delta_chain(&[(i1, ib2), (i2, ib1), (i3, ib3)], -1),
                Chain::new_delta_chain(&[(i1, ib1), (i2, ib3), (i3, ib2)], -1),
                Chain::new_delta_chain(&[(i1, ib3), (i2, ib2), (i3, ib1)], -1),
            ])
        }
        _ => panic!("BUG: Invalid inputs to combine epsilons!"),
    }
}

#[cfg(test)]
mod test {
    use super::VertexFlows;
    use crate::ufo::{Color, UfoMath};

    #[test]
    fn qqg() {
        let data = UfoMath("T(3,1,2)".to_string());
        let particle_colors = &[Color::Triplet, Color::AntiTriplet, Color::Octet];
        VertexFlows::from_structure(&data, particle_colors);
    }

    #[test]
    fn ggg() {
        let data = UfoMath("f(1,2,3)".to_string());
        let particle_colors = &[Color::Octet, Color::Octet, Color::Octet];
        VertexFlows::from_structure(&data, particle_colors);
    }

    #[test]
    fn gggg() {
        let data = UfoMath("f(1,2,-1)*f(3,4,-1)".to_string());
        let particle_colors = &[Color::Octet, Color::Octet, Color::Octet, Color::Octet];
        VertexFlows::from_structure(&data, particle_colors);
    }
}
