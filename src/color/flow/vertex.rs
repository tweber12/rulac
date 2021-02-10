use crate::color::flow::chain::{Chain, ChainBuilder};
use crate::color::tensor::{
    AntiSextetIndex, AntiTripletIndex, ColorIndex, ColorTensor, OctetIndex, SextetIndex,
    TripletIndex, UndefinedIndex,
};
use crate::ufo::{Color, UfoMath};

const TF: f64 = 0.5;
const N_COLORS: u8 = 3;

const OFFSET_LEFT: i64 = 1000;
const OFFSET_RIGHT: i64 = 2000;

pub struct VertexFlow {}
impl VertexFlow {
    pub fn from_structure(structure: &UfoMath, external: &[Color]) -> VertexFlow {
        let chains = ChainBuilder::from_expr(structure, external);
        let chains = replace_tensors_initial(chains, external);
        let chains = add_external(chains, external);
        let chains = replace_octet_indices(chains);
        println!("{:?}", chains);
        unimplemented!();
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

fn add_external(mut chains: ChainBuilder, particle_colors: &[Color]) -> ChainBuilder {
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
                chains.append_all(tensor);
                for chain in chains.chains.iter_mut() {
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
                chains.append_all(tensor);
                for chain in chains.chains.iter_mut() {
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
                chains.append_all(tensor);
                for chain in chains.chains.iter_mut() {
                    chain.sextet_indices.push(SextetIndex(i));
                }
            }
            _ => continue,
        }
    }
    chains
}

fn combine_octet_tensors(tensors: &[ColorTensor]) -> ChainBuilder {
    match tensors {
        [ColorTensor::FundamentalRep {
            i2: il, jb3: jl, ..
        }, ColorTensor::FundamentalRep {
            i2: ir, jb3: jr, ..
        }] => ChainBuilder {
            chains: vec![
                Chain::new_delta_chain(&[(*il, *jr), (*ir, *jl)], TF),
                Chain::new_delta_chain(&[(*il, *jl), (*ir, *jr)], -TF / (N_COLORS as f64)),
            ],
        },
        _ => panic!("BUG: Unsupported tensors in octet combination!"),
    }
}

fn replace_octet_indices(chains: ChainBuilder) -> ChainBuilder {
    chains.replace_indices(
        &|index| matches!(index, ColorIndex::Octet {..}),
        &combine_octet_tensors,
    )
}

#[cfg(test)]
mod test {
    use super::VertexFlow;
    use crate::ufo::{Color, UfoMath};

    #[test]
    fn qqg() {
        let data = UfoMath("T(3,1,2)".to_string());
        let particle_colors = &[Color::Triplet, Color::AntiTriplet, Color::Octet];
        VertexFlow::from_structure(&data, particle_colors);
    }

    #[test]
    fn ggg() {
        let data = UfoMath("f(1,2,3)".to_string());
        let particle_colors = &[Color::Octet, Color::Octet, Color::Octet];
        VertexFlow::from_structure(&data, particle_colors);
    }
}
