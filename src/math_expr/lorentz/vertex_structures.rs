use crate::math_expr;
use crate::math_expr::lorentz;
use crate::math_expr::lorentz::propagators::{BasicPropagator, Propagators};
use crate::math_expr::lorentz::tensor_components::SpinTensorComponents;
use crate::math_expr::lorentz::{
    ExternalComponent, IndexIter, Indices, LorentzExpr, LorentzIndex, SpinIndex, SpinorIndex,
};
use crate::math_expr::parse::{parse_math, ParseError};
use crate::math_expr::Number;
use crate::ufo;
use num_traits::Zero;

#[derive(Clone, Debug, PartialEq)]
pub enum VertexStructure {
    Scalar {
        factor: LorentzExpr,
        structure: LorentzExpr,
    },
    Vector {
        factor: LorentzExpr,
        structure: [LorentzExpr; 4],
    },
    Tensor {
        factor: LorentzExpr,
        structure: Box<[[LorentzExpr; 4]; 4]>,
    },
}

pub struct StructureBuilder<'a> {
    components: &'a SpinTensorComponents,
    propagators: &'a Propagators,
}
impl<'a> StructureBuilder<'a> {
    pub fn new(
        components: &'a SpinTensorComponents,
        propagators: &'a Propagators,
    ) -> StructureBuilder<'a> {
        StructureBuilder {
            components,
            propagators,
        }
    }
    pub fn amputated_structure(
        &mut self,
        lorentz: &ufo::Lorentz,
        external: usize,
    ) -> Result<VertexStructure, ParseError> {
        assert!(external < lorentz.spins.len());
        match lorentz.spins[external] {
            1 => self.amputated_scalar(lorentz, external),
            2 | 3 => self.amputated_vector(lorentz, external),
            4 | 5 => self.amputated_tensor(lorentz, external),
            0 => panic!("BUG: Spin+1 = 0 is nonsensical, so something went terribly wrong!"),
            -1 => unimplemented!("Ghosts are currently not supported"),
            s => unimplemented!("Only spins up to 2 are supported: {}", s),
        }
    }

    pub fn complete_structure(
        &mut self,
        lorentz: &ufo::Lorentz,
        external: usize,
        anti_particle: bool,
        mass: f64,
    ) -> Result<VertexStructure, ParseError> {
        let amputated = self.amputated_structure(lorentz, external)?;
        let propagator = match lorentz.spins[external] {
            1 => &self.propagators.spin_zero(mass),
            2 => self.propagators.spin_one_half(mass),
            3 => self.propagators.spin_one(mass),
            4 => self.propagators.spin_three_half(mass),
            5 => self.propagators.spin_two(mass),
            _ => unimplemented!("Unsupported spin"),
        };
        let propagator = if anti_particle {
            &propagator.outgoing
        } else {
            &propagator.incoming
        };
        let structure = match amputated {
            VertexStructure::Scalar { factor, structure } => {
                self.add_propagator_scalar(&factor, &structure, propagator)
            }
            VertexStructure::Vector { factor, structure } => self.add_propagator_vector(
                &factor,
                &structure,
                propagator,
                lorentz.spins[external],
                !anti_particle,
            ),
            VertexStructure::Tensor { factor, structure } => self.add_propagator_tensor(
                &factor,
                &structure,
                propagator,
                lorentz.spins[external],
                !anti_particle,
            ),
        };
        Ok(structure)
    }

    fn amputated_scalar(
        &self,
        lorentz: &ufo::Lorentz,
        external: usize,
    ) -> Result<VertexStructure, ParseError> {
        let ufo::UfoMath(ref str) = lorentz.structure;
        let expr = parse_math(str)?;
        let mut external_indices = Indices::new();
        let factor = amputated_factor(&lorentz.spins, external);
        let structure = self.eval_component(&expr, &lorentz.spins, external, &mut external_indices);
        Ok(VertexStructure::Scalar { factor, structure })
    }

    fn amputated_vector(
        &self,
        lorentz: &ufo::Lorentz,
        external: usize,
    ) -> Result<VertexStructure, ParseError> {
        let ufo::UfoMath(ref str) = lorentz.structure;
        let expr = parse_math(str)?;
        let factor = amputated_factor(&lorentz.spins, external);
        let mut structure: [LorentzExpr; 4] = Default::default();
        for (mut index_values, values) in
            IndexIter::new(&indices_for_spin(lorentz.spins[external], external))
        {
            assert_eq!(values.len(), 1);
            let component = self.eval_component(&expr, &lorentz.spins, external, &mut index_values);
            structure[values[0] as usize] = component;
        }
        Ok(VertexStructure::Vector { factor, structure })
    }

    fn amputated_tensor(
        &self,
        lorentz: &ufo::Lorentz,
        external: usize,
    ) -> Result<VertexStructure, ParseError> {
        let ufo::UfoMath(ref str) = lorentz.structure;
        let expr = parse_math(str)?;
        let factor = amputated_factor(&lorentz.spins, external);
        let mut structure: [[LorentzExpr; 4]; 4] = Default::default();
        for (mut index_values, values) in
            IndexIter::new(&indices_for_spin(lorentz.spins[external], external))
        {
            assert_eq!(values.len(), 2);
            let component = self.eval_component(&expr, &lorentz.spins, external, &mut index_values);
            structure[values[0] as usize][values[1] as usize] = component;
        }
        Ok(VertexStructure::Tensor {
            factor,
            structure: Box::new(structure),
        })
    }

    fn eval_component(
        &self,
        structure: &LorentzExpr,
        spins: &[i64],
        external: usize,
        external_indices: &mut Indices,
    ) -> LorentzExpr {
        let summed_over = setup_summed_indices(spins, external);
        let mut expr = LorentzExpr::Number {
            value: Number::zero(),
        };
        for (mut indices, _) in IndexIter::with_external(&summed_over, external_indices.clone()) {
            let mut summand = lorentz::expand_sums(structure, self.components, &mut indices);
            for component in get_external_components(spins, external, &indices).into_iter() {
                summand = summand * LorentzExpr::ExternalComponent { component };
            }
            expr = expr + summand
        }
        expr
    }

    fn add_propagator_scalar(
        &mut self,
        factor: &LorentzExpr,
        structure: &LorentzExpr,
        propagator: &BasicPropagator,
    ) -> VertexStructure {
        VertexStructure::Scalar {
            factor: (factor.clone()) / (propagator.denominator.clone()),
            structure: (structure.clone()) * (propagator.numerator.clone()),
        }
    }

    fn add_propagator_vector(
        &mut self,
        factor: &LorentzExpr,
        amputated_structure: &[LorentzExpr; 4],
        propagator: &BasicPropagator,
        spin: i64,
        incoming: bool,
    ) -> VertexStructure {
        let (indices_in, indices_out) = if incoming {
            (indices_for_spin(spin, 0), indices_for_spin(spin, 1))
        } else {
            (indices_for_spin(spin, 1), indices_for_spin(spin, 0))
        };
        let mut structure: [LorentzExpr; 4] = Default::default();
        for (index_values_out, values_out) in IndexIter::new(&indices_out) {
            for (mut index_values_in, values_in) in
                IndexIter::with_external(&indices_in, index_values_out)
            {
                let mut summand = lorentz::expand_sums(
                    &propagator.numerator,
                    &self.components,
                    &mut index_values_in,
                );
                summand = summand * amputated_structure[values_in[0] as usize].clone();
                structure[values_out[0] as usize] =
                    structure[values_out[0] as usize].clone() + summand;
            }
        }
        VertexStructure::Vector {
            structure,
            factor: (factor.clone()) / (propagator.denominator.clone()),
        }
    }

    fn add_propagator_tensor(
        &mut self,
        factor: &LorentzExpr,
        amputated_structure: &[[LorentzExpr; 4]; 4],
        propagator: &BasicPropagator,
        spin: i64,
        incoming: bool,
    ) -> VertexStructure {
        let (indices_in, indices_out) = if incoming {
            (indices_for_spin(spin, 0), indices_for_spin(spin, 1))
        } else {
            (indices_for_spin(spin, 1), indices_for_spin(spin, 0))
        };
        let mut structure: [[LorentzExpr; 4]; 4] = Default::default();
        for (index_values_out, values_out) in IndexIter::new(&indices_out) {
            for (mut index_values_in, values_in) in
                IndexIter::with_external(&indices_in, index_values_out)
            {
                let mut summand = lorentz::expand_sums(
                    &propagator.numerator,
                    &self.components,
                    &mut index_values_in,
                );
                summand = summand
                    * amputated_structure[values_in[0] as usize][values_in[1] as usize].clone();
                structure[values_out[0] as usize][values_out[1] as usize] =
                    structure[values_out[0] as usize][values_out[1] as usize].clone() + summand;
            }
        }
        VertexStructure::Tensor {
            structure: Box::new(structure),
            factor: (factor.clone()) / (propagator.denominator.clone()),
        }
    }
}

fn amputated_factor(spins: &[i64], left_out: usize) -> LorentzExpr {
    let mut factor = LorentzExpr::Number {
        value: Number::from(1),
    };
    for (i, &s) in spins.iter().enumerate().skip(left_out) {
        if s != 1 {
            continue;
        }
        factor = LorentzExpr::BinaryOp {
            operator: math_expr::BinaryOperator::Mul,
            left: Box::new(factor),
            right: Box::new(LorentzExpr::ExternalComponent {
                component: ExternalComponent::Scalar(i + 1),
            }),
        }
    }
    factor
}

fn get_external_components(
    spins: &[i64],
    external: usize,
    index_values: &Indices,
) -> Vec<ExternalComponent> {
    let mut components = Vec::new();
    for (particle, &spin) in spins.iter().enumerate().skip(external) {
        let component = match &*indices_for_spin(spin, particle) {
            // There are no external components for spin zero,
            //their contribution is added to the overall factor
            [] => continue,
            [i] => ExternalComponent::Vector(particle, index_values[*i]),
            [i, j] => ExternalComponent::Tensor(particle, index_values[*i], index_values[*j]),
            x => panic!("BUG: Object with more than two indices returned!: {:?}", x),
        };
        components.push(component)
    }
    components
}

fn setup_summed_indices(spins: &[i64], external: usize) -> Vec<SpinIndex> {
    let mut indices = Vec::new();
    for (i, &s) in spins.iter().enumerate() {
        if i == external {
            continue;
        }
        indices.extend(indices_for_spin(s, i));
    }
    indices
}

fn indices_for_spin(spin: i64, particle: usize) -> Vec<SpinIndex> {
    let i = particle as i64 + 1;
    match spin {
        1 => Vec::new(),
        2 => vec![SpinorIndex(i).into()],
        3 => vec![LorentzIndex(i).into()],
        4 => vec![LorentzIndex(i + 1000).into(), SpinorIndex(i + 2000).into()],
        5 => vec![LorentzIndex(i + 1000).into(), LorentzIndex(i + 2000).into()],
        -1 => unimplemented!("Ghosts are currently not supported!"),
        _ => unimplemented!("Only spins up to 2 are currently supported!"),
    }
}

#[cfg(test)]
mod test {
    use crate::math_expr::lorentz::propagators::Propagators;
    use crate::math_expr::lorentz::tensor_components::SpinTensorComponents;
    use crate::ufo::UfoModel;

    #[test]
    fn sm_mg5_amputated() {
        let propagators = Propagators::load("models/common/propagators.toml").unwrap();
        let components = SpinTensorComponents::load("models/common/spin_structures.toml").unwrap();
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let mut builder = super::StructureBuilder::new(&components, &propagators);
        for structure in model.lorentz_structures.values() {
            if structure.spins.contains(&-1) {
                continue;
            }
            for (i, _) in structure.spins.iter().enumerate() {
                builder.amputated_structure(&structure, i).unwrap();
            }
        }
    }
    #[test]

    fn sm_mg5_full() {
        let propagators = Propagators::load("models/common/propagators.toml").unwrap();
        let components = SpinTensorComponents::load("models/common/spin_structures.toml").unwrap();
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        let mut builder = super::StructureBuilder::new(&components, &propagators);
        for structure in model.lorentz_structures.values() {
            if structure.spins.contains(&-1) {
                continue;
            }
            for (i, _) in structure.spins.iter().enumerate() {
                builder
                    .complete_structure(&structure, i, false, 0.0)
                    .unwrap();
                builder
                    .complete_structure(&structure, i, false, 10.0)
                    .unwrap();
                builder
                    .complete_structure(&structure, i, true, 0.0)
                    .unwrap();
                builder
                    .complete_structure(&structure, i, true, 10.0)
                    .unwrap();
            }
        }
    }
}
