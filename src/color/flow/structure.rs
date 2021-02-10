use crate::color::tensor::{
    AntiSextetIndex, ColorExpr, ColorIndex, ColorTensor, OctetIndex, SextetIndex, TripletIndex,
    UndefinedIndex,
};
use crate::color::validate;
use crate::math_expr::parse::{parse_math, ParseError};
use crate::math_expr::{BinaryOperator, Number};
use crate::ufo::{Color, UfoMath, UfoModel, Vertex};
use std::collections::HashMap;

pub struct ColorStructures {
    structures: HashMap<UfoMath, HashMap<Vec<Color>, FlowString>>,
    vertices: HashMap<String, Vec<FlowString>>,
}
impl ColorStructures {
    pub fn new(model: &UfoModel) -> ColorStructures {
        let mut structures = ColorStructures {
            structures: HashMap::new(),
            vertices: HashMap::new(),
        };
        for vertex in model.vertices.iter() {
            let particle_colors: Vec<_> = vertex
                .particles
                .iter()
                .map(|p| model.particles[p].color)
                .collect();
            structures.add_structures(&vertex.color, particle_colors);
        }
        structures
    }
    fn add_structures(&mut self, structures: &[UfoMath], particle_colors: Vec<Color>) {
        for structure in structures {
            self.structures
                .entry(structure.clone())
                .or_default()
                .entry(particle_colors.clone())
                .or_insert_with(|| FlowString::new(structure, &particle_colors).unwrap());
        }
    }
}

pub struct FlowString {}
impl FlowString {
    fn new(structure: &UfoMath, external: &[Color]) -> Result<FlowString, ColorError> {
        unimplemented!();
    }
}

/// From looking at MG5_aMC v2.8.3, the color structure contains nothing but
/// products of color tensors.
/// (see `models/import_ufo.py` line 1340 and 1474ff)
struct Builder {
    /// A string of color tensors that are implicitly multiplied
    tensors: Vec<ColorTensor>,
    triplet_indices: Vec<TripletIndex>,
    sextet_indices: Vec<SextetIndex>,
    octet_indices: Vec<OctetIndex>,
    offset: i64,
}
impl Builder {
    fn new(structure: &UfoMath, external: &[Color]) -> Result<Builder, ColorError> {
        let UfoMath(structure) = structure;
        let expr: ColorExpr = parse_math(structure)?;
        let mut structure = Builder {
            tensors: Vec::new(),
            triplet_indices: Vec::new(),
            sextet_indices: Vec::new(),
            octet_indices: Vec::new(),
            offset: -10000,
        };
        structure.add_expr(expr)?;
        structure.validate(external)?;
        Ok(structure)
    }
    fn add_expr(&mut self, expr: ColorExpr) -> Result<(), ColorError> {
        match expr {
            ColorExpr::BinaryOp {
                operator,
                left,
                right,
            } => {
                if operator != BinaryOperator::Mul {
                    return Err(ColorError::UnsupportedOperator);
                }
                self.add_expr(*left)?;
                self.add_expr(*right)?;
            }
            ColorExpr::Tensor { tensor } => self.tensors.push(tensor),
            ColorExpr::Sum { expr, index } => {
                match index {
                    ColorIndex::Triplet { index } => self.triplet_indices.push(index),
                    ColorIndex::Sextet { index } => self.sextet_indices.push(index),
                    ColorIndex::Octet { index } => self.octet_indices.push(index),
                    _ => panic!("BUG: The color indices have not been normalized!"),
                }
                self.add_expr(*expr)?;
            }
            ColorExpr::Number {
                value: Number::Integer(i),
            } => {
                if i != 1 {
                    return Err(ColorError::UnsupportedNumber(i));
                }
                // Do nothing in this case, the color structure is trivial
            }
            _ => return Err(ColorError::UnsupportedExpression(expr)),
        }
        Ok(())
    }

    fn validate(&self, particle_colors: &[Color]) -> Result<(), ColorError> {
        validate::validate_colors(&self.tensors, particle_colors).map_err(|err| err.into())
    }

    fn simplify(&mut self) {}

    fn initial_replacements(mut self, particle_colors: &[Color]) -> Result<Builder, ColorError> {
        let to_replace = self.tensors;
        self.tensors = Vec::new();
        for tensor in to_replace {
            match tensor {
                ColorTensor::KroneckerDelta { i1, jb2 } => {
                    self.replace_identity(i1, jb2, particle_colors)?
                }
                // ColorTensor::StructureConstant { a1, a2, a3 } => self.add_trace(&[a1, a2, a3]),
                _ => self.tensors.push(tensor),
            }
        }
        Ok(self)
    }

    fn replace_identity(
        &mut self,
        i1: UndefinedIndex,
        jb2: UndefinedIndex,
        particle_colors: &[Color],
    ) -> Result<(), ColorError> {
        let UndefinedIndex(i1) = i1;
        let UndefinedIndex(i2) = jb2;
        if i1 < 0 || i2 < 0 {
            return Err(ColorError::ContractedIdentity);
        }
        match (
            particle_colors[i1 as usize - 1],
            particle_colors[i2 as usize - 1],
        ) {
            (Color::Triplet, Color::AntiTriplet) => self
                .tensors
                .push(ColorTensor::new_kronecker_triplet(i1, i2)),
            (Color::AntiTriplet, Color::Triplet) => self
                .tensors
                .push(ColorTensor::new_kronecker_triplet(i2, i1)),
            (Color::Octet, Color::Octet) => {
                self.add_trace(&[OctetIndex(i1).into(), OctetIndex(i2).into()])
            }
            (Color::Sextet, Color::AntiSextet) => {
                self.add_trace(&[SextetIndex(i1).into(), AntiSextetIndex(i2).into()])
            }
            (Color::AntiSextet, Color::Sextet) => {
                self.add_trace(&[AntiSextetIndex(i1).into(), SextetIndex(i2).into()])
            }
            (a, b) => return Err(ColorError::UnsupportedIdentity(a, b)),
        }
        Ok(())
    }

    fn add_trace(&mut self, indices: &[ColorIndex]) {
        let start: TripletIndex = self.summation_index();
        let sum = start;
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
        }
    }
    fn summation_index<T>(&mut self) -> T
    where
        T: From<i64>,
    {
        let next = self.offset;
        self.offset -= 1;
        next.into()
    }
}

#[derive(Debug)]
pub enum ColorError {
    Parse(ParseError),
    UnsupportedOperator,
    ContractedIdentity,
    UnsupportedNumber(i64),
    UnsupportedExpression(ColorExpr),
    InvalidModel(validate::ValidationError),
    UnsupportedIdentity(Color, Color),
}
impl From<ParseError> for ColorError {
    fn from(err: ParseError) -> ColorError {
        ColorError::Parse(err)
    }
}
impl From<validate::ValidationError> for ColorError {
    fn from(err: validate::ValidationError) -> ColorError {
        ColorError::InvalidModel(err)
    }
}

#[cfg(test)]
mod test {
    use super::ColorStructures;
    use crate::ufo::{UfoMath, UfoModel};

    #[test]
    fn load_sm_mg5() {
        let model = UfoModel::load("tests/models_json/sm_mg5").unwrap();
        ColorStructures::new(&model);
    }

    #[test]
    fn load_sm_nlo() {
        let model = UfoModel::load("tests/models_json/sm_NLO").unwrap();
        ColorStructures::new(&model);
    }
}
