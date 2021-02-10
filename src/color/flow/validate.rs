use crate::color::tensor::{
    AntiSextetIndex, AntiTripletIndex, ColorTensor, OctetIndex, SextetIndex, TripletIndex,
};
use crate::ufo::Color;
use std::collections::HashMap;

pub fn validate_colors(
    tensors: &[ColorTensor],
    particle_colors: &[Color],
) -> Result<(), ValidationError> {
    let mut internal: HashMap<i64, Color> = HashMap::new();
    for tensor in tensors {
        validate_tensor_colors(tensor, particle_colors, &mut internal)?;
    }
    Ok(())
}

pub fn validate_tensor_colors(
    tensor: &ColorTensor,
    particle_colors: &[Color],
    internal_colors: &mut HashMap<i64, Color>,
) -> Result<(), ValidationError> {
    match tensor {
        ColorTensor::FundamentalRep { a1, i2, jb3 } => {
            validate_octet(a1, particle_colors, internal_colors)?;
            validate_triplet(i2, particle_colors, internal_colors)?;
            validate_anti_triplet(jb3, particle_colors, internal_colors)?;
        }
        ColorTensor::Epsilon { i1, i2, i3 } => {
            validate_triplet(i1, particle_colors, internal_colors)?;
            validate_triplet(i2, particle_colors, internal_colors)?;
            validate_triplet(i3, particle_colors, internal_colors)?;
        }
        ColorTensor::EpsilonBar { ib1, ib2, ib3 } => {
            validate_anti_triplet(ib1, particle_colors, internal_colors)?;
            validate_anti_triplet(ib2, particle_colors, internal_colors)?;
            validate_anti_triplet(ib3, particle_colors, internal_colors)?;
        }
        ColorTensor::StructureConstant { a1, a2, a3 } => {
            validate_octet(a1, particle_colors, internal_colors)?;
            validate_octet(a2, particle_colors, internal_colors)?;
            validate_octet(a3, particle_colors, internal_colors)?;
        }
        ColorTensor::SymmetricTensor { a1, a2, a3 } => {
            validate_octet(a1, particle_colors, internal_colors)?;
            validate_octet(a2, particle_colors, internal_colors)?;
            validate_octet(a3, particle_colors, internal_colors)?;
        }
        ColorTensor::SextetRep { a1, alpha2, betab3 } => {
            validate_octet(a1, particle_colors, internal_colors)?;
            validate_sextet(alpha2, particle_colors, internal_colors)?;
            validate_anti_sextet(betab3, particle_colors, internal_colors)?;
        }
        ColorTensor::SextetClebschGordan { alpha1, ib2, jb3 } => {
            validate_sextet(alpha1, particle_colors, internal_colors)?;
            validate_anti_triplet(ib2, particle_colors, internal_colors)?;
            validate_anti_triplet(jb3, particle_colors, internal_colors)?;
        }
        ColorTensor::AntiSextetClebschGordan { alphab1, i2, j3 } => {
            validate_anti_sextet(alphab1, particle_colors, internal_colors)?;
            validate_triplet(i2, particle_colors, internal_colors)?;
            validate_triplet(j3, particle_colors, internal_colors)?;
        }
        _ => (),
    }
    Ok(())
}

#[derive(Debug)]
pub struct ValidationError {
    particle_index: i64,
    actual: Color,
    expected: Color,
}

fn validate_triplet(
    index: &TripletIndex,
    particle_colors: &[Color],
    internal_colors: &mut HashMap<i64, Color>,
) -> Result<(), ValidationError> {
    let TripletIndex(i) = *index;
    validate_color_indices(i, particle_colors, internal_colors, Color::Triplet)
}

fn validate_anti_triplet(
    index: &AntiTripletIndex,
    particle_colors: &[Color],
    internal_colors: &mut HashMap<i64, Color>,
) -> Result<(), ValidationError> {
    let AntiTripletIndex(i) = *index;
    validate_color_indices(i, particle_colors, internal_colors, Color::AntiTriplet)
}

fn validate_sextet(
    index: &SextetIndex,
    particle_colors: &[Color],
    internal_colors: &mut HashMap<i64, Color>,
) -> Result<(), ValidationError> {
    let SextetIndex(i) = *index;
    validate_color_indices(i, particle_colors, internal_colors, Color::Sextet)
}

fn validate_anti_sextet(
    index: &AntiSextetIndex,
    particle_colors: &[Color],
    internal_colors: &mut HashMap<i64, Color>,
) -> Result<(), ValidationError> {
    let AntiSextetIndex(i) = *index;
    validate_color_indices(i, particle_colors, internal_colors, Color::AntiSextet)
}

fn validate_octet(
    index: &OctetIndex,
    particle_colors: &[Color],
    internal_colors: &mut HashMap<i64, Color>,
) -> Result<(), ValidationError> {
    let OctetIndex(i) = *index;
    validate_color_indices(i, particle_colors, internal_colors, Color::Octet)
}

fn validate_color_indices(
    index: i64,
    particle_colors: &[Color],
    internal_colors: &mut HashMap<i64, Color>,
    expected: Color,
) -> Result<(), ValidationError> {
    if index > 0 {
        // The index corresponds to an external particle. Check that its color
        // representation matches the expected one for the tensor.
        let actual = particle_colors[index as usize - 1];
        if actual != expected {
            return Err(ValidationError {
                particle_index: index,
                actual,
                expected,
            });
        }
    } else {
        // The index is summed internally.
        match internal_colors.get(&index) {
            Some(&col) => {
                // The other index has already been seen. Check that it is
                // the inverse of the expected one, since for example a
                // 3 index is contracted with a 3_bar, not a 3.
                if col != expected.bar() {
                    return Err(ValidationError {
                        particle_index: index,
                        actual: col,
                        expected,
                    });
                }
            }
            None => {
                // The other index has not been seen yet, so store this one
                // together with its color.
                internal_colors.insert(index, expected);
            }
        }
    }
    Ok(())
}
