use crate::math_expr::lorentz::{expand_sums, IndexIter, LorentzIndex, SpinorIndex};
use crate::math_expr::{MathExpr, Number, TensorIndex};
use num_complex::Complex64;
use num_traits::Zero;
use serde::Deserialize;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::Read;
use std::path;

pub struct SpinTensorComponents {
    pub metric: TensorComponents2,
    pub identity: TensorComponents2,
    pub gamma: TensorComponents3,
    pub epsilon: TensorComponents4,
    pub charge_conjugation: TensorComponents2,
    pub gamma5: TensorComponents2,
    pub proj_m: TensorComponents2,
    pub proj_p: TensorComponents2,
    pub sigma: TensorComponents4,
}
impl SpinTensorComponents {
    pub fn load<P: AsRef<path::Path>>(
        path: P,
    ) -> Result<SpinTensorComponents, SpinComponentsError> {
        let mut contents = Vec::new();
        fs::File::open(path)?.read_to_end(&mut contents)?;
        let internal: SpinTensorComponentsInternal = toml::from_slice(&contents)?;
        let identity = SpinTensorComponents::compute_identity();
        let epsilon = SpinTensorComponents::compute_epsilon(internal.epsilon_sign);
        let mut components = SpinTensorComponents {
            metric: SpinTensorComponents::compute_metric(&internal.metric),
            identity,
            gamma: SpinTensorComponents::compute_gamma(&internal.gamma),
            epsilon,
            charge_conjugation: SpinTensorComponents::compute_charge_conjugation(
                &internal.charge_conjugation,
            ),
            gamma5: TensorComponents2::new(),
            proj_m: TensorComponents2::new(),
            proj_p: TensorComponents2::new(),
            sigma: TensorComponents4::new(),
        };
        components.compute_gamma5(&internal.gamma5);
        components.compute_projm(&internal.proj_m);
        components.compute_projp(&internal.proj_p);
        components.compute_sigma(&internal.sigma);
        Ok(components)
    }

    // Internal functions to derive the remaining lorentz structures
    fn compute_metric(diagonal: &[Number]) -> TensorComponents2 {
        let mut metric = TensorComponents2::new();
        for (i, d) in LorentzIndex::range().zip(diagonal.iter()) {
            metric.insert(i, i, *d);
        }
        metric
    }
    fn compute_gamma(matrix: &[[[Number; 4]; 4]]) -> TensorComponents3 {
        let mut gamma = TensorComponents3::new();
        for mu in LorentzIndex::range() {
            for i in SpinorIndex::range() {
                for j in SpinorIndex::range() {
                    let value = matrix[mu as usize][i as usize][j as usize];
                    gamma.insert(mu, i, j, value);
                }
            }
        }
        gamma
    }
    fn compute_charge_conjugation(matrix: &[[Number; 4]]) -> TensorComponents2 {
        let mut charge_conjugation = TensorComponents2::new();
        for i in SpinorIndex::range() {
            for j in SpinorIndex::range() {
                let value = matrix[i as usize][j as usize];
                charge_conjugation.insert(i, j, value);
            }
        }
        charge_conjugation
    }
    fn compute_identity() -> TensorComponents2 {
        let mut components = TensorComponents2::new();
        for i in SpinorIndex::range() {
            components.insert(i, i, Complex64::new(1f64, 0f64));
        }
        components
    }
    fn compute_gamma5(&mut self, relation: &SpinTensorRelation) {
        // The lorentz indices are NOT summed over but fixed
        for (mut indices, values) in IndexIter::new_split(&[], &relation.spinor) {
            for (i, &index) in relation.lorentz.iter().enumerate() {
                indices.set_index(index, i as u8);
            }
            let expr = expand_sums(&relation.expr, self, &mut indices).constant_propagation();
            match expr.extract_number() {
                None => panic!(
                    "BUG: Failed to compute ({},{}) component of gamma5: {:?}",
                    values[0], values[1], expr
                ),
                Some(e) => self.gamma5.insert_values(&values, e),
            }
        }
    }
    fn compute_projm(&mut self, relation: &SpinTensorRelation) {
        for (mut indices, values) in relation.iter() {
            let expr = expand_sums(&relation.expr, self, &mut indices).constant_propagation();
            match expr.extract_number() {
                None => panic!(
                    "BUG: Failed to compute ({},{}) component of ProjM: {:?}",
                    values[0], values[1], expr
                ),
                Some(e) => self.proj_m.insert_values(&values, e),
            }
        }
    }
    fn compute_projp(&mut self, relation: &SpinTensorRelation) {
        for (mut indices, values) in relation.iter() {
            let expr = expand_sums(&relation.expr, self, &mut indices).constant_propagation();
            match expr.extract_number() {
                None => panic!(
                    "BUG: Failed to compute ({},{}) component of ProjP: {:?}",
                    values[0], values[1], expr
                ),
                Some(e) => self.proj_p.insert_values(&values, e),
            }
        }
    }
    fn compute_sigma(&mut self, relation: &SpinTensorRelation) {
        for (mut indices, values) in relation.iter() {
            let expr = expand_sums(&relation.expr, self, &mut indices).constant_propagation();
            match expr.extract_number() {
                None => panic!(
                    "BUG: Failed to compute ({},{},{},{}) component of sigma: {:?}",
                    values[0], values[1], values[2], values[3], expr
                ),
                Some(e) => self.sigma.insert_values(&values, e),
            }
        }
    }
    fn compute_epsilon(sign: Number) -> TensorComponents4 {
        let mut components = TensorComponents4::new();
        let mut start: Vec<u8> = LorentzIndex::range().collect();
        permutohedron::heap_recursive(&mut start, |permutation| {
            let even = is_permutation_even(permutation);
            let re = if even { sign } else { -sign };
            components.insert(
                permutation[0],
                permutation[1],
                permutation[2],
                permutation[3],
                re,
            );
        });
        components
    }
}

fn is_permutation_even(permutation: &[u8]) -> bool {
    // This just checks every possible pair if it needs to be swapped
    // That's not the most efficient algorithm but it's simple and since we only ever consider
    // vectors going up to length 4 any performance gains from more complicated methods should
    // be negligible anyway
    let mut even = true;
    for (i, &v) in permutation.iter().enumerate() {
        for (j, &w) in permutation.iter().enumerate() {
            if j <= i {
                continue;
            }
            if v > w {
                even = !even;
            }
        }
    }
    even
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
struct SpinTensorComponentsInternal {
    epsilon_sign: Number,
    metric: [Number; 4],
    gamma: [[[Number; 4]; 4]; 4],
    charge_conjugation: [[Number; 4]; 4],
    gamma5: SpinTensorRelation,
    proj_p: SpinTensorRelation,
    proj_m: SpinTensorRelation,
    sigma: SpinTensorRelation,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
struct SpinTensorRelation {
    #[serde(deserialize_with = "crate::math_expr::parse::deserialize_lorentz_expr")]
    expr: MathExpr,
    #[serde(default)]
    lorentz: Vec<LorentzIndex>,
    #[serde(default)]
    spinor: Vec<SpinorIndex>,
}
impl SpinTensorRelation {
    fn iter(&self) -> IndexIter {
        IndexIter::new_split(&self.lorentz, &self.spinor)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct TensorComponents<T>
where
    T: std::hash::Hash + Eq,
{
    components: HashMap<T, Number>,
}
impl<T> TensorComponents<T>
where
    T: std::hash::Hash + Eq,
{
    fn new() -> TensorComponents<T> {
        TensorComponents {
            components: HashMap::new(),
        }
    }
    pub fn get(&self, index: &T) -> Number {
        *self.components.get(index).unwrap_or(&Number::zero())
    }
    fn insert<V: Into<Number> + Zero>(&mut self, index: T, value: V) {
        if !value.is_zero() {
            self.components.insert(index, value.into());
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TensorComponents2 {
    components: TensorComponents<(u8, u8)>,
}
impl TensorComponents2 {
    fn new() -> TensorComponents2 {
        TensorComponents2 {
            components: TensorComponents::new(),
        }
    }
    pub fn get(&self, i1: u8, i2: u8) -> Number {
        self.components.get(&(i1, i2))
    }
    fn insert<T: Into<Number> + Zero>(&mut self, i1: u8, i2: u8, value: T) {
        self.components.insert((i1, i2), value);
    }
    fn insert_values<T: Into<Number> + Zero>(&mut self, values: &[u8], value: T) {
        assert_eq!(values.len(), 2);
        self.components.insert((values[0], values[1]), value);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TensorComponents3 {
    components: TensorComponents<(u8, u8, u8)>,
}
impl TensorComponents3 {
    fn new() -> TensorComponents3 {
        TensorComponents3 {
            components: TensorComponents::new(),
        }
    }
    pub fn get(&self, i1: u8, i2: u8, i3: u8) -> Number {
        self.components.get(&(i1, i2, i3))
    }
    fn insert<T: Into<Number> + Zero>(&mut self, i1: u8, i2: u8, i3: u8, value: T) {
        self.components.insert((i1, i2, i3), value);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TensorComponents4 {
    components: TensorComponents<(u8, u8, u8, u8)>,
}
impl TensorComponents4 {
    fn new() -> TensorComponents4 {
        TensorComponents4 {
            components: TensorComponents::new(),
        }
    }
    pub fn get(&self, i1: u8, i2: u8, i3: u8, i4: u8) -> Number {
        self.components.get(&(i1, i2, i3, i4))
    }
    fn insert<T: Into<Number> + Zero>(&mut self, i1: u8, i2: u8, i3: u8, i4: u8, value: T) {
        self.components.insert((i1, i2, i3, i4), value);
    }
    fn insert_values<T: Into<Number> + Zero>(&mut self, values: &[u8], value: T) {
        assert_eq!(values.len(), 4);
        self.insert(values[0], values[1], values[2], values[3], value);
    }
}

#[derive(Debug)]
pub enum SpinComponentsError {
    IoError(std::io::Error),
    TomlConversionError(toml::de::Error),
}
impl From<std::io::Error> for SpinComponentsError {
    fn from(err: std::io::Error) -> SpinComponentsError {
        SpinComponentsError::IoError(err)
    }
}
impl From<toml::de::Error> for SpinComponentsError {
    fn from(err: toml::de::Error) -> SpinComponentsError {
        SpinComponentsError::TomlConversionError(err)
    }
}
impl fmt::Display for SpinComponentsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SpinComponentsError::IoError(ref err) => {
                write!(f, "Failed to read the spin structures file: {}", err)
            }
            SpinComponentsError::TomlConversionError(ref err) => {
                write!(f, "Failed to parse the spin structures file: {}", err)
            }
        }
    }
}
impl std::error::Error for SpinComponentsError {}

#[cfg(test)]
mod test {
    use crate::math_expr::Number;
    use num_complex::Complex64;

    #[test]
    fn is_permutation_even() {
        use super::is_permutation_even;

        assert_eq!(is_permutation_even(&[0, 1]), true);
        assert_eq!(is_permutation_even(&[0, 1, 2]), true);
        assert_eq!(is_permutation_even(&[1, 2, 0]), true);
        assert_eq!(is_permutation_even(&[2, 0, 1]), true);
        assert_eq!(is_permutation_even(&[2, 1, 0]), false);
        assert_eq!(is_permutation_even(&[0, 2, 1]), false);
        assert_eq!(is_permutation_even(&[1, 0, 2]), false);
    }

    #[test]
    fn epsilon() {
        let eps = super::SpinTensorComponents::compute_epsilon(Number::Integer(1));
        assert_eq!(eps.get(0, 1, 2, 3).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(eps.get(2, 1, 3, 0).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(eps.get(3, 2, 1, 0).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(
            eps.get(1, 2, 3, 0).as_complex(),
            Complex64::new(-1f64, 0f64)
        );
        assert_eq!(
            eps.get(3, 0, 1, 2).as_complex(),
            Complex64::new(-1f64, 0f64)
        );
        assert_eq!(
            eps.get(1, 3, 0, 2).as_complex(),
            Complex64::new(-1f64, 0f64)
        );
        assert_eq!(eps.get(1, 2, 1, 0).as_complex(), Complex64::new(0f64, 0f64));
        assert_eq!(eps.get(1, 0, 3, 0).as_complex(), Complex64::new(0f64, 0f64));
    }

    #[test]
    fn gamma5() {
        let components =
            super::SpinTensorComponents::load("models/common/spin_structures.toml").unwrap();
        let gamma5 = components.gamma5;
        assert_eq!(gamma5.get(0, 0).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(gamma5.get(1, 1).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(gamma5.get(2, 2).as_complex(), Complex64::new(-1f64, 0f64));
        assert_eq!(gamma5.get(3, 3).as_complex(), Complex64::new(-1f64, 0f64));
        assert_eq!(gamma5.components.components.len(), 4);
    }

    #[test]
    fn proj_m() {
        let components =
            super::SpinTensorComponents::load("models/common/spin_structures.toml").unwrap();
        let projm = components.proj_m;
        assert_eq!(projm.get(2, 2).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projm.get(3, 3).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projm.components.components.len(), 2);
    }

    #[test]
    fn proj_p() {
        let components =
            super::SpinTensorComponents::load("models/common/spin_structures.toml").unwrap();
        let projp = components.proj_p;
        assert_eq!(projp.get(0, 0).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projp.get(1, 1).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projp.components.components.len(), 2);
    }

    #[test]
    fn zero_range() {
        let mut r = 0..0;
        assert_eq!(r.next(), None);
    }
}
