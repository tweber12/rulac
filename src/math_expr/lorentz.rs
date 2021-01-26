use super::{BinaryOperator, IndexRange, MathExpr, Number, SummationIndex};
use num_complex::Complex64;
use num_traits::identities::Zero;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::Read;
use std::path;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LorentzIndex(i64);
impl From<i64> for LorentzIndex {
    fn from(index: i64) -> LorentzIndex {
        LorentzIndex(index)
    }
}
impl IndexRange for LorentzIndex {
    fn range() -> std::ops::Range<u8> {
        0..4
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SpinorIndex(i64);
impl From<i64> for SpinorIndex {
    fn from(index: i64) -> SpinorIndex {
        SpinorIndex(index)
    }
}
impl IndexRange for SpinorIndex {
    fn range() -> std::ops::Range<u8> {
        1..5
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "tensor")]
pub enum LorentzTensor {
    #[serde(rename = "C")]
    ChargeConjugation {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    Epsilon {
        mu1: LorentzIndex,
        mu2: LorentzIndex,
        mu3: LorentzIndex,
        mu4: LorentzIndex,
    },
    Gamma {
        mu1: LorentzIndex,
        i2: SpinorIndex,
        i3: SpinorIndex,
    },
    Gamma5 {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    #[serde(rename = "Identity")]
    KroneckerDelta {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    Metric {
        mu1: LorentzIndex,
        mu2: LorentzIndex,
    },
    #[serde(rename = "P")]
    Momentum {
        mu1: LorentzIndex,
        particle: i64,
    },
    ProjM {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    ProjP {
        i1: SpinorIndex,
        i2: SpinorIndex,
    },
    Sigma {
        mu1: LorentzIndex,
        mu2: LorentzIndex,
        i3: SpinorIndex,
        i4: SpinorIndex,
    },
}
impl LorentzTensor {
    fn get_component(&self, components: &SpinTensorComponents, indices: &Indices) -> Number {
        match *self {
            LorentzTensor::ChargeConjugation { i1, i2 } => {
                components.charge_conjugation.get(indices[i1], indices[i2])
            }
            LorentzTensor::Epsilon { mu1, mu2, mu3, mu4 } => {
                components
                    .epsilon
                    .get(indices[mu1], indices[mu2], indices[mu3], indices[mu4])
            }
            LorentzTensor::Gamma { mu1, i2, i3 } => {
                components.gamma.get(indices[mu1], indices[i2], indices[i3])
            }
            LorentzTensor::Gamma5 { i1, i2 } => components.gamma5.get(indices[i1], indices[i2]),
            LorentzTensor::KroneckerDelta { i1, i2 } => {
                components.identity.get(indices[i1], indices[i2])
            }
            LorentzTensor::Metric { mu1, mu2 } => components.metric.get(indices[mu1], indices[mu2]),
            LorentzTensor::ProjM { i1, i2 } => components.proj_m.get(indices[i1], indices[i2]),
            LorentzTensor::ProjP { i1, i2 } => components.proj_p.get(indices[i1], indices[i2]),
            LorentzTensor::Sigma { mu1, mu2, i3, i4 } => {
                components
                    .sigma
                    .get(indices[mu1], indices[mu2], indices[i3], indices[i4])
            }
            LorentzTensor::Momentum { .. } => unimplemented! {},
        }
    }
}

pub struct SpinTensorComponents {
    metric: TensorComponents2,
    identity: TensorComponents2,
    gamma: TensorComponents3,
    epsilon: TensorComponents4,
    charge_conjugation: TensorComponents2,
    gamma5: TensorComponents2,
    proj_m: TensorComponents2,
    proj_p: TensorComponents2,
    sigma: TensorComponents4,
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
        SpinTensorComponents::compute_gamma5(&internal.gamma5, &mut components);
        SpinTensorComponents::compute_projm(&internal.proj_m, &mut components);
        SpinTensorComponents::compute_projp(&internal.proj_p, &mut components);
        SpinTensorComponents::compute_sigma(&internal.sigma, &mut components);
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
                    let value = matrix[mu as usize][(i - 1) as usize][(j - 1) as usize];
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
                let value = matrix[(i - 1) as usize][(j - 1) as usize];
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
    fn compute_gamma5(relation: &SpinTensorRelation, components: &mut SpinTensorComponents) {
        let mut gamma5 = TensorComponents2::new();
        let mut indices = Indices::new();
        for (i, &index) in relation.lorentz.iter().enumerate() {
            indices.set_index(index, i as u8);
        }
        for i in SpinorIndex::range() {
            indices.set_index(relation.spinor[0], i);
            for j in SpinorIndex::range() {
                indices.set_index(relation.spinor[1], j);
                let expr =
                    expand_sums(&relation.expr, components, &mut indices).constant_propagation();
                match expr.extract_number() {
                    None => panic!(
                        "BUG: Failed to compute ({},{}) component of gamma5: {:?}",
                        i, j, expr
                    ),
                    Some(e) => gamma5.insert(i, j, e),
                }
            }
        }
        components.gamma5 = gamma5;
    }
    fn compute_projm(relation: &SpinTensorRelation, components: &mut SpinTensorComponents) {
        let mut projm = TensorComponents2::new();
        let mut indices = Indices::new();
        for i in SpinorIndex::range() {
            indices.set_index(relation.spinor[0], i);
            for j in SpinorIndex::range() {
                indices.set_index(relation.spinor[1], j);
                let expr =
                    expand_sums(&relation.expr, components, &mut indices).constant_propagation();
                match expr.extract_number() {
                    None => panic!(
                        "BUG: Failed to compute ({},{}) component of ProjM: {:?}",
                        i, j, expr
                    ),
                    Some(e) => projm.insert(i, j, e),
                }
            }
        }
        components.proj_m = projm;
    }
    fn compute_projp(relation: &SpinTensorRelation, components: &mut SpinTensorComponents) {
        let mut projp = TensorComponents2::new();
        let mut indices = Indices::new();
        for i in SpinorIndex::range() {
            indices.set_index(relation.spinor[0], i);
            for j in SpinorIndex::range() {
                indices.set_index(relation.spinor[1], j);
                let expr =
                    expand_sums(&relation.expr, components, &mut indices).constant_propagation();
                match expr.extract_number() {
                    None => panic!(
                        "BUG: Failed to compute ({},{}) component of ProjP: {:?}",
                        i, j, expr
                    ),
                    Some(e) => projp.insert(i, j, e),
                }
            }
        }
        components.proj_p = projp;
    }
    fn compute_sigma(relation: &SpinTensorRelation, components: &mut SpinTensorComponents) {
        let mut sigma = TensorComponents4::new();
        let mut indices = Indices::new();
        for mu in LorentzIndex::range() {
            indices.set_index(relation.lorentz[0], mu);
            for nu in LorentzIndex::range() {
                indices.set_index(relation.lorentz[1], nu);
                for i in SpinorIndex::range() {
                    indices.set_index(relation.spinor[0], i);
                    for j in SpinorIndex::range() {
                        indices.set_index(relation.spinor[1], j);
                        let expr = expand_sums(&relation.expr, components, &mut indices)
                            .constant_propagation();
                        match expr.extract_number() {
                            None => panic!(
                                "BUG: Failed to compute ({},{},{},{}) component of sigma: {:?}",
                                mu, nu, i, j, expr
                            ),
                            Some(e) => sigma.insert(mu, nu, i, j, e),
                        }
                    }
                }
            }
        }
        components.sigma = sigma;
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

#[derive(Debug)]
struct Indices {
    indices: HashMap<SummationIndex, u8>,
}
impl Indices {
    fn new() -> Indices {
        Indices {
            indices: HashMap::new(),
        }
    }
    fn set_index<T: Into<SummationIndex>>(&mut self, index: T, value: u8) {
        self.indices.insert(index.into(), value);
    }
    fn unset_index<T: Into<SummationIndex>>(&mut self, index: T) {
        self.indices.remove(&index.into());
    }
}
impl<T: Into<SummationIndex>> std::ops::Index<T> for Indices {
    type Output = u8;
    fn index(&self, idx: T) -> &u8 {
        self.indices.index(&idx.into())
    }
}

fn expand_sums(
    expr: &MathExpr,
    components: &SpinTensorComponents,
    indices: &mut Indices,
) -> MathExpr {
    match expr {
        MathExpr::Sum { ref expr, index } => {
            let mut terms = index.range().map(|i| {
                indices.set_index(*index, i);
                expand_sums(expr, components, indices)
            });
            let mut val = terms
                .next()
                .expect("BUG: There has to be at least one value for an index!");
            for t in terms {
                val = MathExpr::BinaryOp {
                    operator: BinaryOperator::Add,
                    left: Box::new(val),
                    right: Box::new(t),
                };
            }
            indices.unset_index(*index);
            val
        }
        MathExpr::LorentzTensor { lorentz } => {
            let value = lorentz.get_component(components, indices);
            MathExpr::Number { value }
        }
        MathExpr::BinaryOp {
            operator,
            left,
            right,
        } => MathExpr::BinaryOp {
            operator: *operator,
            left: Box::new(expand_sums(left, components, indices)),
            right: Box::new(expand_sums(right, components, indices)),
        },
        MathExpr::UnaryOp { operator, operand } => MathExpr::UnaryOp {
            operator: *operator,
            operand: Box::new(expand_sums(operand, components, indices)),
        },
        MathExpr::Conditional {
            condition,
            if_true,
            if_false,
        } => MathExpr::Conditional {
            condition: Box::new(expand_sums(condition, components, indices)),
            if_true: Box::new(expand_sums(if_true, components, indices)),
            if_false: Box::new(expand_sums(if_false, components, indices)),
        },
        MathExpr::Comparison { operators, values } => MathExpr::Comparison {
            values: values
                .iter()
                .map(|c| expand_sums(c, components, indices))
                .collect(),
            operators: operators.clone(),
        },
        MathExpr::Call { function, args } => MathExpr::Call {
            function: function.clone(),
            args: args
                .iter()
                .map(|a| expand_sums(a, components, indices))
                .collect(),
        },
        _ => expr.clone(),
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

#[derive(Clone, Debug, PartialEq)]
pub struct TensorComponents<T>
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
        assert_eq!(gamma5.get(1, 1).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(gamma5.get(2, 2).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(gamma5.get(3, 3).as_complex(), Complex64::new(-1f64, 0f64));
        assert_eq!(gamma5.get(4, 4).as_complex(), Complex64::new(-1f64, 0f64));
        assert_eq!(gamma5.components.components.len(), 4);
    }

    #[test]
    fn proj_m() {
        let components =
            super::SpinTensorComponents::load("models/common/spin_structures.toml").unwrap();
        let projm = components.proj_m;
        assert_eq!(projm.get(3, 3).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projm.get(4, 4).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projm.components.components.len(), 2);
    }

    #[test]
    fn proj_p() {
        let components =
            super::SpinTensorComponents::load("models/common/spin_structures.toml").unwrap();
        let projp = components.proj_p;
        assert_eq!(projp.get(1, 1).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projp.get(2, 2).as_complex(), Complex64::new(1f64, 0f64));
        assert_eq!(projp.components.components.len(), 2);
    }
}
