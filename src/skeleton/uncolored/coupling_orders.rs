use crate::skeleton::uncolored::Id;
use crate::ufo::{UfoModel, Vertex};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
enum RestrictMode {
    AllOrders,
    MinimumOrder(HashMap<String, usize>),
    FixedOrder(HashMap<String, OrderCount>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OrderCount {
    LessThanEqual(u8),
    Exact(u8),
}
impl OrderCount {
    fn max(&self) -> u8 {
        match *self {
            OrderCount::LessThanEqual(n) => n,
            OrderCount::Exact(n) => n,
        }
    }
}

pub struct CouplingOrderRestriction {
    indices: HashMap<String, usize>,
    max: Vec<u8>,
    mode: RestrictMode,
}
impl CouplingOrderRestriction {
    pub fn all_orders() -> CouplingOrderRestriction {
        CouplingOrderRestriction {
            indices: HashMap::new(),
            max: Vec::new(),
            mode: RestrictMode::AllOrders,
        }
    }
    pub fn minimum_order(model: &UfoModel) -> CouplingOrderRestriction {
        let mut weights = HashMap::new();
        let mut indices = HashMap::new();
        for (i, order) in model.coupling_orders.values().enumerate() {
            weights.insert(order.name.clone(), order.hierarchy as usize);
            indices.insert(order.name.clone(), i);
        }
        CouplingOrderRestriction {
            indices,
            max: Vec::new(),
            mode: RestrictMode::MinimumOrder(weights),
        }
    }
    pub fn fixed_order(restrict: HashMap<String, OrderCount>) -> CouplingOrderRestriction {
        let mut indices = HashMap::new();
        let mut max = Vec::new();
        for (i, (n, c)) in restrict.iter().enumerate() {
            indices.insert(n.clone(), i);
            max.push(c.max());
        }
        CouplingOrderRestriction {
            indices,
            max,
            mode: RestrictMode::FixedOrder(restrict),
        }
    }

    pub fn add_particle(&self, left: &[u8], right: &[u8]) -> Option<Vec<u8>> {
        let next = add(left, right);
        if self.max.iter().zip(next.iter()).all(|(max, v)| v <= max) {
            Some(next)
        } else {
            None
        }
    }

    pub fn add_vertex(
        &self,
        left: &[u8],
        right: &[u8],
        vertex: &Vertex,
        model: &UfoModel,
    ) -> Option<Vec<u8>> {
        if self.mode == RestrictMode::AllOrders {
            return Some(Vec::new());
        }
        let orders = model.couplings[&vertex.couplings[0].coupling].order.clone();
        for v in vertex.couplings.iter() {
            assert_eq!(model.couplings[&v.coupling].order, orders);
        }
        let mut next = add(left, right);
        if next.is_empty() {
            next = vec![0; self.indices.len()];
        }
        for (name, count) in orders.iter() {
            next[self.indices[name]] += *count as u8;
        }
        if self.max.iter().zip(next.iter()).all(|(max, v)| v <= max) {
            Some(next)
        } else {
            None
        }
    }

    pub fn select_last_level<K>(&self, candidates: Vec<(Id, K)>) -> Vec<K> {
        match self.mode {
            RestrictMode::AllOrders => candidates.into_iter().map(|(_, k)| k).collect(),
            RestrictMode::MinimumOrder(ref weights) => {
                minimum_order(candidates, &self.indices, weights)
            }
            RestrictMode::FixedOrder(ref restrict) => {
                matching_order(candidates, &self.indices, restrict)
            }
        }
    }
}

fn add(left: &[u8], right: &[u8]) -> Vec<u8> {
    match (left.is_empty(), right.is_empty()) {
        (false, false) => left.iter().zip(right.iter()).map(|(l, r)| l + r).collect(),
        (true, false) => right.to_vec(),
        (false, true) => left.to_vec(),
        (true, true) => Vec::new(),
    }
}

fn minimum_order<K>(
    candidates: Vec<(Id, K)>,
    indices: &HashMap<String, usize>,
    weights: &HashMap<String, usize>,
) -> Vec<K> {
    candidates
        .into_iter()
        .min_by_key(|(i, _)| rank(&i.order, indices, weights))
        .map(|(_, k)| vec![k])
        .unwrap_or_default()
}

fn rank(
    counts: &[u8],
    indices: &HashMap<String, usize>,
    weights: &HashMap<String, usize>,
) -> usize {
    weights
        .iter()
        .map(|(s, w)| counts[indices[s]] as usize * w)
        .sum()
}

fn matching_order<K>(
    candidates: Vec<(Id, K)>,
    indices: &HashMap<String, usize>,
    restrictions: &HashMap<String, OrderCount>,
) -> Vec<K> {
    candidates
        .into_iter()
        .filter(|(i, _)| matches(&i.order, indices, restrictions))
        .map(|(_, k)| k)
        .collect()
}

fn matches(
    orders: &[u8],
    indices: &HashMap<String, usize>,
    restrictions: &HashMap<String, OrderCount>,
) -> bool {
    for (coupling, restriction) in restrictions.iter() {
        let value = orders[indices[coupling]];
        let ok = match *restriction {
            OrderCount::Exact(n) => value == n,
            OrderCount::LessThanEqual(n) => value <= n,
        };
        if !ok {
            return false;
        }
    }
    true
}
