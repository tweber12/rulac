const INITIAL_OFFSET: i64 = -10000;

use crate::color::tensor::{AntiTripletIndex, ColorIndex, ColorTensor, TripletIndex};
use crate::color::ColorExpr;
use crate::math_expr::parse::parse_math;
use crate::math_expr::{BinaryOperator, Number};
use crate::ufo::UfoMath;
use num_rational::Rational32;
use num_traits::Zero;

#[derive(Clone, Debug)]
pub struct ChainBuilder {
    pub chains: Vec<Chain>,
}
impl ChainBuilder {
    pub fn from_expr(structure: &UfoMath) -> ChainBuilder {
        ChainBuilder {
            chains: vec![Chain::from_expr(structure)],
        }
    }

    pub fn from_chain(chain: Chain) -> ChainBuilder {
        ChainBuilder {
            chains: vec![chain],
        }
    }

    pub fn from_chains(chains: &[Chain]) -> ChainBuilder {
        ChainBuilder {
            chains: chains.to_vec(),
        }
    }

    pub fn empty() -> ChainBuilder {
        ChainBuilder {
            chains: vec![Chain::new(0, 0)],
        }
    }

    pub fn from_tensor(tensor: ColorTensor) -> ChainBuilder {
        let mut chains = ChainBuilder::from_chain(Chain::new(0, 1));
        chains.append_all(tensor);
        chains
    }

    pub fn add_index<I: Into<ColorIndex> + Copy>(&mut self, index: I) {
        for chain in self.chains.iter_mut() {
            chain.add_index(index);
        }
    }

    pub fn add_factor<I: Into<Rational32>>(&mut self, factor: I) {
        let factor = factor.into();
        for chain in self.chains.iter_mut() {
            chain.factor *= factor;
        }
    }

    pub fn append_all(&mut self, tensor: ColorTensor) {
        for chain in self.chains.iter_mut() {
            chain.append(tensor);
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

    pub fn replace_indices<P, F>(self, index_predicate: &P, function: &F) -> ChainBuilder
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

    pub fn reduce(mut self) -> ChainBuilder {
        for c in self.chains.iter_mut() {
            c.order_tensors();
        }
        self.chains.sort_by(|a, b| a.tensors.cmp(&b.tensors));
        let mut iter = self.chains.into_iter();
        let mut first = match iter.next() {
            Some(n) => n,
            None => return ChainBuilder::empty(),
        };
        let mut out = Vec::new();
        for chain in iter {
            if chain.tensors != first.tensors {
                if !first.factor.is_zero() {
                    out.push(first);
                }
                first = chain;
            } else {
                first.factor += chain.factor;
            }
        }
        if !first.factor.is_zero() {
            out.push(first);
        }
        ChainBuilder { chains: out }
    }

    pub fn combine_pairs<L, R, F>(
        self,
        predicate_left: &L,
        predicate_right: &R,
        function: &F,
    ) -> ChainBuilder
    where
        L: Fn(&ColorTensor) -> bool,
        R: Fn(&ColorTensor) -> bool,
        F: Fn(ColorTensor, ColorTensor) -> ChainBuilder,
    {
        let mut out = ChainBuilder::new();
        for chain in self.chains {
            out.add_chains(chain.combine_pairs(predicate_left, predicate_right, function));
        }
        out
    }

    fn new() -> ChainBuilder {
        ChainBuilder { chains: Vec::new() }
    }

    fn add_chains(&mut self, other: ChainBuilder) {
        self.chains.extend(other.chains);
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

    fn offset(&self) -> i64 {
        self.chains
            .iter()
            .map(|c| c.offset)
            .min()
            .unwrap_or(INITIAL_OFFSET)
    }
}

#[derive(Clone, Debug)]
pub struct Chain {
    pub factor: Rational32,
    pub tensors: Vec<ColorTensor>,
    indices: Vec<ColorIndex>,
    offset: i64,
}
impl Chain {
    pub fn new_trace<T: Into<Rational32>>(offset: i64, indices: &[ColorIndex], factor: T) -> Chain {
        let mut chain = Chain::new(offset, factor);
        chain.add_trace(indices);
        chain
    }

    pub fn new_delta_chain<T: Into<Rational32>>(
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
            indices: Vec::new(),
            offset: 0,
        }
    }

    fn new<T: Into<Rational32>>(offset: i64, factor: T) -> Chain {
        Chain {
            factor: factor.into(),
            tensors: Vec::new(),
            indices: Vec::new(),
            offset,
        }
    }

    fn join(&mut self, other: &Chain) {
        self.factor *= other.factor;
        self.tensors.extend(other.tensors.iter());
        self.indices.extend(other.indices.iter());
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
        let mut out = ChainBuilder::from_chain(self);
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
        let (selected, other) = self.indices.into_iter().partition(index_predicate);
        self.indices = other;
        let mut out = ChainBuilder::from_chain(self);
        for index in selected.into_iter().filter(index_predicate) {
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
        let mut out = ChainBuilder::from_chain(self);
        out.join_all_multi(&function(&matches).chains);
        out
    }

    fn combine_pairs<L, R, F>(
        mut self,
        predicate_left: &L,
        predicate_right: &R,
        function: &F,
    ) -> ChainBuilder
    where
        L: Fn(&ColorTensor) -> bool,
        R: Fn(&ColorTensor) -> bool,
        F: Fn(ColorTensor, ColorTensor) -> ChainBuilder,
    {
        let (selected, rest) = self
            .tensors
            .into_iter()
            .partition(|t| predicate_left(t) || predicate_right(t));
        self.tensors = rest;
        if selected.is_empty() {
            return ChainBuilder::from_chain(self);
        }
        let (mut left, mut right): (Vec<_>, Vec<_>) =
            selected.into_iter().partition(predicate_left);
        if left.is_empty() || right.is_empty() {
            self.tensors.extend(left);
            self.tensors.extend(right);
            return ChainBuilder::from_chain(self);
        }
        while left.len() > right.len() {
            self.tensors.push(
                left.pop()
                    .expect("UNREACHABLE: left.len() > right.len() >= 0"),
            );
        }
        while right.len() > left.len() {
            self.tensors.push(
                right
                    .pop()
                    .expect("UNREACHABLE: right.len() > left.len() >= 0"),
            );
        }
        let mut result = ChainBuilder::from_chain(self);
        for (l, r) in left.into_iter().zip(right) {
            result.join_all_multi(&function(l, r).chains);
        }
        result.combine_pairs(predicate_left, predicate_right, function)
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
            self.add_index(next);
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

    fn from_expr(structure: &UfoMath) -> Chain {
        let UfoMath(structure) = structure;
        let expr: ColorExpr = parse_math(structure).unwrap();
        let mut structure = Chain::new(INITIAL_OFFSET, 1);
        structure.add_expr(expr);
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
                self.add_index(index);
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

    fn add_index<I: Into<ColorIndex>>(&mut self, index: I) {
        self.indices.push(index.into());
    }

    fn order_tensors(&mut self) {
        self.tensors.sort();
    }
}

#[cfg(test)]
mod test {
    use crate::color::tensor::ColorTensor;
    #[test]
    fn order_delta() {
        let d1 = ColorTensor::new_kronecker_triplet(2, 1);
        let d2 = ColorTensor::new_kronecker_triplet(1, 5);
        let d3 = ColorTensor::new_kronecker_triplet(1, 3);
        assert!(d1 > d2);
        assert!(d1 > d3);
        assert!(d2 > d3);
    }
}
