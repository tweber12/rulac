pub mod colored;
pub mod uncolored;

use crate::skeleton::uncolored::InternalId;
use crate::ufo::PdgCode;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct External {
    pub pdg_code: PdgCode,
    pub id: InternalId,
    pub particle: usize,
    pub flipped: bool,
}

pub trait SkeletonId {
    fn level(&self) -> usize;
}

pub type ExternalLevel<I> = HashMap<I, External>;
pub type InternalLevel<I, T> = HashMap<I, Bone<I, T>>;

#[derive(Clone, Debug)]
pub struct Skeleton<I, T> {
    pub external: ExternalLevel<I>,
    pub internal: Vec<InternalLevel<I, T>>,
    pub left_out: usize,
    pub last: Bone<I, T>,
}
impl<I: Hash + Eq + Clone + SkeletonId, T> Skeleton<I, T> {
    pub fn count_graphs(&self) -> u64 {
        self.last.count_graphs(&self)
    }
    fn get_bone(&self, level: usize, id: &I) -> &Bone<I, T> {
        &self.internal[level - 2][id]
    }
    fn remove_unused(&mut self) {
        let mut used = Used::new(&self.last);
        let mut iter = self.internal.iter_mut();
        while let Some(level) = iter.next_back() {
            used.remove_unused(level);
        }
    }
}

#[derive(Clone, Debug)]
pub struct Bone<I, T> {
    pub pdg_code: PdgCode,
    pub id: InternalId,
    pub fragments: Vec<BoneFragment<I, T>>,
}
impl<I: Hash + Eq + Clone + SkeletonId, T> Bone<I, T> {
    fn count_graphs(&self, skeleton: &Skeleton<I, T>) -> u64 {
        self.fragments
            .iter()
            .map(|f| f.count_graphs(skeleton))
            .sum()
    }
}

#[derive(Clone, Debug)]
pub struct BoneFragment<I, T> {
    pub constituents: Vec<I>,
    pub outgoing: usize,
    pub content: T,
}
impl<I: Hash + Eq + Clone + SkeletonId, T> BoneFragment<I, T> {
    fn count_graphs(&self, skeleton: &Skeleton<I, T>) -> u64 {
        self.constituents
            .iter()
            .map(|id| match id.level() {
                1 => 1,
                l => skeleton.get_bone(l, id).count_graphs(skeleton),
            })
            .product()
    }
}

struct Used<I> {
    used: HashSet<I>,
}
impl<I: Hash + Eq + Clone> Used<I> {
    fn new<T>(start: &Bone<I, T>) -> Used<I> {
        let mut used = HashSet::new();
        for f in start.fragments.iter() {
            for c in f.constituents.iter() {
                used.insert(c.clone());
            }
        }
        Used { used }
    }

    fn remove_unused<T>(&mut self, level: &mut HashMap<I, Bone<I, T>>) {
        level.retain(|i, v| {
            if !self.used.contains(i) {
                return false;
            }
            for f in v.fragments.iter() {
                self.used.extend(f.constituents.iter().cloned());
            }
            true
        });
    }
}
