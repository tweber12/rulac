use crate::ufo::{PdgCode, Vertex};
use permutohedron::LexicalPermutation;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VertexList<'a> {
    pub contents: HashMap<PdgCode, VertexListParticle<'a>>,
}
impl<'a> VertexList<'a> {
    pub fn with_filter<P>(vertices: &'a HashMap<String, Vertex>, predicate: &P) -> VertexList<'a>
    where
        P: Fn(&Vertex) -> bool,
    {
        VertexList::new_internal(vertices.values().filter(|&v| predicate(v)))
    }
    fn new_internal<I>(vertices: I) -> VertexList<'a>
    where
        I: Iterator<Item = &'a Vertex>,
    {
        let mut list = VertexList {
            contents: HashMap::new(),
        };
        for vertex in vertices {
            list.insert_vertex(vertex);
        }
        list
    }
    fn insert_vertex(&mut self, vertex: &'a Vertex) {
        let mut particles = vertex.particles.clone();
        particles.sort();
        let mut ok = true;
        while ok {
            self.contents
                .entry(particles[0])
                .or_insert_with(VertexListParticle::new)
                .insert(&particles, vertex);
            ok = particles.next_permutation();
        }
    }
}
#[derive(Debug)]
pub struct VertexListParticle<'a> {
    pub contents: HashMap<PdgCode, Vec<VertexLeaf<'a>>>,
}
impl<'a> VertexListParticle<'a> {
    fn new() -> VertexListParticle<'a> {
        VertexListParticle {
            contents: HashMap::new(),
        }
    }
    fn insert(&mut self, particles: &[PdgCode], vertex: &'a Vertex) {
        self.contents
            .entry(particles[1])
            .or_default()
            .push(VertexLeaf::new(particles, vertex));
    }
}

#[derive(Debug)]
pub struct VertexLeaf<'a> {
    pub vertex: &'a Vertex,
    pub state: VertexLeafState,
}
impl<'a> VertexLeaf<'a> {
    fn new(particles: &[PdgCode], vertex: &'a Vertex) -> VertexLeaf<'a> {
        let state = if particles.len() == 3 {
            VertexLeafState::Complete(particles[2])
        } else {
            VertexLeafState::Incomplete(particles[2..].to_owned())
        };
        VertexLeaf { vertex, state }
    }
}

#[derive(Debug)]
pub enum VertexLeafState {
    Complete(PdgCode),
    Incomplete(Vec<PdgCode>),
}

#[cfg(test)]
mod test {
    use permutohedron::LexicalPermutation;

    // The numbers are different than expected maybe, since the original permutation, that the data is already in isn't counted

    #[test]
    fn permute_trivial() {
        let mut data = vec![1, 1];
        let mut permutations = 0;
        loop {
            let ok = data.next_permutation();
            if !ok {
                break;
            }
            println!("{:?}", data);
            permutations += 1;
        }
        assert_eq!(permutations, 0);
    }

    #[test]
    fn permute_112() {
        let mut data = vec![1, 1, 2];
        let mut permutations = 0;
        loop {
            let ok = data.next_permutation();
            if !ok {
                break;
            }
            println!("{:?}", data);
            permutations += 1;
        }
        assert_eq!(permutations, 2);
    }
}
