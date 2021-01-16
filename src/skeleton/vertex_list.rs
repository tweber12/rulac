use crate::ufo::{PdgCode, Vertex};
use permutohedron::LexicalPermutation;
use std::collections::HashMap;

pub struct VertexList<'a> {
    pub contents: HashMap<PdgCode, VertexListParticle<'a>>,
}
impl<'a> VertexList<'a> {
    pub fn with_filter<P>(vertices: &'a [Vertex], predicate: &P) -> VertexList<'a>
    where
        P: Fn(&Vertex) -> bool,
    {
        VertexList::new_internal(vertices.iter().filter(|&v| predicate(v)))
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
                .or_insert_with(|| VertexListParticle::new())
                .insert(&particles, vertex);
            ok = particles.next_permutation();
        }
    }
}
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
pub struct CompleteParticle<'a> {
    pub vertex: &'a Vertex,
    pub out: PdgCode,
}
pub struct IncompleteVertex<'a> {
    pub vertex: &'a Vertex,
    pub remaining: Vec<PdgCode>,
}
pub enum VertexLeaf<'a> {
    Complete(CompleteParticle<'a>),
    Incomplete(IncompleteVertex<'a>),
}
impl<'a> VertexLeaf<'a> {
    fn new(particles: &[PdgCode], vertex: &'a Vertex) -> VertexLeaf<'a> {
        if particles.len() == 3 {
            VertexLeaf::Complete(CompleteParticle {
                vertex,
                out: particles[2],
            })
        } else {
            VertexLeaf::Incomplete(IncompleteVertex {
                vertex,
                remaining: particles[2..].to_owned(),
            })
        }
    }
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
