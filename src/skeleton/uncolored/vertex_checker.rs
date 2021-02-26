use crate::math_expr::{eval_ufo_math, EvalContext, Number};
use crate::ufo::{CouplingValue, UfoModel, Vertex};
use num_traits::Zero;
use std::collections::HashMap;

pub struct VertexChecker<'a> {
    context: &'a EvalContext,
    model: &'a UfoModel,
    vertices: HashMap<String, bool>,
    couplings: HashMap<String, Number>,
}
impl<'a> VertexChecker<'a> {
    pub fn new(context: &'a EvalContext, model: &'a UfoModel) -> VertexChecker<'a> {
        VertexChecker {
            context,
            model,
            vertices: HashMap::new(),
            couplings: HashMap::new(),
        }
    }
    pub fn check_vertex(&mut self, vertex: &Vertex) -> bool {
        if let Some(&present) = self.vertices.get(&vertex.name) {
            return present;
        }
        let mut present = true;
        for coupling in vertex.couplings.iter() {
            let value = self.get_coupling(&coupling.coupling);
            present = present && !value.is_zero();
        }
        self.vertices.insert(vertex.name.clone(), present);
        println!("{} -> {}", vertex.name, present);
        present
    }

    fn get_coupling(&mut self, name: &str) -> Number {
        if let Some(&value) = self.couplings.get(name) {
            return value;
        }
        let value = match &self.model.couplings[name].value {
            CouplingValue::Simple(expr) => eval_ufo_math(expr, &self.context).unwrap(),
            _ => unimplemented!(),
        };
        self.couplings.insert(name.to_string(), value);
        println!("\t{} -> {:?}", name, value);
        value
    }
}
