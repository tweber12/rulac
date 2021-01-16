pub mod math;

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path;
use std::fmt;

use math::MathExpr;

#[derive(Debug)]
pub struct UfoModel {
    pub function_library: HashMap<String, FunctionDefinition>,
    pub parameters: HashMap<String, Parameter>,
    pub particles: HashMap<PdgCode, Particle>,
    pub coupling_orders: HashMap<String, CouplingOrder>,
    pub couplings: HashMap<String, Coupling>,
    pub lorentz_structures: HashMap<String, Lorentz>,
    pub decays: HashMap<PdgCode, Decay>,
    pub propagators: HashMap<String, Propagator>,
    pub vertices: Vec<Vertex>,
}
impl UfoModel {
    pub fn load<P: AsRef<path::Path>>(path: P) -> Result<UfoModel, UfoError> {
        let function_library = read_into_map(&path, "function_library.json")?;
        let parameters = read_into_map(&path, "parameters.json")?;
        let particles = read_into_map(&path, "particles.json")?;
        let coupling_orders = read_into_map(&path, "coupling_orders.json")?;
        let couplings = read_into_map(&path, "couplings.json")?;
        let lorentz_structures = read_into_map(&path, "lorentz.json")?;
        let decays = match read_into_map(&path, "decays.json") {
            Ok(d) => d,
            Err(_) => HashMap::new(),
        };
        let propagators = match read_into_map(&path, "propagators.json") {
            Ok(p) => p,
            Err(_) => HashMap::new(),
        };
        let vertex_file = fs::File::open(&path.as_ref().join("vertices.json"))?;
        let vertices = serde_json::from_reader(vertex_file)?;
        Ok(UfoModel {
            function_library,
            parameters,
            particles,
            coupling_orders,
            couplings,
            lorentz_structures,
            decays,
            propagators,
            vertices,
        })
    }
}

#[derive(Debug)]
pub enum UfoError {
    JsonError(serde_json::Error),
    IoError(std::io::Error),
}
impl From<std::io::Error> for UfoError {
    fn from(err: std::io::Error) -> UfoError {
        UfoError::IoError(err)
    }
}
impl From<serde_json::Error> for UfoError {
    fn from(err: serde_json::Error) -> UfoError {
        UfoError::JsonError(err)
    }
}

fn read_into_map<P, T>(path: &P, name: &str) -> Result<HashMap<T::Name, T>, UfoError>
where
    P: AsRef<path::Path>,
    T: DeserializeOwned + Named,
    <T as Named>::Name: Eq + std::hash::Hash,
{
    let file = fs::File::open(path.as_ref().join(name))?;
    let list: Vec<T> = serde_json::from_reader(file)?;
    Ok(list.into_iter().map(|t| (t.name(), t)).collect())
}

trait Named {
    type Name;
    fn name(&self) -> Self::Name;
}
impl Named for Decay {
    type Name = PdgCode;
    fn name(&self) -> PdgCode {
        self.particle
    }
}
impl Named for Particle {
    type Name = PdgCode;
    fn name(&self) -> PdgCode {
        self.pdg_code
    }
}
macro_rules! derive_named_string {
    ($for:ident) => {
        impl Named for $for {
            type Name = String;
            fn name(&self) -> String {
                self.name.clone()
            }
        }
    };
}
derive_named_string!(CouplingOrder);
derive_named_string!(Coupling);
derive_named_string!(FunctionDefinition);
derive_named_string!(Lorentz);
derive_named_string!(Parameter);
derive_named_string!(Propagator);

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PdgCode(pub i64);
impl fmt::Display for PdgCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PdgCode(p) = self;
        p.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CouplingOrder {
    pub name: String,
    pub expansion_order: i64,
    pub hierarchy: i64,
    pub perturbative_expansion: Option<i64>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Particle {
    pub pdg_code: PdgCode,
    pub name: String,
    pub antiname: String,
    pub spin: i64,
    pub color: i64,
    pub mass: String,
    pub width: String,
    pub texname: String,
    pub antitexname: String,
    pub counterterm: Option<String>,
    pub charge: f64,
    pub line: String,
    pub propagating: bool,
    pub goldstoneboson: bool,
    #[serde(rename = "GhostNumber")]
    pub ghost_number: i64,
    #[serde(rename = "LeptonNumber")]
    pub lepton_number: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ParameterNature {
    Internal,
    External,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ParameterType {
    Real,
    Complex,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ValOrExpr {
    Value(f64),
    Expr(MathExpr),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum CouplingValue {
    Simple(MathExpr),
    Orders(HashMap<i64, MathExpr>),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub texname: String,
    pub nature: ParameterNature,
    #[serde(rename = "type")]
    pub ptype: ParameterType,
    pub value: ValOrExpr,
    pub lhablock: Option<String>,
    pub lhacode: Option<Vec<i64>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct VertexCoupling {
    #[serde(rename = "color")]
    pub color_index: u64,
    #[serde(rename = "lorentz")]
    pub lorentz_index: u64,
    pub coupling: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Vertex {
    pub name: String,
    pub particles: Vec<PdgCode>,
    pub color: Vec<MathExpr>,
    pub lorentz: Vec<String>,
    pub couplings: Vec<VertexCoupling>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Lorentz {
    pub name: String,
    pub spins: Vec<i64>,
    pub structure: MathExpr,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PartialWidth {
    pub decay_products: Vec<String>,
    pub width: MathExpr,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Decay {
    pub name: String,
    pub particle: PdgCode,
    pub partial_widths: Vec<PartialWidth>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Coupling {
    pub name: String,
    pub value: CouplingValue,
    pub order: HashMap<String, i64>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Propagator {
    pub name: String,
    pub numerator: MathExpr,
    pub denominator: MathExpr,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct FunctionDefinition {
    pub name: String,
    pub arguments: Vec<String>,
    pub expr: MathExpr,
}

#[cfg(test)]
mod test {
    use serde_json;
    use std::fs;

    #[test]
    fn test_coupling_orders() {
        let file = fs::File::open("SM_NLO/coupling_orders.json").unwrap();
        let _: Vec<super::CouplingOrder> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_particles() {
        let file = fs::File::open("SM_NLO/particles.json").unwrap();
        let _: Vec<super::Particle> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_parameters() {
        let file = fs::File::open("SM_NLO/parameters.json").unwrap();
        let _: Vec<super::Parameter> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_vertices() {
        let file = fs::File::open("SM_NLO/vertices.json").unwrap();
        let _: Vec<super::Vertex> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_lorentz() {
        let file = fs::File::open("SM_NLO/lorentz.json").unwrap();
        let _: Vec<super::Lorentz> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_couplings() {
        let file = fs::File::open("SM_NLO/couplings.json").unwrap();
        let _: Vec<super::Coupling> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_decays() {
        let file = fs::File::open("SM_NLO/decays.json").unwrap();
        let _: Vec<super::Decay> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_propagators() {
        let file = fs::File::open("SM_NLO/propagators.json").unwrap();
        let _: Vec<super::Propagator> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_function_library() {
        let file = fs::File::open("SM_NLO/function_library.json").unwrap();
        let _: Vec<super::FunctionDefinition> = serde_json::from_reader(file).unwrap();
    }
}
