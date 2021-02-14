use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::BufReader;
use std::path;

#[derive(Debug)]
pub struct UfoModel {
    pub function_library: HashMap<String, FunctionDefinition>,
    pub parameters: HashMap<String, Parameter>,
    pub particles: HashMap<PdgCode, Particle>,
    pub coupling_orders: HashMap<String, CouplingOrder>,
    pub couplings: HashMap<String, Coupling>,
    pub lorentz_structures: HashMap<String, Lorentz>,
    pub decays: HashMap<PdgCode, Decay>,
    pub vertices: HashMap<String, Vertex>,
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
        let vertices = read_into_map(&path, "vertices.json")?;
        Ok(UfoModel {
            function_library,
            parameters,
            particles,
            coupling_orders,
            couplings,
            lorentz_structures,
            decays,
            vertices,
        })
    }

    pub fn anti_pdg_code(&self, pdg_code: PdgCode) -> PdgCode {
        self.particles[&pdg_code].anti_pdg_code()
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
    let list: Vec<T> = serde_json::from_reader(BufReader::new(file))?;
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
derive_named_string!(Vertex);

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct UfoMath(pub String);

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PdgCode(pub i64);
impl fmt::Display for PdgCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PdgCode(p) = self;
        p.fmt(f)
    }
}

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize_repr, Deserialize_repr,
)]
#[repr(i8)]
pub enum Spin {
    Ghost = -1,
    Zero = 1,
    OneHalf = 2,
    One = 3,
    ThreeHalf = 4,
    Two = 5,
}

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize_repr, Deserialize_repr,
)]
#[repr(i8)]
pub enum Color {
    Singlet = 1,
    Triplet = 3,
    AntiTriplet = -3,
    Sextet = 6,
    AntiSextet = -6,
    Octet = 8,
}
impl Color {
    pub fn bar(self) -> Color {
        match self {
            Color::Triplet => Color::AntiTriplet,
            Color::AntiTriplet => Color::Triplet,
            Color::Sextet => Color::AntiSextet,
            Color::AntiSextet => Color::Sextet,
            _ => self,
        }
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
    pub spin: Spin,
    pub color: Color,
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
}
impl Particle {
    pub fn self_conjugate(&self) -> bool {
        // This is exactly the same definition as in the python object_library
        self.name == self.antiname
    }
    pub fn anti_pdg_code(&self) -> PdgCode {
        if !self.self_conjugate() {
            let PdgCode(pdg) = self.pdg_code;
            PdgCode(-pdg)
        } else {
            self.pdg_code
        }
    }
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
    Expr(UfoMath),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum CouplingValue {
    Simple(UfoMath),
    Orders(HashMap<i64, UfoMath>),
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
    pub color_index: usize,
    #[serde(rename = "lorentz")]
    pub lorentz_index: usize,
    pub coupling: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Vertex {
    pub name: String,
    pub particles: Vec<PdgCode>,
    pub color: Vec<UfoMath>,
    pub lorentz: Vec<String>,
    pub couplings: Vec<VertexCoupling>,
}
impl Vertex {
    pub fn get_particle_colors(&self, model: &UfoModel) -> Vec<Color> {
        self.particles
            .iter()
            .map(|p| model.particles[p].color)
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Lorentz {
    pub name: String,
    pub spins: Vec<Spin>,
    pub structure: UfoMath,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PartialWidth {
    pub decay_products: Vec<String>,
    pub width: UfoMath,
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
pub struct FunctionDefinition {
    pub name: String,
    pub arguments: Vec<String>,
    pub expr: UfoMath,
}

#[cfg(test)]
mod test {
    use std::fs;

    #[test]
    fn test_coupling_orders() {
        let file = fs::File::open("tests/models_json/SM_NLO/coupling_orders.json").unwrap();
        let _: Vec<super::CouplingOrder> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_particles() {
        let file = fs::File::open("tests/models_json/SM_NLO/particles.json").unwrap();
        let _: Vec<super::Particle> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_parameters() {
        let file = fs::File::open("tests/models_json/SM_NLO/parameters.json").unwrap();
        let _: Vec<super::Parameter> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_vertices() {
        let file = fs::File::open("tests/models_json/SM_NLO/vertices.json").unwrap();
        let _: Vec<super::Vertex> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_lorentz() {
        let file = fs::File::open("tests/models_json/SM_NLO/lorentz.json").unwrap();
        let _: Vec<super::Lorentz> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_couplings() {
        let file = fs::File::open("tests/models_json/SM_NLO/couplings.json").unwrap();
        let _: Vec<super::Coupling> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_decays() {
        let file = fs::File::open("tests/models_json/SM_NLO/decays.json").unwrap();
        let _: Vec<super::Decay> = serde_json::from_reader(file).unwrap();
    }

    #[test]
    fn test_function_library() {
        let file = fs::File::open("tests/models_json/SM_NLO/function_library.json").unwrap();
        let _: Vec<super::FunctionDefinition> = serde_json::from_reader(file).unwrap();
    }
}
