use crate::ufo::{Parameter, UfoModel};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::Path;

// A temporary way to handle external parameters. The main disadvantage of this is that the
// lhablocks aren't ordered in any way, and neither are the parameters inside each block.

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ExternalParameters {
    pub data: HashMap<String, HashMap<String, f64>>,
}
impl ExternalParameters {
    pub fn write_toml_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ExternalError> {
        let bytes = toml::to_vec(self)?;
        fs::File::create(path)?.write_all(&bytes)?;
        Ok(())
    }
    pub fn write_default_toml_file<P: AsRef<Path>>(
        model: &UfoModel,
        path: P,
    ) -> Result<(), ExternalError> {
        let mut external = HashMap::new();
        for parameter in model.parameters.values() {
            let (block, name, value) = match parameter {
                Parameter::Internal { .. } => continue,
                Parameter::External {
                    lha_block,
                    name,
                    value,
                    ..
                } => (lha_block, name, value),
            };
            external
                .entry(block.to_string())
                .or_insert_with(|| HashMap::new())
                .insert(name.to_string(), *value);
        }
        ExternalParameters { data: external }.write_toml_file(path)
    }
}

#[derive(Debug)]
pub enum ExternalError {
    IntoToml(toml::ser::Error),
    FromToml(toml::de::Error),
    IoError(std::io::Error),
}
impl From<toml::ser::Error> for ExternalError {
    fn from(err: toml::ser::Error) -> ExternalError {
        ExternalError::IntoToml(err)
    }
}
impl From<std::io::Error> for ExternalError {
    fn from(err: std::io::Error) -> ExternalError {
        ExternalError::IoError(err)
    }
}
