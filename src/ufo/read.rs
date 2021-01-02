use std::path;
use cpython::{Python, PyErr, PyObject, PyList, PyString, PythonObject, PyModule, ObjectProtocol};

#[derive(Debug)]
pub struct UfoError {
    pub source: PyErr
}
impl From<PyErr> for UfoError {
    fn from(other: PyErr) -> UfoError {
        return UfoError {
            source: other
        }
    }
}

#[derive(Debug)]
pub struct UfoModel {
    coupling_orders: Vec<CouplingOrder>,
}

pub fn convert<P: AsRef<path::Path>>(model_path: P, model_name: &str) -> Result<UfoModel,UfoError> {
    let gil = Python::acquire_gil();
    let python = gil.python();
    let model = load_model(python, model_path, model_name)?;
    let coupling_orders = convert_coupling_orders(python, model)?;
    Ok(UfoModel{
        coupling_orders,
    })
}

fn load_model<P: AsRef<path::Path>>(python: Python, model_path: P, model_name: &str) -> Result<PyModule,UfoError> {
    let sys = python.import("sys")?;
    let sys_path: PyList = sys.get(python, "path")?.cast_into(python).unwrap();
    sys_path.append(python, PyString::new(python, model_path.as_ref().to_str().unwrap()).into_object());
    let ufo = python.import(model_name)?;
    Ok(ufo)
}

fn convert_coupling_orders(python: Python, model: PyModule) -> Result<Vec<CouplingOrder>,UfoError> {
    let orders: PyList = model.get(python, "all_orders")?.cast_into(python).unwrap();
    let converted: Result<Vec<_>,_> = orders.iter(python).map(|item| convert_coupling_order(python, item)).collect();
    Ok(converted?)
}
fn convert_coupling_order(python: Python, object: PyObject) -> Result<CouplingOrder, UfoError> {
    let name = object.getattr(python, "name")?.extract::<String>(python)?;
    let expansion_order = object.getattr(python, "expansion_order")?.extract::<i64>(python)?;
    let hierarchy = object.getattr(python, "hierarchy")?.extract::<i64>(python)?;
    let perturbative_expansion = object.getattr(python, "perturbative_expansion")?.extract::<i64>(python)?;
    Ok(CouplingOrder{
        name,
        expansion_order,
        hierarchy,
        perturbative_expansion,
    })
}

#[cfg(test)]
mod test {
    #[test]
    fn load() {
        println!("{:?}", super::convert("ufo", "SM_NLO"));
        assert_eq!(1,0);
    }
}