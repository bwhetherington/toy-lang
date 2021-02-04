use crate::{
    common::TLResult,
    module::ModuleLoader,
    parser::{desugar_statements, module},
    runtime::{Engine, Value},
};
use std::fs;

#[derive(Clone)]
pub struct FileLoader;

impl ModuleLoader for FileLoader {
    fn load_module(&mut self, src: &str, engine: &mut Engine) -> TLResult<Option<Value>> {
        let res = fs::read_to_string(src);
        match res {
            Ok(content) => {
                // Parser level 1
                let parsed = module(&content)?;
                let desugared = desugar_statements(&parsed)?;
                let res = engine.eval_module(&desugared)?;
                Ok(Some(res))
            }
            _ => Ok(None),
        }
    }
}
