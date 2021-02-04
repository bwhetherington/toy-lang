use crate::{
    common::{Str, TLError},
    runtime::{Engine, Value},
};
use dyn_clonable::*;
use std::collections::HashMap;

#[clonable]
pub trait ModuleLoader: Clone {
    fn load_module(&mut self, src: &str, engine: &mut Engine) -> Result<Option<Value>, TLError>;
}

#[derive(Clone)]
pub struct CacheLoader<T> {
    loader: T,
    cache: HashMap<Str, Value>,
}

impl<T> CacheLoader<T> {
    pub fn new(base: T) -> CacheLoader<T> {
        CacheLoader {
            loader: base,
            cache: HashMap::new(),
        }
    }
}

impl<T> ModuleLoader for CacheLoader<T>
where
    T: ModuleLoader + Clone,
{
    fn load_module(&mut self, src: &str, engine: &mut Engine) -> Result<Option<Value>, TLError> {
        // Check if we've already cached the module
        if self.cache.contains_key(src) {
            println!("got cached module: {}", src);
            Ok(self.cache.get(src).cloned())
        } else {
            let value = self.loader.load_module(src, engine)?;
            if let Some(value) = &value {
                self.cache.insert(src.into(), value.clone());
            }
            Ok(value)
        }
    }
}
