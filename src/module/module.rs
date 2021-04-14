use std::fmt;

pub trait ModuleLoader {
    fn entry_point(&mut self, path: &str) -> Option<()>;

    fn load_module(&mut self, path: &str) -> Option<String>;

    fn after_load_module(&mut self);
}

impl fmt::Debug for Box<dyn ModuleLoader> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<ModuleLoader>")
    }
}
