pub trait ModuleLoader {
    fn entry_point(&mut self, path: &str) -> Option<()>;

    fn load_module(&mut self, path: &str) -> Option<String>;

    fn after_load_module(&mut self);
}
