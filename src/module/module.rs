pub trait ModuleLoader {
    fn load_module(&mut self, path: &str) -> Option<String>;

    fn after_load_module(&mut self) {}
}
