use crate::module::{ModuleLoader, ModulePath, PackageType};
use std::{
    fs,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug)]
pub struct FileLoader {
    file_stack: Vec<PathBuf>,
}

impl ModuleLoader for FileLoader {
    fn load_module(&mut self, path: &str) -> Option<String> {
        let path = ModulePath::resolve(path);
        let path = self.resolve_path(&path)?;
        let res = fs::read_to_string(&path).ok();
        self.file_stack.push(path.parent()?.to_owned());
        res
    }

    fn after_load_module(&mut self) {
        self.file_stack.pop();
    }
}

impl FileLoader {
    pub fn new() -> FileLoader {
        FileLoader {
            file_stack: Vec::new(),
        }
    }

    fn resolve_package(&self, package: &PackageType) -> Option<PathBuf> {
        match package {
            PackageType::Relative => match self.file_stack.last() {
                Some(path) => Some(path.clone()),
                None => Some(Path::new(".").to_owned()),
            },
            PackageType::Imported => {
                // Module location
                let module_location = Path::new("/home/bwh/toy-lang").to_path_buf();
                Some(module_location)
            }
        }
    }

    fn resolve_path(&self, path: &ModulePath) -> Option<PathBuf> {
        let ModulePath { package_type, path } = path;
        let mut root = self.resolve_package(package_type)?;
        for section in path {
            root.push(section);
        }
        Some(root)
    }
}
