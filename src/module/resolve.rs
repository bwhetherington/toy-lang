#[derive(Clone, Copy, Debug)]
pub enum PackageType {
    Relative,
    Imported,
}

#[derive(Clone, Debug)]
pub struct ModulePath {
    pub package_type: PackageType,
    pub path: Vec<String>,
}

impl ModulePath {
    pub fn resolve(src: &str) -> ModulePath {
        let path: Vec<_> = src.split("/").map(ToString::to_string).collect();
        let package_type = if src.starts_with(".") {
            PackageType::Relative
        } else {
            PackageType::Imported
        };
        ModulePath { package_type, path }
    }

    pub fn is_relative(&self) -> bool {
        match self.package_type {
            PackageType::Relative => true,
            _ => false,
        }
    }
}
