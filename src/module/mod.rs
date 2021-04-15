#[cfg(feature = "fs")]
mod file;
mod module;
mod resolve;

#[cfg(feature = "fs")]
pub use file::FileLoader;

pub use module::{ModuleLoader, NoopLoader};
pub use resolve::{ModulePath, PackageType};
