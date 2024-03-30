use abi_stable::export_root_module;
use dwarf::plug_in::PluginModRef;

pub mod lu_dog;

pub use lu_dog::{name, new};

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    lu_dog::instantiate_root_module()
}
