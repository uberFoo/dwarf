use abi_stable::{
    declare_root_module_statics,
    library::RootModule,
    package_version_strings, sabi_trait,
    sabi_types::VersionStrings,
    std_types::{RBox, RVec},
    std_types::{RResult, RStr},
    StableAbi,
};

use crate::chacha::ffi_value::FfiValue;

pub mod error;
pub use error::{Error, Unsupported};

#[sabi_trait]
/// A plugin which is loaded by the application,and provides some functionality.
// #[sabi(debug_print)]
pub trait Plugin: Clone + Debug + Display + Send + Sync {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        name: RStr<'_>,
        args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error>;

    /// Closes the plugin,
    ///
    /// This does not unload the dynamic library of this plugin,
    /// you can instantiate another instance of this plugin with
    /// `PluginMod_Ref::get_module().new()(application_handle)`.
    ///
    ///
    ///
    /// The `#[sabi(last_prefix_field)]` attribute here means that this is the last method
    /// that was defined in the first compatible version of the library
    /// (0.1.0, 0.2.0, 0.3.0, 1.0.0, 2.0.0 ,etc),
    /// requiring new methods to always be added below preexisting ones.
    ///
    /// The `#[sabi(last_prefix_field)]` attribute would stay on this method until the library
    /// bumps its "major" version,
    /// at which point it would be moved to the last method at the time.
    #[sabi(last_prefix_field)]
    fn name(&self) -> RStr<'_>;
}

pub type PluginType = Plugin_TO<'static, RBox<()>>;

/// The root module of a`plugin` dynamic library.
///
/// To load this module,
/// call <PluginMod as RootModule>::load_from_directory(some_directory_path)
#[repr(C)]
#[derive(StableAbi)]
#[sabi(kind(Prefix(prefix_ref = PluginModRef)))]
#[sabi(missing_field(panic))]
pub struct PluginModule {
    pub name: extern "C" fn() -> RStr<'static>,
    pub id: extern "C" fn() -> RStr<'static>,
    /// Constructs the plugin.
    ///
    ///
    /// The `#[sabi(last_prefix_field)]` attribute here means that this is the last field in this struct
    /// that was defined in the first compatible version of the library
    /// (0.1.0, 0.2.0, 0.3.0, 1.0.0, 2.0.0 ,etc),
    /// requiring new fields to always be added below preexisting ones.
    ///
    /// The `#[sabi(last_prefix_field)]` attribute would stay on this field until the library
    /// bumps its "major" version,
    /// at which point it would be moved to the last field at the time.
    ///
    #[sabi(last_prefix_field)]
    pub new: extern "C" fn(RVec<FfiValue>) -> RResult<PluginType, Error>,
}

impl RootModule for PluginModRef {
    declare_root_module_statics! {PluginModRef}
    const BASE_NAME: &'static str = "plugin";
    const NAME: &'static str = "plugin";
    const VERSION_STRINGS: VersionStrings = package_version_strings!();
}
