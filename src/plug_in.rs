use std::{
    fmt,
    ops::{Deref, DerefMut},
    path::Path,
};

use abi_stable::{
    declare_root_module_statics,
    library::{LibraryError, RootModule},
    package_version_strings, sabi_trait,
    sabi_types::VersionStrings,
    std_types::{RBox, RString, RVec},
    std_types::{RCowStr, ROk, ROption, RResult, RSome, RStr},
    DynTrait, StableAbi,
};
use serde::{Deserialize, Serialize};

use crate::chacha::value::FfiValue;

pub mod error;
pub use error::{Error, Unsupported};

// pub type StorePluginType = Plugin_TO<'static, RBox<()>>;

pub struct StorePluginType {
    pub inner: Plugin_TO<'static, RBox<()>>,
}

impl Deref for StorePluginType {
    type Target = Plugin_TO<'static, RBox<()>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for StorePluginType {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl std::fmt::Debug for StorePluginType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StorePluginType")
            .field("inner", &self.inner.plugin_id())
            .finish()
    }
}

impl std::fmt::Display for StorePluginType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.inner.plugin_id())
    }
}

#[sabi_trait]
pub trait StorePlugin {}

#[sabi_trait]
/// A plugin which is loaded by the application,and provides some functionality.
// #[sabi(debug_print)]
pub trait Plugin {
    /// Handles a JSON encoded command.
    fn json_command(
        &mut self,
        command: RStr<'_>,
        // app: ApplicationMut<'_>,
    ) -> RResult<RString, Error>;

    fn invoke_func(&mut self, name: RStr<'_>, args: RVec<RString>) -> RResult<FfiValue, Error>;

    /// Gets the PluginId that was passed to this plugin in its constructor.
    fn plugin_id(&self) -> &PluginId;

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
    fn close(
        self,
        //app: ApplicationMut<'_>
    );
}

pub type PluginType = Plugin_TO<'static, RBox<()>>;

/// The identifier for a plugin.
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, StableAbi, Serialize, Deserialize)]
pub struct PluginId {
    pub named: RCowStr<'static>,
    /// The number of the instance of this Plugin.
    pub instance: u64,
}

/// The root module of a`plugin` dynamic library.
///
/// To load this module,
/// call <PluginMod as RootModule>::load_from_directory(some_directory_path)
#[repr(C)]
#[derive(StableAbi)]
#[sabi(kind(Prefix(prefix_ref = PluginMod_Ref)))]
#[sabi(missing_field(panic))]
pub struct PluginMod {
    pub load: extern "C" fn(PluginId, RString) -> RResult<PluginType, Error>,
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
    pub new: extern "C" fn(PluginId) -> RResult<PluginType, Error>,
}

impl RootModule for PluginMod_Ref {
    declare_root_module_statics! {PluginMod_Ref}
    const BASE_NAME: &'static str = "plugin";
    const NAME: &'static str = "plugin";
    const VERSION_STRINGS: VersionStrings = package_version_strings!();
}

/// The response from having called `ApplicationMut::send_command_to_plugin` ealier.
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, StableAbi)]
pub struct PluginResponse<'a> {
    /// The id of the plugin that is responding.
    pub plugin_id: PluginId,
    /// The response from the plugin
    pub response: RCowStr<'a>,
}

impl<'a> PluginResponse<'a> {
    pub fn owned_response(plugin_id: PluginId, response: RString) -> Self {
        Self {
            plugin_id,
            response: response.into(),
        }
    }
    pub fn borrowed_response(plugin_id: PluginId, response: RStr<'a>) -> Self {
        Self {
            plugin_id,
            response: response.into(),
        }
    }
}

/// This struct is the root module,
/// which must be converted to `ExampleLib_Ref` to be passed through ffi.
///
/// The `#[sabi(kind(Prefix(prefix_ref = ExampleLib_Ref)))]`
/// attribute tells `StableAbi` to create an ffi-safe static reference type
/// for `ExampleLib` called `ExampleLib_Ref`.
///
/// The `#[sabi(missing_field(panic))]` attribute specifies that trying to
/// access a field that doesn't exist must panic with a message saying that
/// the field is inaccessible.
#[repr(C)]
#[derive(StableAbi)]
#[sabi(kind(Prefix(prefix_ref = ExampleLib_Ref)))]
#[sabi(missing_field(panic))]
pub struct ExampleLib {
    pub new_appender: extern "C" fn() -> AppenderBox<u32>,

    pub new_boxed_interface: extern "C" fn() -> BoxedInterface<'static>,

    /// The `#[sabi(last_prefix_field)]` attribute here means that this is the last
    /// field in this struct that was defined in the first compatible version of the library
    /// (0.1.0, 0.2.0, 0.3.0, 1.0.0, 2.0.0 ,etc),
    /// requiring new fields to always be added below preexisting ones.
    ///
    /// The `#[sabi(last_prefix_field)]` attribute would stay on this field until the
    /// library bumps its "major" version,
    /// at which point it would be moved to the last field at the time.
    ///
    #[sabi(last_prefix_field)]
    pub append_string: extern "C" fn(&mut BoxedInterface<'_>, RString),
}

/// The RootModule trait defines how to load the root module of a library.
impl RootModule for ExampleLib_Ref {
    abi_stable::declare_root_module_statics! {ExampleLib_Ref}

    const BASE_NAME: &'static str = "example_library";
    const NAME: &'static str = "example_library";
    const VERSION_STRINGS: VersionStrings = package_version_strings!();
}

/// `#[sabi_trait]` is how one creates an ffi-safe trait object from a trait definition.
///
/// In this case the trait object is `Appender_TO<'lt, Pointer<()>, Element>`,where:
///
/// - `'lt`:
///     Is the lifetime bound of the type that constructed the trait object
///     (`'static` is the lifetime bound of objects that don't borrow anything).
///
/// - `Pointer<()>`:
///     Is any pointer that implements some abi_stable specific traits,
///     this pointer owns the value that implements `Appender`.
///
/// - `Element`:
///     This is the element type of the collection that we operate on.
///
#[sabi_trait]
pub trait Appender {
    /// The element type of the collection.
    type Element;

    /// Appends one element at the end of the collection.
    fn push(&mut self, value: Self::Element);

    /// Appends many elements at the end of the collection.
    fn append(&mut self, vec: RVec<Self::Element>);

    /// Converts this collection into an `RVec`.
    ///
    /// As opposed to regular trait objects,
    /// it is possible to call by-value methods on trait objects generated by `#[sabi_trait]`.
    ///
    /// The `#[sabi(last_prefix_field)]` attribute here means that this is the last method
    /// that was defined in the first compatible version of the library
    /// (0.1.0, 0.2.0, 0.3.0, 1.0.0, 2.0.0 ,etc),
    /// requiring new methods to always be added below preexisting ones.
    ///
    /// The `#[sabi(last_prefix_field)]` attribute would stay on this method until the library
    /// bumps its "major" version,
    /// at which point it would be moved to the last method at the time.
    ///
    #[sabi(last_prefix_field)]
    fn into_rvec(self) -> RVec<Self::Element>;
}

/// A type alias for the Appender trait object.
///
/// `'static` here means that the trait object cannot contain any borrows.
pub type AppenderBox<T> = Appender_TO<'static, RBox<()>, T>;

/// This loads the root from the library in the `directory` folder.
///
/// This for the case where this example is copied into the 3 crates.
///
pub fn load_root_module_in_directory(directory: &Path) -> Result<ExampleLib_Ref, LibraryError> {
    ExampleLib_Ref::load_from_directory(directory)
}

/// This loads the root module
///
/// This is for the case where this example is copied into a single crate
// pub fn load_root_module_in_directory(_: &Path) -> Result<ExampleLib_Ref, LibraryError> {
//     ExampleLib_Ref::load_module_with(|| Ok(super::implementation::get_library()))
// }

//////////////////////////////////////////////////////////

/// This type implements `InterfaceType`
/// (because of the `#[sabi(impl_InterfaceType())]` helper attribute of `#[derive(StableAbi)]` ),
/// describing the traits required when constructing `DynTrait<_, TheInterface>`,
/// and are then implemented by it.
#[repr(C)]
#[derive(StableAbi)]
#[sabi(impl_InterfaceType(Sync, Send, Debug, Display))]
pub struct TheInterface;

/// An alias for the trait object used in this example
pub type BoxedInterface<'borr> = DynTrait<'borr, RBox<()>, TheInterface>;

impl<T> Appender for RVec<T> {
    type Element = T;
    fn push(&mut self, value: Self::Element) {
        self.push(value);
    }
    fn append(&mut self, vec: RVec<Self::Element>) {
        self.extend(vec);
    }
    fn into_rvec(self) -> RVec<Self::Element> {
        self
    }
}
