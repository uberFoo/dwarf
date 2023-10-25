use std::{
    cell::RefCell,
    fmt::{self, Display},
    path::Path,
    rc::Rc,
};

use abi_stable::{
    export_root_module,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::{TD_CanDowncast, TD_Opaque},
    std_types::{RErr, ROk, RResult, RStr, RString, RVec},
};
use dwarf::{
    chacha::value::{FfiProxy, FfiValue, Value},
    plug_in::{Error, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
};
use log::debug;
use reqwest::Client;

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, id, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "http_client".into()
}

#[sabi_extern_fn]
pub fn id() -> RStr<'static> {
    "http_client".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    ROk(Plugin_TO::from_value(HttpClient(Client::new()), TD_Opaque))
}

#[derive(Clone, Debug)]
struct HttpClient(Client);

impl Display for HttpClient {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Plugin for HttpClient {
    fn name(&self) -> RStr<'_> {
        "HttpClient".into()
    }

    fn close(self) {}

    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let module_str = module.as_str();
            debug!("module: {module_str}, type: {ty}, func: {func}, args: {args:?}");
            Ok(FfiValue::Empty)
            // match module_str {
            //     "sarzak" => self.sarzak.invoke_func(module, ty, func, args).into(),
            //     "merlin" => self.merlin.invoke_func(module, ty, func, args).into(),
            //     _ => Err(Error::Uber("Invalid module".into())),
            // }
        })()
        .into()
    }
}
