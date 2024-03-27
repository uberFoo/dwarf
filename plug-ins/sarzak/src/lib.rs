use std::{
    cell::RefCell,
    fmt::{self, Display},
    path::Path,
    rc::Rc,
};

use abi_stable::{
    export_root_module,
    external_types::crossbeam_channel::RSender,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::{TD_CanDowncast, TD_Opaque},
    std_types::{RErr, ROk, RResult, RStr, RString, RVec},
};
use dwarf::{
    chacha::{
        ffi_value::{FfiProxy, FfiValue},
        value::Value,
    },
    plug_in::{Error, LambdaCall, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
};
use log::debug;

pub mod merlin;
pub mod sarzak;

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "sarzak".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(lambda_sender: RSender<LambdaCall>, args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    let this = if args.len() == 0 {
        // Each of these is a "sub-plug-in". We have to instantiate it the same
        // way that a plug-in is instantiated in the VM or interpreter.
        let sarzak = sarzak::instantiate_root_module();
        let sarzak = sarzak.new();
        let sarzak = sarzak(lambda_sender.clone(), vec![].into()).unwrap();

        let merlin = merlin::instantiate_root_module();
        let merlin = merlin.new();
        let merlin = merlin(lambda_sender, vec![].into()).unwrap();

        Model { sarzak, merlin }
    } else if args.len() == 1 {
        let sarzak = sarzak::instantiate_root_module();
        let sarzak = sarzak.new();
        let sarzak = sarzak(lambda_sender.clone(), args.clone()).unwrap();

        let merlin = merlin::instantiate_root_module();
        let merlin = merlin.new();
        let merlin = merlin(lambda_sender, args).unwrap();

        Model { sarzak, merlin }
    } else {
        return RErr(Error::Uber("Invalid arguments".into()));
    };

    ROk(Plugin_TO::from_value(this, TD_Opaque))
}

#[derive(Clone, Debug)]
struct Model {
    sarzak: PluginType,
    merlin: PluginType,
}

impl Display for Model {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "sarzak: {:?}\nmerlin: {:?}", self.sarzak, self.merlin)
    }
}

impl Plugin for Model {
    fn name(&self) -> RStr<'_> {
        "sarzak".into()
    }

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
            match module_str {
                "sarzak" => self.sarzak.invoke_func(module, ty, func, args).into(),
                "merlin" => self.merlin.invoke_func(module, ty, func, args).into(),
                _ => Err(Error::Uber("Invalid module".into())),
            }
        })()
        .into()
    }
}
