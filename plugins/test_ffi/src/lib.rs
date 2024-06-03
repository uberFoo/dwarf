use std::fmt::{self, Display};

use abi_stable::{
    export_root_module,
    external_types::crossbeam_channel::RSender,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::TD_Opaque,
    std_types::{RErr, ROk, RResult, RStr, RVec},
};
use dwarf::{
    bubba::error::BubbaError,
    chacha::ffi_value::FfiValue,
    plug_in::{Error, LambdaCall, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
};

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "test_ffi".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(lambda_sender: RSender<LambdaCall>, args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    let plugin = test_ffi::instantiate_root_module();
    let plugin = plugin.new();
    let plugin = plugin(lambda_sender, args).unwrap();
    ROk(Plugin_TO::from_value(plugin, TD_Opaque))
}

mod test_ffi {
    use super::*;

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "test_ffi".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(
        _lambda_sender: RSender<LambdaCall>,
        _args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(TestFfi::default(), TD_Opaque))
    }

    #[derive(Clone, Debug, Default)]
    struct TestFfi {}

    impl Display for TestFfi {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Plugin for TestFfi {
        fn name(&self) -> RStr<'_> {
            "Md".into()
        }

        #[tracing::instrument]
        fn invoke_func(
            &self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            match ty.as_str() {
                "TestFfi" => match func.as_str() {
                    "test_bool" => {
                        let arg: bool = match args
                            .first()
                            .unwrap()
                            .try_into()
                            .map_err(|e: BubbaError| Error::Plugin(e.to_string().into()))
                        {
                            Ok(b) => b,
                            Err(e) => return RErr(e),
                        };

                        Ok(FfiValue::Boolean(arg).into())
                    }
                    func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                },
                ty => Err(Error::Plugin(format!("Invalid type: {ty}").into())),
            }
            .into()
        }

        #[tracing::instrument]
        fn invoke_func_mut(
            &mut self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            Ok(FfiValue::Empty).into()
        }
    }
}
