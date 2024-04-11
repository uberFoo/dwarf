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
    chacha::{error::ChaChaError, ffi_value::FfiValue},
    plug_in::{Error, LambdaCall, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
};

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "md".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(lambda_sender: RSender<LambdaCall>, args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    if let Some(FfiValue::String(plugin)) = args.first() {
        match plugin.as_str() {
            "md" => {
                let plugin = md::instantiate_root_module();
                let plugin = plugin.new();
                let plugin = plugin(lambda_sender, vec![].into()).unwrap();
                ROk(Plugin_TO::from_value(plugin, TD_Opaque))
            }
            _ => RErr(Error::Plugin(format!("Invalid plugin {plugin}").into())),
        }
    } else {
        RErr(Error::Plugin("Invalid plugin".into()))
    }
}

mod md {
    use super::*;

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "md".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(
        _lambda_sender: RSender<LambdaCall>,
        _args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(Md::default(), TD_Opaque))
    }

    #[derive(Clone, Debug)]
    struct Md {}

    impl Default for Md {
        fn default() -> Self {
            Self {}
        }
    }

    impl Display for Md {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Plugin for Md {
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
                "Md" => match func.as_str() {
                    "to_html" => {
                        let md: String = args
                            .first()
                            .unwrap()
                            .try_into()
                            .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                            .unwrap();

                        let md =
                            markdown::to_html_with_options(&md, &markdown::Options::gfm()).unwrap();

                        // This is as hack to get around a bug in markdown-rs
                        let md = md.replace("&lt;", "<").replace("&gt;", ">");

                        Ok(FfiValue::String(md.into()).into())
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
