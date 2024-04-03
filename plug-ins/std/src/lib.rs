use std::{
    fmt::{self, Display},
    fs::File,
    io::Read,
    sync::Arc,
};

use abi_stable::{
    export_root_module,
    external_types::crossbeam_channel::RSender,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::TD_Opaque,
    std_types::{RBox, RErr, ROk, RResult, RStr, RVec},
};
use dwarf::{
    chacha::{error::ChaChaError, ffi_value::FfiValue},
    plug_in::{Error, LambdaCall, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
    DwarfInteger,
};
use slab::Slab;

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "std".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(lambda_sender: RSender<LambdaCall>, args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    if let Some(FfiValue::String(plugin)) = args.first() {
        match plugin.as_str() {
            "fs" => {
                dbg!("woot");
                let plugin = fs::instantiate_root_module();
                let plugin = plugin.new();
                let plugin = plugin(lambda_sender, vec![].into()).unwrap();
                ROk(Plugin_TO::from_value(plugin, TD_Opaque))
            }
            _ => RErr(Error::Uber(format!("Invalid plugin {plugin}").into())),
        }
    } else {
        RErr(Error::Uber("Invalid plugin".into()))
    }
}

mod fs {
    use super::*;

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "fs".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(
        lambda_sender: RSender<LambdaCall>,
        args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(Fs::default(), TD_Opaque))
    }

    #[derive(Clone, Debug)]
    struct Fs {
        files: Slab<Arc<File>>,
        errors: Slab<Arc<std::io::Error>>,
    }

    impl Default for Fs {
        fn default() -> Self {
            Self {
                files: Slab::new(),
                errors: Slab::new(),
            }
        }
    }

    impl Display for Fs {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Plugin for Fs {
        fn name(&self) -> RStr<'_> {
            "Fs".into()
        }

        #[tracing::instrument]
        fn invoke_func(
            &self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            Ok(FfiValue::Empty).into()
        }

        #[tracing::instrument]
        fn invoke_func_mut(
            &mut self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            match ty.as_str() {
                "File" => match func.as_str() {
                    "open" => {
                        tracing::trace!("open enter");
                        let path: String = args
                            .first()
                            .unwrap()
                            .try_into()
                            .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                            .unwrap();

                        let file = File::open(path);
                        let result = match file {
                            Ok(file) => {
                                let entry = self.files.vacant_entry();
                                let key = entry.key();
                                self.files.insert(Arc::new(file));
                                ROk(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                            }
                            Err(e) => {
                                let entry = self.errors.vacant_entry();
                                let key = entry.key();
                                self.errors.insert(Arc::new(e));
                                RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                            }
                        };

                        tracing::trace!("open exit");
                        Ok(FfiValue::Result(result))
                    }
                    "read" => {
                        let key: DwarfInteger = args
                            .first()
                            .unwrap()
                            .try_into()
                            .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                            .unwrap();
                        let mut buf: String = args
                            .get(1)
                            .unwrap()
                            .try_into()
                            .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                            .unwrap();

                        let file = self.files.get_mut(key as usize).unwrap();
                        let result = match file.read_to_string(&mut buf) {
                            Ok(_) => ROk(RBox::new(FfiValue::Integer(buf.len() as DwarfInteger))),
                            Err(e) => {
                                let entry = self.errors.vacant_entry();
                                let key = entry.key();
                                self.errors.insert(Arc::new(e));
                                RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                            }
                        };

                        Ok(FfiValue::Result(result).into())
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type: {ty}").into())),
            }
            .into()
        }
    }
}
