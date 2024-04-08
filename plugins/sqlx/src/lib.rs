use std::{
    fmt::{self, Display},
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
use futures_lite::future;
use slab::Slab;

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "sqlx".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(
    lambda_sender: RSender<LambdaCall>,
    _args: RVec<FfiValue>,
) -> RResult<PluginType, Error> {
    let plugin = dsqlx::instantiate_root_module();
    let plugin = plugin.new();
    let plugin = plugin(lambda_sender, vec![].into()).unwrap();
    ROk(Plugin_TO::from_value(plugin, TD_Opaque))
}

mod dsqlx {
    use super::*;

    use sqlx::{
        postgres::{PgPoolOptions, Postgres},
        Error as SqlxError, Pool,
    };

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "sqlx".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(
        _lambda_sender: RSender<LambdaCall>,
        _args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(Sqlx::default(), TD_Opaque))
    }

    #[derive(Clone, Debug)]
    struct Sqlx {
        pools: Slab<Pool<Postgres>>,
        errors: Slab<Arc<SqlxError>>,
    }

    impl Default for Sqlx {
        fn default() -> Self {
            Self {
                pools: Slab::new(),
                errors: Slab::new(),
            }
        }
    }

    impl Display for Sqlx {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Plugin for Sqlx {
        fn name(&self) -> RStr<'_> {
            "Sqlx".into()
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
            future::block_on(async {
                match ty.as_str() {
                    "Sqlx" => match func.as_str() {
                        "connect" => {
                            tracing::trace!("connect enter");
                            let connection_string: String = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                                .unwrap();

                            let pool = PgPoolOptions::new()
                                .max_connections(5)
                                .connect(&connection_string)
                                .await;

                            let result = match pool {
                                Ok(pool) => {
                                    let entry = self.pools.vacant_entry();
                                    let key = entry.key();
                                    self.pools.insert(pool);
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
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Uber(format!("Invalid type: {ty}").into())),
                }
                .into()
            })
        }
    }
}
