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
    let plugin = postgres::instantiate_root_module();
    let plugin = plugin.new();
    let plugin = plugin(lambda_sender, vec![].into()).unwrap();
    ROk(Plugin_TO::from_value(plugin, TD_Opaque))
}

mod postgres {
    use super::*;

    use std::sync::Mutex;

    use sqlx::{
        postgres::{PgPoolOptions, Postgres},
        Error as SqlxError, Row,
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
        lambda_sender: RSender<LambdaCall>,
        _args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(Sqlx::new(lambda_sender), TD_Opaque))
    }

    #[derive(Clone)]
    struct Sqlx {
        lambda_call: RSender<LambdaCall>,
        pools: Arc<Mutex<Slab<sqlx::Pool<Postgres>>>>,
        errors: Arc<Mutex<Slab<Arc<SqlxError>>>>,
        rows: Arc<Mutex<Slab<Arc<sqlx::postgres::PgRow>>>>,
    }

    impl std::fmt::Debug for Sqlx {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Sqlx {
        fn new(lambda_call: RSender<LambdaCall>) -> Self {
            Self {
                lambda_call,
                pools: Arc::new(Mutex::new(Slab::new())),
                errors: Arc::new(Mutex::new(Slab::new())),
                rows: Arc::new(Mutex::new(Slab::new())),
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
            future::block_on(async {
                match ty.as_str() {
                    "Error" => match func.as_str() {
                        "to_string" => {
                            let key: DwarfInteger = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let guard = self.errors.lock().unwrap();
                            let error = guard.get(key as usize).unwrap();

                            Ok(FfiValue::String(error.to_string().into()))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Map" => match func.as_str() {
                        "execute" => {
                            let query: String = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let pool: DwarfInteger = args
                                .get(1)
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let bindings: Vec<FfiValue> = args
                                .get(2)
                                .unwrap()
                                .clone()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let guard = self.pools.lock().unwrap();
                            let pool = guard.get(pool as usize).unwrap();

                            let mut result = sqlx::query(&query);
                            for binding in bindings {
                                match binding {
                                    FfiValue::String(value) => {
                                        result = result.bind(value.to_string());
                                    }
                                    FfiValue::Integer(value) => {
                                        result = result.bind(value as i64);
                                    }
                                    _ => {
                                        panic!("Invalid binding");
                                    }
                                }
                            }
                            let result = result.execute(pool).await;

                            let result = match result {
                                Ok(result) => ROk(RBox::new(result.rows_affected().into())),
                                Err(e) => {
                                    let mut guard = self.errors.lock().unwrap();
                                    let entry = guard.vacant_entry();
                                    let key = entry.key();
                                    guard.insert(Arc::new(e));
                                    RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                }
                            };

                            Ok(FfiValue::Result(result))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Query" => match func.as_str() {
                        "query_all" => {
                            let query: String = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let pool: DwarfInteger = args
                                .get(1)
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let FfiValue::Lambda(lambda) = args.get(2).unwrap() else {
                                panic!("Invalid lambda");
                            };

                            let guard = self.pools.lock().unwrap();
                            let pool = guard.get(pool as usize).unwrap();

                            let result = sqlx::query(&query)
                                .map(|row: sqlx::postgres::PgRow| {
                                    let (s, result) = crossbeam::channel::bounded(1);

                                    let key = {
                                        let mut guard = self.rows.lock().unwrap();
                                        guard.insert(Arc::new(row))
                                    };

                                    let lambda_call = LambdaCall {
                                        lambda: *lambda,
                                        args: vec![FfiValue::Integer(key as DwarfInteger)].into(),
                                        result: s.into(),
                                    };
                                    self.lambda_call.send(lambda_call).unwrap();
                                    let result = result.recv().unwrap();

                                    let mut guard = self.rows.lock().unwrap();
                                    guard.remove(key);

                                    result.unwrap()
                                })
                                .fetch_all(pool)
                                .await;

                            let result = match result {
                                Ok(result) => ROk(RBox::new(result.into())),
                                Err(e) => {
                                    let mut guard = self.errors.lock().unwrap();
                                    let entry = guard.vacant_entry();
                                    let key = entry.key();
                                    guard.insert(Arc::new(e));
                                    RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                }
                            };

                            Ok(FfiValue::Result(result))
                        }
                        "query_one" => {
                            let query: String = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let pool: DwarfInteger = args
                                .get(1)
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let FfiValue::Lambda(lambda) = args.get(2).unwrap() else {
                                panic!("Invalid lambda");
                            };

                            let guard = self.pools.lock().unwrap();
                            let pool = guard.get(pool as usize).unwrap();

                            let result = sqlx::query(&query)
                                .map(|row: sqlx::postgres::PgRow| {
                                    let (s, result) = crossbeam::channel::bounded(1);

                                    let key = {
                                        let mut guard = self.rows.lock().unwrap();
                                        guard.insert(Arc::new(row))
                                    };

                                    let lambda_call = LambdaCall {
                                        lambda: *lambda,
                                        args: vec![FfiValue::Integer(key as DwarfInteger)].into(),
                                        result: s.into(),
                                    };
                                    self.lambda_call.send(lambda_call).unwrap();
                                    let result = result.recv().unwrap();

                                    let mut guard = self.rows.lock().unwrap();
                                    guard.remove(key);

                                    result.unwrap()
                                })
                                .fetch_one(pool)
                                .await;

                            let result = match result {
                                Ok(result) => ROk(RBox::new(result.into())),
                                Err(e) => {
                                    let mut guard = self.errors.lock().unwrap();
                                    let entry = guard.vacant_entry();
                                    let key = entry.key();
                                    guard.insert(Arc::new(e));
                                    RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                }
                            };

                            Ok(FfiValue::Result(result))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Row" => match func.as_str() {
                        "get" => {
                            let row: DwarfInteger = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let index: String = args
                                .get(1)
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let ty: String = args
                                .get(2)
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let guard = self.rows.lock().unwrap();
                            let row = guard.get(row as usize).unwrap();

                            let result = match ty.as_str() {
                                "::sqlx::type::Integer" => {
                                    let result: i64 = row.get(index.as_str());
                                    FfiValue::Integer(result as DwarfInteger)
                                }
                                "::sqlx::type::Short" => {
                                    let result: i32 = row.get(index.as_str());
                                    FfiValue::Integer(result as DwarfInteger)
                                }
                                "::sqlx::type::String" => {
                                    let result: String = row.get(index.as_str());
                                    FfiValue::String(result.into())
                                }
                                "::sqlx::type::Timestamp" => {
                                    let result: chrono::NaiveDateTime = row.get(index.as_str());
                                    FfiValue::String(result.to_string().into())
                                }
                                ty => {
                                    panic!("Invalid type: {ty}");
                                }
                            };

                            Ok(result)
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Plugin(format!("Invalid type: {ty}").into())),
                }
                .into()
            })
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
                                .map_err(|e: ChaChaError| Error::Plugin(e.to_string().into()))
                                .unwrap();

                            let pool = PgPoolOptions::new()
                                .max_connections(5)
                                .connect(&connection_string)
                                .await;

                            let result = match pool {
                                Ok(pool) => {
                                    let mut guard = self.pools.lock().unwrap();
                                    let entry = guard.vacant_entry();
                                    let key = entry.key();
                                    guard.insert(pool.into());
                                    ROk(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                }
                                Err(e) => {
                                    let mut guard = self.errors.lock().unwrap();
                                    let entry = guard.vacant_entry();
                                    let key = entry.key();
                                    guard.insert(Arc::new(e));
                                    RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                }
                            };

                            tracing::trace!("open exit");
                            Ok(FfiValue::Result(result))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Plugin(format!("Invalid type: {ty}").into())),
                }
                .into()
            })
        }
    }
}
