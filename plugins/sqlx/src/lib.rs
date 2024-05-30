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
    bubba::error::BubbaError,
    chacha::ffi_value::FfiValue,
    plug_in::{Error, LambdaCall, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
    DwarfInteger,
};
use futures_lite::future;
use slab::Slab;

const INTEGER: &str = "::sqlx::type::Integer";
const SHORT: &str = "::sqlx::type::Short";
const STRING: &str = "::sqlx::type::String";
const TIMESTAMP: &str = "::sqlx::type::Timestamp";

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
    let plugin = postgres::instantiate_sub_module();
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

    pub fn instantiate_sub_module() -> PluginModRef {
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
                            let key: DwarfInteger = match args.first().unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let guard = self.errors.lock().unwrap();
                            let error = guard.get(key as usize).unwrap();

                            Ok(FfiValue::String(error.to_string().into()))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Map" => match func.as_str() {
                        "execute" => {
                            let query: String = match args.first().unwrap().try_into() {
                                Ok(query) => query,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let pool: DwarfInteger = match args.get(1).unwrap().try_into() {
                                Ok(pool) => pool,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let bindings: Vec<String> = match args.get(2).unwrap().try_into() {
                                Ok(bindings) => bindings,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let guard = self.pools.lock().unwrap();
                            let pool = guard.get(pool as usize).unwrap();

                            let mut result = sqlx::query(&query);
                            for binding in bindings {
                                result = result.bind(binding.to_string());
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
                    "Query" => {
                        match func.as_str() {
                            "query_all" => {
                                // The first parameter is the query string.
                                let query: String =
                                    match args.first().unwrap().try_into().map_err(
                                        |e: BubbaError| Error::Plugin(e.to_string().into()),
                                    ) {
                                        Ok(query) => query,
                                        Err(e) => return RErr(e),
                                    };

                                // The second parameter is a handle to the pool.
                                let pool: DwarfInteger = match args.get(1).unwrap().try_into() {
                                    Ok(pool) => pool,
                                    Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                                };

                                // The lambda to invoke on the result.
                                let FfiValue::Lambda(lambda) = args.get(2).unwrap() else {
                                    panic!("Invalid lambda");
                                };

                                let bindings: Vec<String> =
                                    match args.get(3).unwrap().try_into().map_err(
                                        |e: BubbaError| Error::Plugin(e.to_string().into()),
                                    ) {
                                        Ok(bindings) => bindings,
                                        Err(e) => return RErr(e),
                                    };

                                // Dereference the pool handle
                                let guard = self.pools.lock().unwrap();
                                let pool = guard.get(pool as usize).unwrap();

                                // Build the query
                                let mut result = sqlx::query(&query);

                                // Add bindings to the query
                                for binding in bindings {
                                    result = result.bind(binding.to_string());
                                }

                                // Run the query, mapping results to the lambda.
                                let result = result
                                    .map(|row: sqlx::postgres::PgRow| {
                                        // This is how we get the result of running the lambda.
                                        let (s, result) = crossbeam::channel::bounded(1);

                                        // Store the result locally. We can''t return the row directly --
                                        // we can only return `FfiValue`''s.
                                        let key = {
                                            let mut guard = self.rows.lock().unwrap();
                                            guard.insert(Arc::new(row))
                                        };

                                        // Invoke the lambda, passing a handle to the row.
                                        let lambda_call = LambdaCall {
                                            lambda: *lambda,
                                            args: vec![FfiValue::Integer(key as DwarfInteger)]
                                                .into(),
                                            result: s.into(),
                                        };
                                        self.lambda_call.send(lambda_call).unwrap();

                                        // Wait for the result.
                                        let result = result.recv().unwrap();

                                        // Remove the row.
                                        let mut guard = self.rows.lock().unwrap();
                                        guard.remove(key);

                                        // Return the result from the lambda.
                                        result.unwrap()
                                    })
                                    .fetch_all(pool)
                                    .await;

                                // Now we wrap the result up as an RResult that may be sent back
                                // to dwarf.
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
                                let query: String =
                                    match args.first().unwrap().try_into().map_err(
                                        |e: BubbaError| Error::Plugin(e.to_string().into()),
                                    ) {
                                        Ok(query) => query,
                                        Err(e) => return RErr(e),
                                    };

                                let pool: DwarfInteger = match args.get(1).unwrap().try_into() {
                                    Ok(pool) => pool,
                                    Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                                };

                                let FfiValue::Lambda(lambda) = args.get(2).unwrap() else {
                                    panic!("Invalid lambda");
                                };

                                let bindings: Vec<String> = match args.get(3).unwrap().try_into() {
                                    Ok(bindings) => bindings,
                                    Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                                };

                                let guard = self.pools.lock().unwrap();
                                let pool = guard.get(pool as usize).unwrap();

                                // Run the query
                                let mut result = sqlx::query(&query);
                                for binding in bindings {
                                    result = result.bind(binding.to_string());
                                }
                                let result = result
                                    .map(|row: sqlx::postgres::PgRow| {
                                        let (s, result) = crossbeam::channel::bounded(1);

                                        let key = {
                                            let mut guard = self.rows.lock().unwrap();
                                            guard.insert(Arc::new(row))
                                        };

                                        let lambda_call = LambdaCall {
                                            lambda: *lambda,
                                            args: vec![FfiValue::Integer(key as DwarfInteger)]
                                                .into(),
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
                        }
                    }
                    "Row" => {
                        match func.as_str() {
                            "get" => {
                                // The first parameter is the row handle.
                                let row: DwarfInteger = match args.first().unwrap().try_into() {
                                    Ok(row) => row,
                                    Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                                };

                                // The second parameter is the name of the column.
                                let index: String = match args.get(1).unwrap().try_into() {
                                    Ok(index) => index,
                                    Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                                };

                                // The third parameter is the type of the column.
                                let ty: String = match args.get(2).unwrap().try_into() {
                                    Ok(ty) => ty,
                                    Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                                };

                                let guard = self.rows.lock().unwrap();
                                let row = guard.get(row as usize).unwrap();

                                // Attempt to get the value from the row, according to the presumed
                                // type.
                                let result = match ty.as_str() {
                                    INTEGER => match row.try_get::<i64, &str>(index.as_str()) {
                                        Ok(result) => FfiValue::Result(ROk(RBox::new(
                                            FfiValue::Integer(result as DwarfInteger),
                                        ))),
                                        Err(e) => {
                                            let mut guard = self.errors.lock().unwrap();
                                            let entry = guard.vacant_entry();
                                            let key = entry.key();
                                            guard.insert(Arc::new(e));
                                            FfiValue::Result(RErr(RBox::new(FfiValue::Integer(
                                                key as DwarfInteger,
                                            ))))
                                        }
                                    },
                                    SHORT => match row.try_get::<i32, &str>(index.as_str()) {
                                        Ok(result) => FfiValue::Result(ROk(RBox::new(
                                            FfiValue::Integer(result as DwarfInteger),
                                        ))),
                                        Err(e) => {
                                            let mut guard = self.errors.lock().unwrap();
                                            let entry = guard.vacant_entry();
                                            let key = entry.key();
                                            guard.insert(Arc::new(e));
                                            FfiValue::Result(RErr(RBox::new(FfiValue::Integer(
                                                key as DwarfInteger,
                                            ))))
                                        }
                                    },
                                    STRING => match row.try_get::<String, &str>(index.as_str()) {
                                        Ok(result) => FfiValue::Result(ROk(RBox::new(
                                            FfiValue::String(result.into()),
                                        ))),
                                        Err(e) => {
                                            let mut guard = self.errors.lock().unwrap();
                                            let entry = guard.vacant_entry();
                                            let key = entry.key();
                                            guard.insert(Arc::new(e));
                                            FfiValue::Result(RErr(RBox::new(FfiValue::Integer(
                                                key as DwarfInteger,
                                            ))))
                                        }
                                    },
                                    TIMESTAMP => {
                                        match row
                                            .try_get::<chrono::NaiveDateTime, &str>(index.as_str())
                                        {
                                            Ok(result) => FfiValue::Result(ROk(RBox::new(
                                                FfiValue::String(result.to_string().into()),
                                            ))),
                                            Err(e) => {
                                                let mut guard = self.errors.lock().unwrap();
                                                let entry = guard.vacant_entry();
                                                let key = entry.key();
                                                guard.insert(Arc::new(e));
                                                FfiValue::Result(RErr(RBox::new(
                                                    FfiValue::Integer(key as DwarfInteger),
                                                )))
                                            }
                                        }
                                    }
                                    ty => {
                                        panic!("Invalid type: {ty}");
                                    }
                                };

                                Ok(result)
                            }
                            func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                        }
                    }
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
                    "Sqlx" => {
                        match func.as_str() {
                            "connect" => {
                                tracing::trace!("connect enter");
                                let connection_string: String =
                                    match args.first().unwrap().try_into().map_err(
                                        |e: BubbaError| Error::Plugin(e.to_string().into()),
                                    ) {
                                        Ok(connection_string) => connection_string,
                                        Err(e) => return RErr(e),
                                    };

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
                        }
                    }
                    ty => Err(Error::Plugin(format!("Invalid type: {ty}").into())),
                }
                .into()
            })
        }
    }
}
