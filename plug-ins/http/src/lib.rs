use std::{
    fmt::{self, Display},
    sync::Arc,
};

use abi_stable::{
    export_root_module,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::TD_Opaque,
    std_types::{RBox, RErr, ROk, RResult, RStr, RVec},
};
use async_compat::Compat;
use dwarf::{
    chacha::{error::ChaChaError, ffi_value::FfiValue},
    plug_in::{Error, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
    DwarfInteger,
};
use futures_lite::future;
use log::debug;
use reqwest::{Client, Error as RequestError, RequestBuilder, Response};
use slab::Slab;

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, id, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "http".into()
}

#[sabi_extern_fn]
pub fn id() -> RStr<'static> {
    "http".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    if let Some(FfiValue::String(plugin)) = args.first() {
        match plugin.as_str() {
            "http_client" => {
                let plugin = http_client::instantiate_root_module();
                let plugin = plugin.new();
                let plugin = plugin(vec![].into()).unwrap();
                ROk(Plugin_TO::from_value(plugin, TD_Opaque))
            }
            "http_server" => {
                let plugin = http_server::instantiate_root_module();
                let plugin = plugin.new();
                let plugin = plugin(vec![].into()).unwrap();
                ROk(Plugin_TO::from_value(plugin, TD_Opaque))
            }
            _ => RErr(Error::Uber(format!("Invalid plugin {plugin}").into())),
        }
    } else {
        RErr(Error::Uber("Invalid plugin".into()))
    }
    // ROk(Plugin_TO::from_value(Http, TD_Opaque))
}

#[derive(Clone, Debug)]
struct Http;

impl Display for Http {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Plugin for Http {
    fn name(&self) -> RStr<'_> {
        "Http".into()
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
                "http_client" => {
                    let client = http_client::instantiate_root_module();
                    let client = client.new();
                    let client = client(vec![].into()).unwrap();
                    Ok(FfiValue::PlugIn(client))
                }
                "http_server" => {
                    let server = http_server::instantiate_root_module();
                    let server = server.new();
                    let server = server(vec![].into()).unwrap();
                    Ok(FfiValue::PlugIn(server))
                }
                _ => Err(Error::Uber("Invalid module".into())),
            }
        })()
        .into()
    }
}

mod http_client {
    use super::*;

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, id, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "HttpClient".into()
    }

    #[sabi_extern_fn]
    pub fn id() -> RStr<'static> {
        "HttpClient".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(HttpClient::default(), TD_Opaque))
    }

    #[derive(Clone, Debug)]
    struct HttpClient {
        client: Client,
        requests: Slab<Arc<RequestBuilder>>,
        responses: Slab<Arc<Response>>,
        errors: Slab<Arc<RequestError>>,
    }

    impl Default for HttpClient {
        fn default() -> Self {
            Self {
                client: Client::new(),
                requests: Slab::new(),
                responses: Slab::new(),
                errors: Slab::new(),
            }
        }
    }

    impl Display for HttpClient {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Plugin for HttpClient {
        fn name(&self) -> RStr<'_> {
            "HttpClient".into()
        }

        #[tracing::instrument]
        fn invoke_func(
            &mut self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            future::block_on(Compat::new(async {
                match ty.as_str() {
                    "HttpClient" => match func.as_str() {
                        "get" => {
                            tracing::trace!("get enter");
                            let url: String = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                                .unwrap();

                            let request = self.client.get(url);
                            let entry = self.requests.vacant_entry();
                            let key = entry.key();
                            self.requests.insert(Arc::new(request));

                            tracing::trace!("get exit");
                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    "Request" => match func.as_str() {
                        "send" => {
                            tracing::trace!("send enter");
                            let key: DwarfInteger = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                                .unwrap();

                            let request = self.requests.remove(key as usize);
                            if let Some(request) = Arc::into_inner(request) {
                                let response = request.send().await;
                                let response = match response {
                                    Ok(response) => {
                                        let entry = self.responses.vacant_entry();
                                        let key = entry.key();
                                        self.responses.insert(Arc::new(response));
                                        ROk(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                    }
                                    Err(e) => {
                                        let entry = self.errors.vacant_entry();
                                        let key = entry.key();
                                        self.errors.insert(Arc::new(e));
                                        RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                    }
                                };
                                tracing::trace!("send exit");
                                Ok(FfiValue::Result(response))
                            } else {
                                tracing::trace!("send exit");
                                Ok(FfiValue::Error("Too many references to request.".into()))
                            }
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    "Response" => match func.as_str() {
                        "text" => {
                            tracing::trace!("text enter");
                            let key: DwarfInteger = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                                .unwrap();

                            let response = self.responses.remove(key as usize);
                            if let Some(response) = Arc::into_inner(response) {
                                let body = response.text().await;
                                let result = match body {
                                    Ok(body) => ROk(RBox::new(FfiValue::String(body.into()))),
                                    Err(e) => {
                                        let entry = self.errors.vacant_entry();
                                        let key = entry.key();
                                        self.errors.insert(Arc::new(e));
                                        RErr(RBox::new(FfiValue::Integer(key as DwarfInteger)))
                                    }
                                };
                                tracing::trace!("text exit");
                                Ok(FfiValue::Result(result))
                            } else {
                                tracing::trace!("text exit");
                                Ok(FfiValue::Error("Too many references to response.".into()))
                            }
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    "HttpError" => match func.as_str() {
                        "to_string" => {
                            tracing::trace!("to_string enter");
                            let key: DwarfInteger = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                                .unwrap();

                            let error = self.errors.remove(key as usize);
                            if let Some(error) = Arc::into_inner(error) {
                                let result =
                                    ROk(RBox::new(FfiValue::String(error.to_string().into())));
                                tracing::trace!("to_string exit");
                                Ok(FfiValue::Result(result))
                            } else {
                                tracing::trace!("to_string exit");
                                Ok(FfiValue::Error("Too many references to error.".into()))
                            }
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Uber(format!("Invalid type: {ty}").into())),
                }
                .into()
            }))
        }
    }
}

mod http_server {
    use super::*;

    // use std::sync::{Arc, Mutex};

    use dwarf::bubba::vm::run_lambda;
    // use once_cell::sync::OnceCell;
    // use rustc_hash::FxHashMap as HashMap;
    // use warp::Filter;

    // extern "C" {
    //     fn get_lambda_funcs() -> *const OnceCell<Arc<Mutex<HashMap<usize, Value>>>>;
    // }

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, id, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "HttpServer".into()
    }

    #[sabi_extern_fn]
    pub fn id() -> RStr<'static> {
        "HttpServer".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(HttpServer::default(), TD_Opaque))
    }

    #[derive(Clone, Debug)]
    struct HttpServer {
        // paths: Slab<Arc<dyn Filter<Extract = Tuple>>>,
    }

    impl Default for HttpServer {
        fn default() -> Self {
            Self {}
        }
    }

    impl Display for HttpServer {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl Plugin for HttpServer {
        fn name(&self) -> RStr<'_> {
            "HttpServer".into()
        }

        fn invoke_func(
            &mut self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            // let module_str = module.as_str();
            // debug!("module: {module_str}, type: {ty}, func: {func}, args: {args:?}");
            // Ok(FfiValue::Empty)
            future::block_on(Compat::new(async {
                match ty.as_str() {
                    "HttpServer" => match func.as_str() {
                        // "serve" => {
                        //     warp::serve(hello).run(([127, 0, 0, 1], 3030)).await;
                        //     Ok(FfiValue::Empty)
                        // }
                        "path" => {
                            // let path: String = args
                            //     .first()
                            //     .unwrap()
                            //     .try_into()
                            //     .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                            //     .unwrap();
                            let FfiValue::Lambda(number) = args.get(0).unwrap() else {
                                panic!("Invalid lambda");
                            };

                            // let hello =
                            //     warp::path(path)
                            //         .and(warp::path::param())
                            //         .map(|name: String| {
                            //             let mut args = RVec::new();
                            //             args.push(name.into());
                            //             run_lambda(*number, args)
                            //         });

                            let mut args = RVec::new();
                            args.push("uber".to_owned().into());

                            // let lambda_funcs = unsafe { get_lambda_funcs() };
                            // dbg!(&lambda_funcs);
                            // let foo = unsafe { &*lambda_funcs };
                            // dbg!(&foo);
                            // dbg!(foo.get());
                            run_lambda(*number, args);

                            Ok(FfiValue::Empty)
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Uber(format!("Invalid type: {ty}").into())),
                }
                .into()
            }))
        }
    }
}
