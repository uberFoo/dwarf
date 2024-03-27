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
use async_compat::Compat;
use dwarf::{
    chacha::{error::ChaChaError, ffi_value::FfiValue},
    plug_in::{Error, LambdaCall, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
    DwarfInteger,
};
use futures_lite::future;
use reqwest::{Client, Error as RequestError, RequestBuilder, Response};
use slab::Slab;

#[export_root_module]
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "http".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(lambda_sender: RSender<LambdaCall>, args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    if let Some(FfiValue::String(plugin)) = args.first() {
        match plugin.as_str() {
            "http_client" => {
                let plugin = http_client::instantiate_root_module();
                let plugin = plugin.new();
                let plugin = plugin(lambda_sender, vec![].into()).unwrap();
                ROk(Plugin_TO::from_value(plugin, TD_Opaque))
            }
            "http_server" => {
                let plugin = http_server::instantiate_root_module();
                let plugin = plugin.new();
                let plugin = plugin(lambda_sender, vec![].into()).unwrap();
                ROk(Plugin_TO::from_value(plugin, TD_Opaque))
            }
            _ => RErr(Error::Uber(format!("Invalid plugin {plugin}").into())),
        }
    } else {
        RErr(Error::Uber("Invalid plugin".into()))
    }
    // ROk(Plugin_TO::from_value(Http, TD_Opaque))
}

// #[derive(Clone, Debug)]
// struct Http;

// impl Display for Http {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{:?}", self)
//     }
// }

// impl Plugin for Http {
//     fn name(&self) -> RStr<'_> {
//         "Http".into()
//     }

//     fn invoke_func(
//         &mut self,
//         module: RStr<'_>,
//         ty: RStr<'_>,
//         func: RStr<'_>,
//         args: RVec<FfiValue>,
//     ) -> RResult<FfiValue, Error> {
//         (|| -> Result<FfiValue, Error> {
//             let module_str = module.as_str();
//             debug!("module: {module_str}, type: {ty}, func: {func}, args: {args:?}");
//             match module_str {
//                 "http_client" => {
//                     let client = http_client::instantiate_root_module();
//                     let client = client.new();
//                     let client = client(vec![].into()).unwrap();
//                     Ok(FfiValue::PlugIn(client))
//                 }
//                 "http_server" => {
//                     let server = http_server::instantiate_root_module();
//                     let server = server.new();
//                     let server = server(vec![].into()).unwrap();
//                     Ok(FfiValue::PlugIn(server))
//                 }
//                 _ => Err(Error::Uber("Invalid module".into())),
//             }
//         })()
//         .into()
//     }
// }

mod http_client {
    use super::*;

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "HttpClient".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(
        lambda_sender: RSender<LambdaCall>,
        args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
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

    use std::convert::Infallible;
    use std::future::Future;
    use std::net::SocketAddr;
    use std::pin::Pin;
    use std::sync::{Arc, Mutex};

    use http_body_util::Full;
    use hyper::body::Bytes;
    use hyper::server::conn::http1;
    use hyper::service::service_fn;
    use hyper::service::Service;
    use hyper::Uri;
    use hyper::{body::Incoming as IncomingBody, Request, Response};
    use hyper_util::rt::TokioIo;
    use tokio::net::TcpListener;

    type Counter = i32;

    pub fn instantiate_root_module() -> PluginModRef {
        PluginModule { name, new }.leak_into_prefix()
    }

    #[sabi_extern_fn]
    pub fn name() -> RStr<'static> {
        "HttpServer".into()
    }

    /// Instantiates the plugin.
    #[sabi_extern_fn]
    pub fn new(
        lambda_sender: RSender<LambdaCall>,
        _args: RVec<FfiValue>,
    ) -> RResult<PluginType, Error> {
        ROk(Plugin_TO::from_value(
            HttpServer::new(lambda_sender),
            TD_Opaque,
        ))
    }

    #[derive(Clone, Debug)]
    struct HttpServer {
        // This is how we call lambdas from the plugin.
        lambda_call: RSender<LambdaCall>,
        requests: Slab<Arc<Request<IncomingBody>>>,
        uris: Slab<Arc<Uri>>,
    }

    impl HttpServer {
        fn new(lambda_call: RSender<LambdaCall>) -> Self {
            Self {
                lambda_call,
                requests: Slab::new(),
                uris: Slab::new(),
            }
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
                        "serve" => {
                            let addr = SocketAddr::from(([127, 0, 0, 1], 3000));

                            let listener_result = TcpListener::bind(addr).await;
                            let listener = match listener_result {
                                Ok(listener) => listener,
                                Err(e) => {
                                    return Err(Error::Uber(
                                        format!("Failed to bind TCP listener: {}", e).into(),
                                    ))
                                    .into()
                                }
                            };

                            loop {
                                let stream = listener.accept().await;
                                let (stream, _) = match stream {
                                    Ok(stream) => stream,
                                    Err(e) => {
                                        println!("Error accepting connection: {:?}", e);
                                        continue;
                                    }
                                };

                                println!("Listening on http://{}", addr);

                                let svc = Svc {
                                    counter: Arc::new(Mutex::new(0)),
                                };

                                // Use an adapter to access something implementing `tokio::io`
                                // traits as if they implement `hyper::rt` IO traits.
                                let io = TokioIo::new(stream);
                                let svc_clone = svc.clone();

                                // Spawn a tokio task to serve multiple connections concurrently
                                tokio::task::spawn(async move {
                                    // Finally, we bind the incoming connection to our `hello` service
                                    if let Err(err) = http1::Builder::new()
                                        // `service_fn` converts our function in a `Service`
                                        .serve_connection(io, svc_clone)
                                        .await
                                    {
                                        println!("Error serving connection: {:?}", err);
                                    }
                                });
                            }
                        }
                        "route" => {
                            let FfiValue::Lambda(number) = args.get(0).unwrap() else {
                                panic!("Invalid lambda");
                            };

                            let mut args = RVec::new();
                            args.push("uber".to_owned().into());

                            let (s, result) = crossbeam::channel::bounded(1);

                            let lambda_call = LambdaCall {
                                lambda: *number,
                                args,
                                result: s.into(),
                            };
                            self.lambda_call.send(lambda_call).unwrap();
                            let result = result.recv().unwrap();
                            dbg!(&result);

                            <RResult<FfiValue, Error> as Into<Result<FfiValue, Error>>>::into(
                                result,
                            )
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    "Request" => match func.as_str() {
                        "uri" => {
                            let key: DwarfInteger = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                                .unwrap();

                            let request = self.requests.get(key as usize).unwrap();
                            let uri = request.uri();
                            let key = self.uris.insert(Arc::new(uri.clone()));
                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    "Uri" => match func.as_str() {
                        "path" => {
                            let key: DwarfInteger = args
                                .first()
                                .unwrap()
                                .try_into()
                                .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                                .unwrap();

                            let uri = self.uris.get(key as usize).unwrap();
                            let path = uri.path().to_string();
                            Ok(FfiValue::String(path.into()))
                        }
                        func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Uber(format!("Invalid type: {ty}").into())),
                }
                .into()
            }))
        }
    }

    async fn hello(_: Request<hyper::body::Incoming>) -> Result<Response<Full<Bytes>>, Infallible> {
        Ok(Response::new(Full::new(Bytes::from("Hello, World!"))))
    }

    #[derive(Debug, Clone)]
    struct Svc {
        counter: Arc<Mutex<Counter>>,
    }

    impl Service<Request<IncomingBody>> for Svc {
        type Response = Response<Full<Bytes>>;
        type Error = hyper::Error;
        type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

        fn call(&self, req: Request<IncomingBody>) -> Self::Future {
            fn mk_response(s: String) -> Result<Response<Full<Bytes>>, hyper::Error> {
                Ok(Response::builder().body(Full::new(Bytes::from(s))).unwrap())
            }

            if req.uri().path() != "/favicon.ico" {
                *self.counter.lock().expect("lock poisoned") += 1;
            }

            let res = match req.uri().path() {
                "/" => mk_response(format!("home! counter = {:?}", self.counter)),
                "/posts" => mk_response(format!("posts, of course! counter = {:?}", self.counter)),
                "/authors" => mk_response(format!(
                    "authors extraordinaire! counter = {:?}",
                    self.counter
                )),
                // Return the 404 Not Found for other routes, and don't increment counter.
                _ => return Box::pin(async { mk_response("oh no! not found".into()) }),
            };

            Box::pin(async { res })
        }
    }
}
