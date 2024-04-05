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
}

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
        _lambda_sender: RSender<LambdaCall>,
        _args: RVec<FfiValue>,
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
            &self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            future::block_on(Compat::new(async { Ok(FfiValue::Empty).into() }))
        }

        #[tracing::instrument]
        fn invoke_func_mut(
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

    use std::cell::RefCell;
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
    use hyper::{body::Incoming as IncomingBody, Request, Response};
    use hyper::{Method, Uri};
    use hyper_util::rt::TokioIo;
    use rustc_hash::FxHashMap as HashMap;
    use tokio::net::TcpListener;

    struct MyStr<'a>(&'a str);

    impl<'a> From<MyStr<'a>> for Method {
        fn from(s: MyStr) -> Self {
            match s {
                MyStr("GET") => Method::GET,
                MyStr("POST") => Method::POST,
                MyStr("PUT") => Method::PUT,
                MyStr("DELETE") => Method::DELETE,
                MyStr("HEAD") => Method::HEAD,
                MyStr("OPTIONS") => Method::OPTIONS,
                MyStr("CONNECT") => Method::CONNECT,
                MyStr("PATCH") => Method::PATCH,
                MyStr("TRACE") => Method::TRACE,
                _ => Method::GET,
            }
        }
    }

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
    struct Route {
        path: String,
        method: Method,
        lambda: usize,
    }

    #[derive(Clone, Debug)]
    struct HttpServer {
        // This is how we call lambdas from the plugin.
        lambda_call: RSender<LambdaCall>,
        requests: Arc<Mutex<Slab<Arc<Request<IncomingBody>>>>>,
        uris: Arc<Mutex<RefCell<Slab<Arc<Uri>>>>>,
        routes: Arc<Mutex<RefCell<HashMap<(String, Method), usize>>>>,
    }

    impl HttpServer {
        fn new(lambda_call: RSender<LambdaCall>) -> Self {
            Self {
                lambda_call,
                requests: Arc::new(Mutex::new(Slab::new())),
                uris: Arc::new(Mutex::new(RefCell::new(Slab::new()))),
                routes: Arc::new(Mutex::new(RefCell::new(HashMap::default()))),
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
            &self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            future::block_on(Compat::new(async {
                match ty.as_str() {
                    "HttpServer" => match func.as_str() {
                        "serve" => {
                            let FfiValue::Integer(port) = args.get(0).unwrap() else {
                                panic!("Invalid port");
                            };

                            let addr = SocketAddr::from(([127, 0, 0, 1], *port as u16));

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

                            println!("Listening on http://{}", addr);

                            loop {
                                let stream = listener.accept().await;
                                let (stream, _) = match stream {
                                    Ok(stream) => stream,
                                    Err(e) => {
                                        println!("Error accepting connection: {:?}", e);
                                        continue;
                                    }
                                };

                                let self_clone = self.clone();
                                let svc = Svc {
                                    server: RefCell::new(self_clone),
                                };
                                // Use an adapter to access something implementing `tokio::io`
                                // traits as if they implement `hyper::rt` IO traits.
                                let io = TokioIo::new(stream);
                                let svc_clone = svc.clone();

                                // Spawn a tokio task to serve multiple connections concurrently
                                tokio::task::spawn(async move {
                                    // Finally, we bind the incoming connection to our service
                                    if let Err(err) =
                                        http1::Builder::new().serve_connection(io, svc_clone).await
                                    {
                                        println!("Error serving connection: {:?}", err);
                                    }
                                });
                            }
                        }
                        "route" => {
                            let FfiValue::String(path) = args.get(0).unwrap() else {
                                panic!("Invalid path");
                            };

                            let FfiValue::String(method) = args.get(1).unwrap() else {
                                panic!("Invalid method");
                            };
                            let method = Method::from(MyStr(method.as_str()));

                            let FfiValue::Lambda(number) = args.get(2).unwrap() else {
                                panic!("Invalid lambda");
                            };

                            println!("adding route {} {}", path, method);

                            self.routes
                                .lock()
                                .unwrap()
                                .borrow_mut()
                                .insert((path.to_string(), method), *number);

                            Ok(FfiValue::Empty)
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

                            if let Some(request) = self.requests.lock().unwrap().get(key as usize) {
                                let uri = request.uri();
                                let key = self
                                    .uris
                                    .lock()
                                    .unwrap()
                                    .borrow_mut()
                                    .insert(Arc::new(uri.clone()));
                                Ok(FfiValue::Integer(key as DwarfInteger))
                            } else {
                                Err(Error::Uber("Invalid request".into()))
                            }
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

                            let guard = self.uris.lock().unwrap();
                            let guard = guard.borrow();
                            let uri = guard.get(key as usize).unwrap();
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

        fn invoke_func_mut(
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
                    // "HttpServer" => match func.as_str() {
                    //     "route" => {
                    //         let FfiValue::String(path) = args.get(0).unwrap() else {
                    //             panic!("Invalid path");
                    //         };

                    //         let FfiValue::String(method) = args.get(1).unwrap() else {
                    //             panic!("Invalid method");
                    //         };
                    //         let method = Method::from(MyStr(method.as_str()));

                    //         let FfiValue::Lambda(number) = args.get(2).unwrap() else {
                    //             panic!("Invalid lambda");
                    //         };

                    //         println!("adding route {} {}", path, method);

                    //         self.routes.insert((path.to_string(), method), *number);

                    //         Ok(FfiValue::Empty)
                    //     }
                    //     func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    // },
                    // "Request" => match func.as_str() {
                    //     "uri" => {
                    //         let key: DwarfInteger = args
                    //             .first()
                    //             .unwrap()
                    //             .try_into()
                    //             .map_err(|e: ChaChaError| Error::Uber(e.to_string().into()))
                    //             .unwrap();

                    //         let request = self.requests.get(key as usize).unwrap();
                    //         let uri = request.uri();
                    //         let key = self.uris.insert(Arc::new(uri.clone()));
                    //         Ok(FfiValue::Integer(key as DwarfInteger))
                    //     }
                    //     func => Err(Error::Uber(format!("Invalid function: {func}").into())),
                    // },
                    ty => Err(Error::Uber(format!("Invalid type: {ty}").into())),
                }
                .into()
            }))
        }
    }

    #[derive(Debug, Clone)]
    struct Svc {
        server: RefCell<HttpServer>,
    }

    impl Service<Request<IncomingBody>> for Svc {
        type Response = Response<Full<Bytes>>;
        type Error = hyper::Error;
        type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

        fn call(&self, req: Request<IncomingBody>) -> Self::Future {
            fn mk_response(s: String) -> Result<Response<Full<Bytes>>, hyper::Error> {
                Ok(Response::builder().body(Full::new(Bytes::from(s))).unwrap())
            }

            fn mk_not_found(s: String) -> Result<Response<Full<Bytes>>, hyper::Error> {
                Ok(Response::builder()
                    .status(404)
                    .body(Full::new(Bytes::from(s)))
                    .unwrap())
            }

            let path = req.uri().path().to_owned();
            let method = req.method().clone();

            let server = self.server.borrow_mut();
            let key = {
                let mut requests = server.requests.lock().unwrap();
                let entry = requests.vacant_entry();
                let key = entry.key();
                requests.insert(Arc::new(req));
                key
            };

            let guard = server.routes.lock().unwrap();

            let lambda_option = guard.borrow().get(&(path.clone(), method.clone())).cloned();
            if let Some(lambda) = lambda_option {
                let (s, result) = crossbeam::channel::bounded(1);

                let lambda_call = LambdaCall {
                    lambda: lambda,
                    args: vec![FfiValue::Integer(key as DwarfInteger)].into(),
                    result: s.into(),
                };
                server.lambda_call.send(lambda_call).unwrap();
                let result = result.recv().unwrap();

                let ROk(FfiValue::String(result)) = result else {
                    return Box::pin(async {
                        mk_response("oh no! something went terribly wrong. 🤯".into())
                    });
                };

                let mut requests = server.requests.lock().unwrap();
                requests.remove(key);

                Box::pin(async move { mk_response(result.to_string()) })
            } else if method == Method::GET {
                // We are going to tack a dot on the front of the path to sandbox it
                // to the CWD.
                let path = format!(".{path}");
                let path = std::path::Path::new(&path);
                if path.exists() {
                    if path.is_dir() {
                        let contents = "<p>Someday there will be a directory viewing page. For now, there's nothing to see here.</p>".to_owned();
                        Box::pin(async move { mk_response(contents) })
                    } else {
                        let contents = std::fs::read(path).unwrap();
                        Box::pin(async move {
                            Ok(Response::builder()
                                .body(Full::new(Bytes::from(contents)))
                                .unwrap())
                        })
                    }
                } else {
                    let path = path.display().to_string();
                    Box::pin(async move {
                        mk_not_found(format!("oops! {path} ({method}) not found").into())
                    })
                }
            } else {
                Box::pin(async move {
                    mk_not_found(format!("oh no! {path} ({method}) not found").into())
                })
            }
        }
    }
}
