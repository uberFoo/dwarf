use std::{
    env,
    fmt::{self, Display},
    fs, io,
    path::PathBuf,
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
    chacha::ffi_value::FfiValue,
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
            "client" => {
                let plugin = http_client::instantiate_sub_module();
                let plugin = plugin.new();
                let plugin = plugin(lambda_sender, vec![].into()).unwrap();
                ROk(Plugin_TO::from_value(plugin, TD_Opaque))
            }
            "server" => {
                let plugin = http_server::instantiate_sub_module();
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

mod http_client {
    use super::*;

    pub fn instantiate_sub_module() -> PluginModRef {
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

    /// Note that the things dangling off here need to be wrapped in `Arc`'s so
    /// that they can be cloned. We need to be Clone to satisfy the TD_Opaque
    /// bound for the plugin stuff.
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
                            let url: String = match args.first().unwrap().try_into() {
                                Ok(url) => url,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let request = self.client.get(url);

                            let entry = self.requests.vacant_entry();
                            let key = entry.key();
                            self.requests.insert(Arc::new(request));

                            tracing::trace!("get exit");
                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        "post" => {
                            tracing::trace!("post enter");
                            let url: String = match args.get(0).unwrap().try_into() {
                                Ok(url) => url,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let request = self.client.post(url);

                            let entry = self.requests.vacant_entry();
                            let key = entry.key();
                            self.requests.insert(Arc::new(request));

                            tracing::trace!("post exit");
                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Request" => match func.as_str() {
                        "header" => {
                            tracing::trace!("header enter");
                            let key: DwarfInteger = match args.get(0).unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let header: String = match args.get(1).unwrap().try_into() {
                                Ok(header) => header,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let value: String = match args.get(2).unwrap().try_into() {
                                Ok(value) => value,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let request = self.requests.remove(key as usize);
                            let request = Arc::try_unwrap(request).unwrap();
                            let request = request.header(header, value);

                            let entry = self.requests.vacant_entry();
                            let key = entry.key();
                            self.requests.insert(Arc::new(request));

                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        "send" => {
                            tracing::trace!("send enter");
                            let key: DwarfInteger = match args.get(0).unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

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
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Response" => match func.as_str() {
                        "text" => {
                            tracing::trace!("text enter");
                            let key: DwarfInteger = match args.get(0).unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

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
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "HttpError" => match func.as_str() {
                        "to_string" => {
                            tracing::trace!("to_string enter");
                            let key: DwarfInteger = match args.get(0).unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

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
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Plugin(format!("Invalid type: {ty}").into())),
                }
                .into()
            }))
        }
    }
}

mod http_server {
    use super::*;

    use std::cell::RefCell;
    use std::future::Future;
    use std::net::SocketAddr;
    use std::pin::Pin;
    use std::sync::{Arc, Mutex};

    use http_body_util::Full;
    use hyper::body::{Body, Bytes};
    use hyper::header::{HeaderValue, CONTENT_TYPE};
    use hyper::server::conn::http1;
    use hyper::service::Service;
    use hyper::{body::Incoming as IncomingBody, Request, Response};
    use hyper::{Method, Uri};
    use hyper_util::rt::{TokioExecutor, TokioIo};
    use hyper_util::server::conn::auto::Builder;
    use rustc_hash::FxHashMap as HashMap;
    use rustls::ServerConfig;
    use rustls_pki_types::{CertificateDer, PrivateKeyDer};
    use tokio::net::TcpListener;
    use tokio_rustls::TlsAcceptor;

    const EXTENSION_DIR: &str = "extensions";
    const PLUGIN_DIR: &str = "http";
    const MISC_DIR: &str = "misc";
    const HTML_404: &str = "404.html";
    const CSS_404: &str = "404.css";
    const WEBP_404: &str = "404.webp";

    struct MethodStr<'a>(&'a str);

    impl<'a> From<MethodStr<'a>> for Method {
        fn from(s: MethodStr) -> Self {
            match s {
                MethodStr("GET") => Method::GET,
                MethodStr("POST") => Method::POST,
                MethodStr("PUT") => Method::PUT,
                MethodStr("DELETE") => Method::DELETE,
                MethodStr("HEAD") => Method::HEAD,
                MethodStr("OPTIONS") => Method::OPTIONS,
                MethodStr("CONNECT") => Method::CONNECT,
                MethodStr("PATCH") => Method::PATCH,
                MethodStr("TRACE") => Method::TRACE,
                _ => Method::GET,
            }
        }
    }

    struct ResponseStr<'a>(&'a str);
    impl<'a> From<ResponseStr<'a>> for ResponseType {
        fn from(s: ResponseStr) -> Self {
            match s {
                ResponseStr("text") => ResponseType::Text,
                ResponseStr("json") => ResponseType::Json,
                _ => ResponseType::Text,
            }
        }
    }

    pub fn instantiate_sub_module() -> PluginModRef {
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
    enum ResponseType {
        Json,
        Text,
    }

    #[derive(Clone, Debug)]
    struct HttpServer {
        // This is how we call lambdas from the plugin.
        lambda_call: RSender<LambdaCall>,
        requests: Arc<Mutex<Slab<Arc<Request<IncomingBody>>>>>,
        uris: Arc<Mutex<RefCell<Slab<Arc<Uri>>>>>,
        routes: Arc<Mutex<RefCell<HashMap<(String, Method), usize>>>>,
        prefix_routes: Arc<Mutex<RefCell<HashMap<(String, Method), usize>>>>,
        tls: Arc<
            Mutex<
                RefCell<
                    Option<(
                        // Cert
                        Vec<CertificateDer<'static>>,
                        // Key
                        PrivateKeyDer<'static>,
                    )>,
                >,
            >,
        >,
        strings: Arc<Mutex<Slab<String>>>,
        response_builders: Arc<Mutex<Slab<Option<hyper::http::response::Builder>>>>,
        responses: Arc<Mutex<Slab<Response<Full<Bytes>>>>>,
    }

    impl HttpServer {
        fn new(lambda_call: RSender<LambdaCall>) -> Self {
            Self {
                lambda_call,
                requests: Arc::new(Mutex::new(Slab::new())),
                uris: Arc::new(Mutex::new(RefCell::new(Slab::new()))),
                routes: Arc::new(Mutex::new(RefCell::new(HashMap::default()))),
                prefix_routes: Arc::new(Mutex::new(RefCell::new(HashMap::default()))),
                tls: Arc::new(Mutex::new(RefCell::new(None))),
                strings: Arc::new(Mutex::new(Slab::new())),
                response_builders: Arc::new(Mutex::new(Slab::new())),
                responses: Arc::new(Mutex::new(Slab::new())),
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
            _module: RStr<'_>,
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

                            let addr = SocketAddr::from(([0, 0, 0, 0], *port as u16));

                            let listener_result = TcpListener::bind(addr).await;
                            let listener = match listener_result {
                                Ok(listener) => listener,
                                Err(e) => {
                                    return Err(Error::Plugin(
                                        format!("Failed to bind TCP listener: {}", e).into(),
                                    ))
                                    .into()
                                }
                            };

                            let tls_acceptor = if let Some((certs, key)) =
                                self.tls.lock().unwrap().borrow_mut().take()
                            {
                                let _ =
                                    rustls::crypto::aws_lc_rs::default_provider().install_default();
                                let mut server_config = ServerConfig::builder()
                                    .with_no_client_auth()
                                    .with_single_cert(certs, key)
                                    .map_err(|e| Error::Plugin(e.to_string().into()))
                                    .unwrap();
                                server_config.alpn_protocols = vec![
                                    b"h2".to_vec(),
                                    b"http/1.1".to_vec(),
                                    b"http/1.0".to_vec(),
                                ];
                                Some(TlsAcceptor::from(Arc::new(server_config)))
                            } else {
                                None
                            };

                            println!("Listening on http://{}", addr);

                            if let Some(tls_acceptor) = tls_acceptor {
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
                                    let svc_clone = svc.clone();

                                    let tls_acceptor = tls_acceptor.clone();
                                    tokio::spawn(async move {
                                        let tls_stream = match tls_acceptor.accept(stream).await {
                                            Ok(tls_stream) => tls_stream,
                                            Err(err) => {
                                                eprintln!(
                                                    "failed to perform tls handshake: {err:#}"
                                                );
                                                return;
                                            }
                                        };
                                        if let Err(err) = Builder::new(TokioExecutor::new())
                                            .serve_connection(TokioIo::new(tls_stream), svc_clone)
                                            .await
                                        {
                                            eprintln!("failed to serve connection: {err:#}");
                                        }
                                    });
                                }
                            } else {
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
                                        if let Err(err) = http1::Builder::new()
                                            .serve_connection(io, svc_clone)
                                            .await
                                        {
                                            println!("Error serving connection: {:?}", err);
                                        }
                                    });
                                }
                            }
                        }
                        "route" => {
                            let FfiValue::String(path) = args.get(0).unwrap() else {
                                panic!("Invalid path");
                            };

                            let FfiValue::String(method) = args.get(1).unwrap() else {
                                panic!("Invalid method");
                            };
                            let method = Method::from(MethodStr(method.as_str()));

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
                        "prefix_route" => {
                            let FfiValue::String(path) = args.get(0).unwrap() else {
                                panic!("Invalid path");
                            };

                            let FfiValue::String(method) = args.get(1).unwrap() else {
                                panic!("Invalid method");
                            };
                            let method = Method::from(MethodStr(method.as_str()));

                            let FfiValue::Lambda(number) = args.get(2).unwrap() else {
                                panic!("Invalid lambda");
                            };

                            println!("adding route {} {}", path, method);

                            self.prefix_routes
                                .lock()
                                .unwrap()
                                .borrow_mut()
                                .insert((path.to_string(), method), *number);

                            Ok(FfiValue::Empty)
                        }
                        "use_tls" => {
                            let cert: String = match args.get(0).unwrap().try_into() {
                                Ok(cert) => cert,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let key: String = match args.get(1).unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let cert_file = fs::File::open(cert.clone())
                                .map_err(|e| {
                                    Error::Plugin(format!("failed to open {}: {}", cert, e).into())
                                })
                                .unwrap();
                            let mut reader = io::BufReader::new(cert_file);

                            // Load and return certificate.
                            let cert: Vec<CertificateDer<'static>> =
                                rustls_pemfile::certs(&mut reader)
                                    .collect::<io::Result<Vec<CertificateDer<'static>>>>()
                                    .unwrap();

                            let key_file = fs::File::open(key.clone())
                                .map_err(|e| {
                                    Error::Plugin(format!("failed to open {}: {}", key, e).into())
                                })
                                .unwrap();
                            let mut reader = io::BufReader::new(key_file);

                            // Load and return a single private key.
                            let key: PrivateKeyDer<'static> =
                                rustls_pemfile::private_key(&mut reader)
                                    .map(|key| key.unwrap())
                                    .unwrap();

                            *self.tls.lock().unwrap().borrow_mut() = Some((cert, key));

                            Ok(FfiValue::Empty)
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Response" => match func.as_str() {
                        "new" => {
                            let response = Response::builder();
                            let mut guard = self.response_builders.lock().unwrap();
                            let entry = guard.vacant_entry();
                            let key = entry.key();
                            guard.insert(Some(response));

                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        "status" => {
                            let key: DwarfInteger = match args.get(0).unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };
                            let status: DwarfInteger = match args.get(1).unwrap().try_into() {
                                Ok(status) => status,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let mut guard = self.response_builders.lock().unwrap();
                            if let Some(option) = guard.get_mut(key as usize) {
                                let response = option.take().unwrap();
                                let response = response.status(status as u16);
                                *option = Some(response);

                                Ok(FfiValue::Empty)
                            } else {
                                Err(Error::Plugin("Invalid response".into()))
                            }
                        }
                        "body" => {
                            let key: DwarfInteger = match args.get(0).unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };
                            let body: String = match args.get(1).unwrap().try_into() {
                                Ok(body) => body,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let mut guard = self.response_builders.lock().unwrap();
                            let option = guard.get_mut(key as usize).unwrap();
                            let response = option.take().unwrap();
                            let response = response.body(Full::new(Bytes::from(body)));
                            guard.remove(key as usize);

                            let mut guard = self.responses.lock().unwrap();
                            let entry = guard.vacant_entry();
                            let key = entry.key();
                            guard.insert(response.unwrap());

                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        "json" => {
                            let key: DwarfInteger = match args.first().unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };
                            let json: String = match args.get(1).unwrap().try_into() {
                                Ok(json) => json,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let mut guard = self.response_builders.lock().unwrap();
                            let option = guard.get_mut(key as usize).unwrap();
                            let response = option.take().unwrap();
                            let response = response
                                .header(CONTENT_TYPE, HeaderValue::from_static("application/json"));
                            let response = response.body(Full::new(Bytes::from(json)));
                            guard.remove(key as usize);

                            let mut guard = self.responses.lock().unwrap();
                            let entry = guard.vacant_entry();
                            let key = entry.key();
                            guard.insert(response.unwrap());

                            Ok(FfiValue::Integer(key as DwarfInteger))
                        }
                        "set_header" => {
                            let key: DwarfInteger = match args.first().unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };
                            let header: String = match args.get(1).unwrap().try_into() {
                                Ok(header) => header,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };
                            let value: String = match args.get(2).unwrap().try_into() {
                                Ok(value) => value,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let mut guard = self.response_builders.lock().unwrap();
                            let option = guard.get_mut(key as usize).unwrap();
                            let response = option.take().unwrap();
                            let response = response.header(header, value);
                            *option = Some(response);

                            Ok(FfiValue::Empty)
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Request" => match func.as_str() {
                        "uri" => {
                            let key: DwarfInteger = match args.first().unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

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
                                Err(Error::Plugin("Invalid request".into()))
                            }
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Suffix" => match func.as_str() {
                        "to_string" => {
                            let key: DwarfInteger = match args.first().unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let guard = self.strings.lock().unwrap();
                            let string = guard.get(key as usize).unwrap();

                            Ok(FfiValue::String(string.to_owned().into()))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    "Uri" => match func.as_str() {
                        "path" => {
                            let key: DwarfInteger = match args.first().unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let guard = self.uris.lock().unwrap();
                            let guard = guard.borrow();
                            let uri = guard.get(key as usize).unwrap();
                            let path = uri.path().to_string();
                            Ok(FfiValue::String(path.into()))
                        }
                        "query" => {
                            let key: DwarfInteger = match args.first().unwrap().try_into() {
                                Ok(key) => key,
                                Err(e) => return RErr(Error::Plugin(e.to_string().into())),
                            };

                            let guard = self.uris.lock().unwrap();
                            let guard = guard.borrow();
                            let uri = guard.get(key as usize).unwrap();
                            let query_string = uri.query().unwrap_or("");
                            let params: HashMap<String, String> = query_string
                                .split('&')
                                .filter_map(|part| {
                                    let mut split = part.split('=');
                                    let key = split.next()?;
                                    let value = split.next()?;
                                    Some((key.to_string(), value.to_string()))
                                })
                                .collect();

                            let value = params.into();

                            Ok(FfiValue::Map(value))
                        }
                        func => Err(Error::Plugin(format!("Invalid function: {func}").into())),
                    },
                    ty => Err(Error::Plugin(format!("Invalid type: {ty}").into())),
                }
                .into()
            }))
        }

        fn invoke_func_mut(
            &mut self,
            module: RStr<'_>,
            ty: RStr<'_>,
            func: RStr<'_>,
            _args: RVec<FfiValue>,
        ) -> RResult<FfiValue, Error> {
            RErr(Error::Plugin(
                format!("Invalid function: {module}::{ty}::{func}").into(),
            ))
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
                Ok(Response::builder()
                    .header("Content-Type", "text/plain; charset=utf-8")
                    .body(Full::new(Bytes::from(s)))
                    .unwrap())
            }

            fn mk_not_found(s: String) -> Result<Response<Full<Bytes>>, hyper::Error> {
                Ok(Response::builder()
                    .status(404)
                    .body(Full::new(Bytes::from(s)))
                    .unwrap())
            }

            fn mk_webp_response(s: Vec<u8>) -> Result<Response<Full<Bytes>>, hyper::Error> {
                Ok(Response::builder()
                    .header("Content-Type", "image/webp")
                    .body(Full::new(Bytes::from(s)))
                    .unwrap())
            }

            fn mk_css_response(s: String) -> Result<Response<Full<Bytes>>, hyper::Error> {
                Ok(Response::builder()
                    .header("Content-Type", "text/css; charset=utf-8")
                    .body(Full::new(Bytes::from(s)))
                    .unwrap())
            }

            let path = req.uri().path().to_owned();
            let method = req.method().clone();

            let server = self.server.borrow_mut();
            let request_handle = {
                let mut requests = server.requests.lock().unwrap();
                let entry = requests.vacant_entry();
                let key = entry.key();
                requests.insert(Arc::new(req));
                key
            };

            // This is setup for the static routes.
            let guard = server.routes.lock().unwrap();
            // Here we pick up the lambda based on the path and method.
            let lambda_option = guard.borrow().get(&(path.clone(), method.clone())).cloned();

            // This is the setup for the prefix routes.
            let p = PathBuf::from(&path);
            let suffix = if let Some(file_name) = p.file_name() {
                file_name.to_str().unwrap()
            } else {
                ""
            };
            let prefix = if let Some(parent) = p.parent() {
                parent.to_str().unwrap()
            } else {
                ""
            };
            let prefix_guard = server.prefix_routes.lock().unwrap();
            // Here we pick up the lambda based on the path and method.
            let prefix_lambda_option = prefix_guard
                .borrow()
                .get(&(prefix.to_owned(), method.clone()))
                .cloned();

            // We are going to tack a dot on the front of the path to sandbox it
            // to the the files subdirectory.
            let file_path = format!("../files{path}");
            let file_path = std::path::Path::new(&file_path);

            if path != "/" && file_path.exists() && method == Method::GET {
                if file_path.is_dir() {
                    let contents = "<p>Someday there may be a directory viewing page. For now, there's nothing to see here.</p>".to_owned();
                    Box::pin(async move { mk_response(contents) })
                } else {
                    let contents = std::fs::read(file_path).unwrap();
                    Box::pin(async move {
                        Ok(Response::builder()
                            .body(Full::new(Bytes::from(contents)))
                            .unwrap())
                    })
                }
            } else if let Some(lambda) = lambda_option {
                // Invoke the lambda passing the handle to the request.
                let response = match invoke_lambda(
                    lambda,
                    vec![FfiValue::Integer(request_handle as DwarfInteger)].into(),
                    &server,
                ) {
                    Ok(result) => result,
                    Err(e) => {
                        return Box::pin(async { mk_response(e) });
                    }
                };

                let mut requests = server.requests.lock().unwrap();
                requests.remove(request_handle);

                let mut responses = server.responses.lock().unwrap();
                let response = responses.remove(response as usize);

                Box::pin(async { Ok(response) })
            } else if let Some(lambda) = prefix_lambda_option {
                let suffix = {
                    let mut guard = server.strings.lock().unwrap();
                    let entry = guard.vacant_entry();
                    let key = entry.key();
                    guard.insert(suffix.to_owned());
                    key
                };

                // Invoke the lambda passing the handle to the request as well as
                // the suffix of the path.
                let response = match invoke_lambda(
                    lambda,
                    vec![
                        FfiValue::Integer(request_handle as DwarfInteger),
                        FfiValue::Integer(suffix as DwarfInteger),
                    ]
                    .into(),
                    &server,
                ) {
                    Ok(result) => result,
                    Err(e) => {
                        return Box::pin(async { mk_response(e) });
                    }
                };

                let mut requests = server.requests.lock().unwrap();
                requests.remove(request_handle);

                let mut responses = server.responses.lock().unwrap();
                let response = responses.remove(response as usize);

                Box::pin(async { Ok(response) })
            } else {
                let mut dwarf_home: PathBuf = env::var("DWARF_HOME")
                    .unwrap_or_else(|_| {
                        let mut home = env::var("HOME").unwrap();
                        home.push_str("/.dwarf");
                        home
                    })
                    .into();
                dwarf_home.push(EXTENSION_DIR);
                dwarf_home.push(PLUGIN_DIR);
                dwarf_home.push(MISC_DIR);

                dbg!(&path);

                if path.contains("/404.css") {
                    dwarf_home.push(CSS_404);
                    let css_404 = fs::read_to_string(&dwarf_home).unwrap();
                    Box::pin(async move { mk_css_response(css_404.into()) })
                } else if path.contains("/404.webp") {
                    dwarf_home.push(WEBP_404);
                    let webp_404 = fs::read(&dwarf_home).unwrap();
                    Box::pin(async move { mk_webp_response(webp_404) })
                } else {
                    dwarf_home.push(HTML_404);
                    let file_404 = fs::read_to_string(&dwarf_home).unwrap();
                    Box::pin(async move { mk_not_found(file_404.into()) })
                }
            }
        }
    }

    /// Invoke a Lambda returning a [`FfiValue::Struct``]
    ///
    /// Invoke a lambda in the dwarf process and return a Struct to the caller.
    /// This is used by the Service.
    fn invoke_lambda(
        lambda: usize,
        args: RVec<FfiValue>,
        server: &HttpServer,
    ) -> Result<DwarfInteger, String> {
        let (s, result) = crossbeam::channel::bounded(1);

        let lambda_call = LambdaCall {
            lambda: lambda,
            args,
            result: s.into(),
        };
        server.lambda_call.send(lambda_call).unwrap();
        let result = result.recv().unwrap();

        let ROk(FfiValue::Struct(response)) = result else {
            match result {
                RErr(e) => {
                    eprintln!("Error in http plugin lambda result: {e:?}");
                    return Err(format!("oh no! something went terribly wrong. 🤯").into());
                }
                ROk(value) => {
                    eprintln!("Expected Struct and found {value:?}");
                    return Err(format!("oh no! something went terribly wrong. 🤯").into());
                }
            }
        };

        let inner = response.get_attr("inner");
        let Some(&FfiValue::Integer(inner)) = inner else {
            return Err("No inner attribute found on Response object! 😱".to_owned());
        };

        Ok(inner)
    }
}
