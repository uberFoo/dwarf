//! The _Metamodel_
//!
//! This is the model of the model. From here all is generated...
use std::sync::Arc;
use std::sync::RwLock;
use std::{
    fmt::{self, Display},
    path::Path,
};

use abi_stable::{
    export_root_module,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::{TD_CanDowncast, TD_Opaque},
    std_types::{RBox, RErr, ROk, ROption, RResult, RStr, RString, RVec},
};
use dwarf::{
    chacha::value::{FfiProxy, FfiValue, Value},
    plug_in::{Error, Plugin, PluginModRef, PluginModule, PluginType, Plugin_TO},
};
use log::debug;
use uuid::{uuid, Uuid};

pub mod store;
pub mod types;
pub use store::ObjectStore;
pub use types::*;
pub const MODEL: &[u8] = include_bytes!("../models/sarzak.bin");

/// Exports the root module of this library.
///
/// This code isn't run until the layout of the type it returns is checked.
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, id, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "sarzak".into()
}

#[sabi_extern_fn]
pub fn id() -> RStr<'static> {
    "sarzak".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    match (|| {
        if args.len() == 0 {
            Ok(SarzakStore {
                store: Arc::new(RwLock::new(ObjectStore::new())),
            })
        } else if args.len() == 1 {
            if let FfiValue::String(path) = &args[0] {
                let store = ObjectStore::load(Path::new(&path.as_str())).unwrap();
                Ok(SarzakStore {
                    store: Arc::new(RwLock::new(store)),
                })
            } else {
                Err(Error::Uber("Invalid arguments".into()))
            }
        } else {
            Err(Error::Uber("Invalid arguments".into()))
        }
    })() {
        Ok(this) => ROk(Plugin_TO::from_value(this, TD_Opaque)),
        Err(e) => RErr(e.into()),
    }
}

#[derive(Clone, Debug)]
struct SarzakStore {
    store: Arc<RwLock<ObjectStore>>,
}

impl Display for SarzakStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.store)
    }
}

impl Plugin for SarzakStore {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "ObjectStore" => match func {
                    "persist" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(path) = args.pop().unwrap() {
                            self.store
                                .read()
                                .unwrap()
                                .persist(Path::new(&path.as_str()))
                                .unwrap();
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid path".into()))
                        }
                    }

                    "inter_acknowledged_event" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(acknowledged_event) = args.pop().unwrap() {
                            let acknowledged_event = acknowledged_event
                                .obj
                                .downcast_into::<AcknowledgedEventProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_acknowledged_event(acknowledged_event.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid AcknowledgedEvent".into()))
                        }
                    }
                    "exhume_acknowledged_event" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let acknowledged_event = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_acknowledged_event(&id.into())
                                .unwrap();
                            let acknowledged_event_proxy = AcknowledgedEventProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: acknowledged_event.clone(),
                                store: self.store.clone(),
                            };
                            let plugin =
                                Plugin_TO::from_value(acknowledged_event_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ACKNOWLEDGED_EVENT_ID.into(),
                                id: acknowledged_event.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_an_associative_referent" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(an_associative_referent) = args.pop().unwrap() {
                            let an_associative_referent = an_associative_referent
                                .obj
                                .downcast_into::<AnAssociativeReferentProxy>()
                                .unwrap();
                            self.store.write().unwrap().inter_an_associative_referent(
                                an_associative_referent.inner.clone(),
                            );
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid AnAssociativeReferent".into()))
                        }
                    }
                    "exhume_an_associative_referent" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let an_associative_referent = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_an_associative_referent(&id.into())
                                .unwrap();
                            let an_associative_referent_proxy = AnAssociativeReferentProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: an_associative_referent.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(
                                an_associative_referent_proxy,
                                TD_CanDowncast,
                            );
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: AN_ASSOCIATIVE_REFERENT_ID.into(),
                                id: an_associative_referent.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_associative" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(associative) = args.pop().unwrap() {
                            let associative =
                                associative.obj.downcast_into::<AssociativeProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_associative(associative.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Associative".into()))
                        }
                    }
                    "exhume_associative" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let associative = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_associative(&id.into())
                                .unwrap();
                            let associative_proxy = AssociativeProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: associative.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(associative_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ASSOCIATIVE_ID.into(),
                                id: associative.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_associative_referent" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(associative_referent) = args.pop().unwrap() {
                            let associative_referent = associative_referent
                                .obj
                                .downcast_into::<AssociativeReferentProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_associative_referent(associative_referent.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid AssociativeReferent".into()))
                        }
                    }
                    "exhume_associative_referent" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let associative_referent = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_associative_referent(&id.into())
                                .unwrap();
                            let associative_referent_proxy = AssociativeReferentProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: associative_referent.clone(),
                                store: self.store.clone(),
                            };
                            let plugin =
                                Plugin_TO::from_value(associative_referent_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ASSOCIATIVE_REFERENT_ID.into(),
                                id: associative_referent.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_associative_referrer" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(associative_referrer) = args.pop().unwrap() {
                            let associative_referrer = associative_referrer
                                .obj
                                .downcast_into::<AssociativeReferrerProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_associative_referrer(associative_referrer.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid AssociativeReferrer".into()))
                        }
                    }
                    "exhume_associative_referrer" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let associative_referrer = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_associative_referrer(&id.into())
                                .unwrap();
                            let associative_referrer_proxy = AssociativeReferrerProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: associative_referrer.clone(),
                                store: self.store.clone(),
                            };
                            let plugin =
                                Plugin_TO::from_value(associative_referrer_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ASSOCIATIVE_REFERRER_ID.into(),
                                id: associative_referrer.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_attribute" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(attribute) = args.pop().unwrap() {
                            let attribute =
                                attribute.obj.downcast_into::<AttributeProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_attribute(attribute.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Attribute".into()))
                        }
                    }
                    "exhume_attribute" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let attribute = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_attribute(&id.into())
                                .unwrap();
                            let attribute_proxy = AttributeProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: attribute.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(attribute_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ATTRIBUTE_ID.into(),
                                id: attribute.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_binary" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(binary) = args.pop().unwrap() {
                            let binary = binary.obj.downcast_into::<BinaryProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_binary(binary.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Binary".into()))
                        }
                    }
                    "exhume_binary" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let binary = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_binary(&id.into())
                                .unwrap();
                            let binary_proxy = BinaryProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: binary.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(binary_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: BINARY_ID.into(),
                                id: binary.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_cardinality" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(cardinality) = args.pop().unwrap() {
                            let cardinality =
                                cardinality.obj.downcast_into::<CardinalityProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_cardinality(cardinality.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Cardinality".into()))
                        }
                    }
                    "exhume_cardinality" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let cardinality = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_cardinality(&id.into())
                                .unwrap();
                            let cardinality_proxy = CardinalityProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: cardinality.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(cardinality_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: CARDINALITY_ID.into(),
                                id: cardinality.read().unwrap().id().into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_conditionality" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(conditionality) = args.pop().unwrap() {
                            let conditionality = conditionality
                                .obj
                                .downcast_into::<ConditionalityProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_conditionality(conditionality.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Conditionality".into()))
                        }
                    }
                    "exhume_conditionality" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let conditionality = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_conditionality(&id.into())
                                .unwrap();
                            let conditionality_proxy = ConditionalityProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: conditionality.clone(),
                                store: self.store.clone(),
                            };
                            let plugin =
                                Plugin_TO::from_value(conditionality_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: CONDITIONALITY_ID.into(),
                                id: conditionality.read().unwrap().id().into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_event" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(event) = args.pop().unwrap() {
                            let event = event.obj.downcast_into::<EventProxy>().unwrap();
                            self.store.write().unwrap().inter_event(event.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Event".into()))
                        }
                    }
                    "exhume_event" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let event =
                                self.store.read().unwrap().exhume_event(&id.into()).unwrap();
                            let event_proxy = EventProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: event.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(event_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: EVENT_ID.into(),
                                id: event.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_external" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(external) = args.pop().unwrap() {
                            let external = external.obj.downcast_into::<ExternalProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_external(external.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid External".into()))
                        }
                    }
                    "exhume_external" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let external = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_external(&id.into())
                                .unwrap();
                            let external_proxy = ExternalProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: external.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(external_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: EXTERNAL_ID.into(),
                                id: external.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_isa" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(isa) = args.pop().unwrap() {
                            let isa = isa.obj.downcast_into::<IsaProxy>().unwrap();
                            self.store.write().unwrap().inter_isa(isa.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Isa".into()))
                        }
                    }
                    "exhume_isa" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let isa = self.store.read().unwrap().exhume_isa(&id.into()).unwrap();
                            let isa_proxy = IsaProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: isa.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(isa_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ISA_ID.into(),
                                id: isa.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_object" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(object) = args.pop().unwrap() {
                            let object = object.obj.downcast_into::<ObjectProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_object(object.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "exhume_object" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let object = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_object(&id.into())
                                .unwrap();
                            let object_proxy = ObjectProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: object.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(object_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: OBJECT_ID.into(),
                                id: object.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_referent" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(referent) = args.pop().unwrap() {
                            let referent = referent.obj.downcast_into::<ReferentProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_referent(referent.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Referent".into()))
                        }
                    }
                    "exhume_referent" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let referent = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_referent(&id.into())
                                .unwrap();
                            let referent_proxy = ReferentProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: referent.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(referent_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: REFERENT_ID.into(),
                                id: referent.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_referrer" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(referrer) = args.pop().unwrap() {
                            let referrer = referrer.obj.downcast_into::<ReferrerProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_referrer(referrer.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Referrer".into()))
                        }
                    }
                    "exhume_referrer" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let referrer = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_referrer(&id.into())
                                .unwrap();
                            let referrer_proxy = ReferrerProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: referrer.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(referrer_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: REFERRER_ID.into(),
                                id: referrer.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_relationship" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(relationship) = args.pop().unwrap() {
                            let relationship = relationship
                                .obj
                                .downcast_into::<RelationshipProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_relationship(relationship.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Relationship".into()))
                        }
                    }
                    "exhume_relationship" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let relationship = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_relationship(&id.into())
                                .unwrap();
                            let relationship_proxy = RelationshipProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: relationship.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(relationship_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: RELATIONSHIP_ID.into(),
                                id: relationship.read().unwrap().id().into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_state" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(state) = args.pop().unwrap() {
                            let state = state.obj.downcast_into::<StateProxy>().unwrap();
                            self.store.write().unwrap().inter_state(state.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid State".into()))
                        }
                    }
                    "exhume_state" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let state =
                                self.store.read().unwrap().exhume_state(&id.into()).unwrap();
                            let state_proxy = StateProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: state.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(state_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: STATE_ID.into(),
                                id: state.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_subtype" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(subtype) = args.pop().unwrap() {
                            let subtype = subtype.obj.downcast_into::<SubtypeProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_subtype(subtype.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Subtype".into()))
                        }
                    }
                    "exhume_subtype" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let subtype = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_subtype(&id.into())
                                .unwrap();
                            let subtype_proxy = SubtypeProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: subtype.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(subtype_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: SUBTYPE_ID.into(),
                                id: subtype.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_supertype" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(supertype) = args.pop().unwrap() {
                            let supertype =
                                supertype.obj.downcast_into::<SupertypeProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_supertype(supertype.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Supertype".into()))
                        }
                    }
                    "exhume_supertype" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let supertype = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_supertype(&id.into())
                                .unwrap();
                            let supertype_proxy = SupertypeProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: supertype.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(supertype_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: SUPERTYPE_ID.into(),
                                id: supertype.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_ty" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(ty) = args.pop().unwrap() {
                            let ty = ty.obj.downcast_into::<TyProxy>().unwrap();
                            self.store.write().unwrap().inter_ty(ty.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Ty".into()))
                        }
                    }
                    "exhume_ty" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let ty = self.store.read().unwrap().exhume_ty(&id.into()).unwrap();
                            let ty_proxy = TyProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: ty.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(ty_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: TY_ID.into(),
                                id: ty.read().unwrap().id().into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "AcknowledgedEvent" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<AcknowledgedEvent, Error> {
                            let id = Uuid::new_v4();
                            let acknowledged_event = AcknowledgedEvent {
                                id,
                                event_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                state_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(acknowledged_event)
                        })() {
                            Ok(acknowledged_event) => {
                                let acknowledged_event = Arc::new(RwLock::new(acknowledged_event));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_acknowledged_event(acknowledged_event.clone());
                                let this = AcknowledgedEventProxy {
                                    inner: acknowledged_event.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ACKNOWLEDGED_EVENT_ID.into(),
                                    id: acknowledged_event.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for acknowledged_event in
                            self.store.read().unwrap().iter_acknowledged_event()
                        {
                            let this = AcknowledgedEventProxy {
                                inner: acknowledged_event.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ACKNOWLEDGED_EVENT_ID.into(),
                                id: acknowledged_event.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "AnAssociativeReferent" => match func {
                    "new" => {
                        if args.len() != 3 {
                            return Err(Error::Uber("Expected 3 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<AnAssociativeReferent, Error> {
                            let id = Uuid::new_v4();
                            let an_associative_referent = AnAssociativeReferent {
                                id,
                                referential_attribute: value_args
                                    .pop()
                                    .unwrap()
                                    .try_into()
                                    .map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                associative: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                referent: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(an_associative_referent)
                        })() {
                            Ok(an_associative_referent) => {
                                let an_associative_referent =
                                    Arc::new(RwLock::new(an_associative_referent));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_an_associative_referent(an_associative_referent.clone());
                                let this = AnAssociativeReferentProxy {
                                    inner: an_associative_referent.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: AN_ASSOCIATIVE_REFERENT_ID.into(),
                                    id: an_associative_referent.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for an_associative_referent in
                            self.store.read().unwrap().iter_an_associative_referent()
                        {
                            let this = AnAssociativeReferentProxy {
                                inner: an_associative_referent.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: AN_ASSOCIATIVE_REFERENT_ID.into(),
                                id: an_associative_referent.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Associative" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Associative, Error> {
                            let id = Uuid::new_v4();
                            let associative = Associative {
                                id,
                                number: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                from: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(associative)
                        })() {
                            Ok(associative) => {
                                let associative = Arc::new(RwLock::new(associative));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_associative(associative.clone());
                                let this = AssociativeProxy {
                                    inner: associative.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ASSOCIATIVE_ID.into(),
                                    id: associative.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for associative in self.store.read().unwrap().iter_associative() {
                            let this = AssociativeProxy {
                                inner: associative.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ASSOCIATIVE_ID.into(),
                                id: associative.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "AssociativeReferent" => match func {
                    "new" => {
                        if args.len() != 4 {
                            return Err(Error::Uber("Expected 4 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<AssociativeReferent, Error> {
                            let id = Uuid::new_v4();
                            let associative_referent = AssociativeReferent {
                                id,
                                description: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                cardinality: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                conditionality: value_args.pop().unwrap().try_into().map_err(
                                    |e| Error::Uber(format!("Error converting value: {e}").into()),
                                )?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(associative_referent)
                        })() {
                            Ok(associative_referent) => {
                                let associative_referent =
                                    Arc::new(RwLock::new(associative_referent));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_associative_referent(associative_referent.clone());
                                let this = AssociativeReferentProxy {
                                    inner: associative_referent.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ASSOCIATIVE_REFERENT_ID.into(),
                                    id: associative_referent.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for associative_referent in
                            self.store.read().unwrap().iter_associative_referent()
                        {
                            let this = AssociativeReferentProxy {
                                inner: associative_referent.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ASSOCIATIVE_REFERENT_ID.into(),
                                id: associative_referent.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "AssociativeReferrer" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<AssociativeReferrer, Error> {
                            let id = Uuid::new_v4();
                            let associative_referrer = AssociativeReferrer {
                                id,
                                cardinality: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(associative_referrer)
                        })() {
                            Ok(associative_referrer) => {
                                let associative_referrer =
                                    Arc::new(RwLock::new(associative_referrer));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_associative_referrer(associative_referrer.clone());
                                let this = AssociativeReferrerProxy {
                                    inner: associative_referrer.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ASSOCIATIVE_REFERRER_ID.into(),
                                    id: associative_referrer.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for associative_referrer in
                            self.store.read().unwrap().iter_associative_referrer()
                        {
                            let this = AssociativeReferrerProxy {
                                inner: associative_referrer.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ASSOCIATIVE_REFERRER_ID.into(),
                                id: associative_referrer.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Attribute" => match func {
                    "new" => {
                        if args.len() != 3 {
                            return Err(Error::Uber("Expected 3 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Attribute, Error> {
                            let id = Uuid::new_v4();
                            let attribute = Attribute {
                                id,
                                name: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                ty: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(attribute)
                        })() {
                            Ok(attribute) => {
                                let attribute = Arc::new(RwLock::new(attribute));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_attribute(attribute.clone());
                                let this = AttributeProxy {
                                    inner: attribute.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ATTRIBUTE_ID.into(),
                                    id: attribute.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for attribute in self.store.read().unwrap().iter_attribute() {
                            let this = AttributeProxy {
                                inner: attribute.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ATTRIBUTE_ID.into(),
                                id: attribute.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Binary" => match func {
                    "new" => {
                        if args.len() != 3 {
                            return Err(Error::Uber("Expected 3 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Binary, Error> {
                            let id = Uuid::new_v4();
                            let binary = Binary {
                                id,
                                number: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                from: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                to: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(binary)
                        })() {
                            Ok(binary) => {
                                let binary = Arc::new(RwLock::new(binary));
                                self.store.write().unwrap().inter_binary(binary.clone());
                                let this = BinaryProxy {
                                    inner: binary.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: BINARY_ID.into(),
                                    id: binary.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for binary in self.store.read().unwrap().iter_binary() {
                            let this = BinaryProxy {
                                inner: binary.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: BINARY_ID.into(),
                                id: binary.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Cardinality" => match func {
                    "new_many" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Cardinality>>, Error> {
                            let many = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_cardinality(&MANY)
                                .unwrap();

                            Ok(many)
                        })() {
                            Ok(many) => {
                                let this = CardinalityProxy {
                                    inner: many.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: MANY.into(),
                                    id: many.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_one" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Cardinality>>, Error> {
                            let one = self.store.read().unwrap().exhume_cardinality(&ONE).unwrap();

                            Ok(one)
                        })() {
                            Ok(one) => {
                                let this = CardinalityProxy {
                                    inner: one.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ONE.into(),
                                    id: one.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for cardinality in self.store.read().unwrap().iter_cardinality() {
                            let this = CardinalityProxy {
                                inner: cardinality.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: CARDINALITY_ID.into(),
                                id: cardinality.read().unwrap().id().into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Conditionality" => match func {
                    "new_conditional" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Conditionality>>, Error> {
                            let conditional = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_conditionality(&CONDITIONAL)
                                .unwrap();

                            Ok(conditional)
                        })() {
                            Ok(conditional) => {
                                let this = ConditionalityProxy {
                                    inner: conditional.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: CONDITIONAL.into(),
                                    id: conditional.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_unconditional" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Conditionality>>, Error> {
                            let unconditional = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_conditionality(&UNCONDITIONAL)
                                .unwrap();

                            Ok(unconditional)
                        })() {
                            Ok(unconditional) => {
                                let this = ConditionalityProxy {
                                    inner: unconditional.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: UNCONDITIONAL.into(),
                                    id: unconditional.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for conditionality in self.store.read().unwrap().iter_conditionality() {
                            let this = ConditionalityProxy {
                                inner: conditionality.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: CONDITIONALITY_ID.into(),
                                id: conditionality.read().unwrap().id().into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Event" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Event, Error> {
                            let id = Uuid::new_v4();
                            let event = Event {
                                id,
                                name: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(event)
                        })() {
                            Ok(event) => {
                                let event = Arc::new(RwLock::new(event));
                                self.store.write().unwrap().inter_event(event.clone());
                                let this = EventProxy {
                                    inner: event.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: EVENT_ID.into(),
                                    id: event.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for event in self.store.read().unwrap().iter_event() {
                            let this = EventProxy {
                                inner: event.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: EVENT_ID.into(),
                                id: event.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "External" => match func {
                    "new" => {
                        if args.len() != 3 {
                            return Err(Error::Uber("Expected 3 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<External, Error> {
                            let id = Uuid::new_v4();
                            let external = External {
                                id,
                                ctor: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                name: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                x_path: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(external)
                        })() {
                            Ok(external) => {
                                let external = Arc::new(RwLock::new(external));
                                self.store.write().unwrap().inter_external(external.clone());
                                let this = ExternalProxy {
                                    inner: external.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: EXTERNAL_ID.into(),
                                    id: external.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for external in self.store.read().unwrap().iter_external() {
                            let this = ExternalProxy {
                                inner: external.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: EXTERNAL_ID.into(),
                                id: external.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Isa" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Isa, Error> {
                            let id = Uuid::new_v4();
                            let isa = Isa {
                                id,
                                number: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                supertype: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(isa)
                        })() {
                            Ok(isa) => {
                                let isa = Arc::new(RwLock::new(isa));
                                self.store.write().unwrap().inter_isa(isa.clone());
                                let this = IsaProxy {
                                    inner: isa.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ISA_ID.into(),
                                    id: isa.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for isa in self.store.read().unwrap().iter_isa() {
                            let this = IsaProxy {
                                inner: isa.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ISA_ID.into(),
                                id: isa.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Object" => match func {
                    "new" => {
                        if args.len() != 3 {
                            return Err(Error::Uber("Expected 3 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Object, Error> {
                            let id = Uuid::new_v4();
                            let object = Object {
                                id,
                                description: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                key_letters: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                name: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(object)
                        })() {
                            Ok(object) => {
                                let object = Arc::new(RwLock::new(object));
                                self.store.write().unwrap().inter_object(object.clone());
                                let this = ObjectProxy {
                                    inner: object.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: OBJECT_ID.into(),
                                    id: object.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for object in self.store.read().unwrap().iter_object() {
                            let this = ObjectProxy {
                                inner: object.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: OBJECT_ID.into(),
                                id: object.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Referent" => match func {
                    "new" => {
                        if args.len() != 4 {
                            return Err(Error::Uber("Expected 4 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Referent, Error> {
                            let id = Uuid::new_v4();
                            let referent = Referent {
                                id,
                                description: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                cardinality: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                conditionality: value_args.pop().unwrap().try_into().map_err(
                                    |e| Error::Uber(format!("Error converting value: {e}").into()),
                                )?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(referent)
                        })() {
                            Ok(referent) => {
                                let referent = Arc::new(RwLock::new(referent));
                                self.store.write().unwrap().inter_referent(referent.clone());
                                let this = ReferentProxy {
                                    inner: referent.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: REFERENT_ID.into(),
                                    id: referent.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for referent in self.store.read().unwrap().iter_referent() {
                            let this = ReferentProxy {
                                inner: referent.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: REFERENT_ID.into(),
                                id: referent.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Referrer" => match func {
                    "new" => {
                        if args.len() != 5 {
                            return Err(Error::Uber("Expected 5 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Referrer, Error> {
                            let id = Uuid::new_v4();
                            let referrer = Referrer {
                                id,
                                description: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                referential_attribute: value_args
                                    .pop()
                                    .unwrap()
                                    .try_into()
                                    .map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                cardinality: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                conditionality: value_args.pop().unwrap().try_into().map_err(
                                    |e| Error::Uber(format!("Error converting value: {e}").into()),
                                )?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(referrer)
                        })() {
                            Ok(referrer) => {
                                let referrer = Arc::new(RwLock::new(referrer));
                                self.store.write().unwrap().inter_referrer(referrer.clone());
                                let this = ReferrerProxy {
                                    inner: referrer.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: REFERRER_ID.into(),
                                    id: referrer.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for referrer in self.store.read().unwrap().iter_referrer() {
                            let this = ReferrerProxy {
                                inner: referrer.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: REFERRER_ID.into(),
                                id: referrer.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Relationship" => match func {
                    "new_associative" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Relationship, Error> {
                            let associative = Relationship::Associative(
                                value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            );

                            Ok(associative)
                        })() {
                            Ok(associative) => {
                                let associative = Arc::new(RwLock::new(associative));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_relationship(associative.clone());
                                let this = RelationshipProxy {
                                    inner: associative.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: RELATIONSHIP_ID.into(),
                                    id: associative.read().unwrap().id().into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_binary" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Relationship, Error> {
                            let binary = Relationship::Binary(
                                value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            );

                            Ok(binary)
                        })() {
                            Ok(binary) => {
                                let binary = Arc::new(RwLock::new(binary));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_relationship(binary.clone());
                                let this = RelationshipProxy {
                                    inner: binary.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: RELATIONSHIP_ID.into(),
                                    id: binary.read().unwrap().id().into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_isa" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Relationship, Error> {
                            let isa =
                                Relationship::Isa(value_args.pop().unwrap().try_into().map_err(
                                    |e| Error::Uber(format!("Error converting value: {e}").into()),
                                )?);

                            Ok(isa)
                        })() {
                            Ok(isa) => {
                                let isa = Arc::new(RwLock::new(isa));
                                self.store.write().unwrap().inter_relationship(isa.clone());
                                let this = RelationshipProxy {
                                    inner: isa.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: RELATIONSHIP_ID.into(),
                                    id: isa.read().unwrap().id().into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for relationship in self.store.read().unwrap().iter_relationship() {
                            let this = RelationshipProxy {
                                inner: relationship.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: RELATIONSHIP_ID.into(),
                                id: relationship.read().unwrap().id().into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "State" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<State, Error> {
                            let id = Uuid::new_v4();
                            let state = State {
                                id,
                                name: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(state)
                        })() {
                            Ok(state) => {
                                let state = Arc::new(RwLock::new(state));
                                self.store.write().unwrap().inter_state(state.clone());
                                let this = StateProxy {
                                    inner: state.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: STATE_ID.into(),
                                    id: state.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for state in self.store.read().unwrap().iter_state() {
                            let this = StateProxy {
                                inner: state.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: STATE_ID.into(),
                                id: state.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Subtype" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Subtype, Error> {
                            let id = Uuid::new_v4();
                            let subtype = Subtype {
                                id,
                                isa: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(subtype)
                        })() {
                            Ok(subtype) => {
                                let subtype = Arc::new(RwLock::new(subtype));
                                self.store.write().unwrap().inter_subtype(subtype.clone());
                                let this = SubtypeProxy {
                                    inner: subtype.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: SUBTYPE_ID.into(),
                                    id: subtype.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for subtype in self.store.read().unwrap().iter_subtype() {
                            let this = SubtypeProxy {
                                inner: subtype.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: SUBTYPE_ID.into(),
                                id: subtype.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Supertype" => match func {
                    "new" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Supertype, Error> {
                            let id = Uuid::new_v4();
                            let supertype = Supertype {
                                id,
                                obj_id: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(supertype)
                        })() {
                            Ok(supertype) => {
                                let supertype = Arc::new(RwLock::new(supertype));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_supertype(supertype.clone());
                                let this = SupertypeProxy {
                                    inner: supertype.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: SUPERTYPE_ID.into(),
                                    id: supertype.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for supertype in self.store.read().unwrap().iter_supertype() {
                            let this = SupertypeProxy {
                                inner: supertype.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: SUPERTYPE_ID.into(),
                                id: supertype.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Ty" => match func {
                    "new_boolean" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Ty>>, Error> {
                            let boolean = self.store.read().unwrap().exhume_ty(&BOOLEAN).unwrap();

                            Ok(boolean)
                        })() {
                            Ok(boolean) => {
                                let this = TyProxy {
                                    inner: boolean.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: BOOLEAN.into(),
                                    id: boolean.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_external" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Ty, Error> {
                            let external =
                                Ty::External(value_args.pop().unwrap().try_into().map_err(
                                    |e| Error::Uber(format!("Error converting value: {e}").into()),
                                )?);

                            Ok(external)
                        })() {
                            Ok(external) => {
                                let external = Arc::new(RwLock::new(external));
                                self.store.write().unwrap().inter_ty(external.clone());
                                let this = TyProxy {
                                    inner: external.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: TY_ID.into(),
                                    id: external.read().unwrap().id().into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_float" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Ty>>, Error> {
                            let float = self.store.read().unwrap().exhume_ty(&FLOAT).unwrap();

                            Ok(float)
                        })() {
                            Ok(float) => {
                                let this = TyProxy {
                                    inner: float.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: FLOAT.into(),
                                    id: float.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_integer" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Ty>>, Error> {
                            let integer = self.store.read().unwrap().exhume_ty(&INTEGER).unwrap();

                            Ok(integer)
                        })() {
                            Ok(integer) => {
                                let this = TyProxy {
                                    inner: integer.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: INTEGER.into(),
                                    id: integer.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_object" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Ty, Error> {
                            let object =
                                Ty::Object(value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?);

                            Ok(object)
                        })() {
                            Ok(object) => {
                                let object = Arc::new(RwLock::new(object));
                                self.store.write().unwrap().inter_ty(object.clone());
                                let this = TyProxy {
                                    inner: object.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: TY_ID.into(),
                                    id: object.read().unwrap().id().into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_s_string" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Ty>>, Error> {
                            let s_string = self.store.read().unwrap().exhume_ty(&S_STRING).unwrap();

                            Ok(s_string)
                        })() {
                            Ok(s_string) => {
                                let this = TyProxy {
                                    inner: s_string.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: S_STRING.into(),
                                    id: s_string.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_s_uuid" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Ty>>, Error> {
                            let s_uuid = self.store.read().unwrap().exhume_ty(&S_UUID).unwrap();

                            Ok(s_uuid)
                        })() {
                            Ok(s_uuid) => {
                                let this = TyProxy {
                                    inner: s_uuid.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: S_UUID.into(),
                                    id: s_uuid.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for ty in self.store.read().unwrap().iter_ty() {
                            let this = TyProxy {
                                inner: ty.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: TY_ID.into(),
                                id: ty.read().unwrap().id().into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "merlin".into()
    }

    fn close(self) {}
}

const ACKNOWLEDGED_EVENT_ID: Uuid = uuid!("2979402f-0980-58b6-9601-62f931e7f368");

#[derive(Clone, Debug)]
pub struct AcknowledgedEventProxy {
    inner: Arc<RwLock<AcknowledgedEvent>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for AcknowledgedEventProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "event_id" => {
                                    let event_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_event(&self.inner.read().unwrap().event_id)
                                        .unwrap();

                                    let this = EventProxy {
                                        inner: event_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: EVENT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "state_id" => {
                                    let state_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_state(&self.inner.read().unwrap().state_id)
                                        .unwrap();

                                    let this = StateProxy {
                                        inner: state_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: STATE_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "event_id" => {
                                    self.inner.write().unwrap().event_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "state_id" => {
                                    self.inner.write().unwrap().state_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "AcknowledgedEvent".into()
    }

    fn close(self) {}
}

impl Display for AcknowledgedEventProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AcknowledgedEvent({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	event_id: {:?},", self.inner.read().unwrap().event_id)?;
        writeln!(f, "	state_id: {:?},", self.inner.read().unwrap().state_id)?;
        writeln!(f, "}})")
    }
}

const AN_ASSOCIATIVE_REFERENT_ID: Uuid = uuid!("7e899d0b-c69b-51e8-b264-d769c9ac9134");

#[derive(Clone, Debug)]
pub struct AnAssociativeReferentProxy {
    inner: Arc<RwLock<AnAssociativeReferent>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for AnAssociativeReferentProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "referential_attribute" => Ok(FfiValue::String(
                                    self.inner
                                        .read()
                                        .unwrap()
                                        .referential_attribute
                                        .clone()
                                        .into(),
                                )),
                                "associative" => {
                                    let associative = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_associative(&self.inner.read().unwrap().associative)
                                        .unwrap();

                                    let this = AssociativeProxy {
                                        inner: associative,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: ASSOCIATIVE_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "referent" => {
                                    let referent = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_associative_referent(
                                            &self.inner.read().unwrap().referent,
                                        )
                                        .unwrap();

                                    let this = AssociativeReferentProxy {
                                        inner: referent,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: ASSOCIATIVE_REFERENT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "referential_attribute" => {
                                    self.inner.write().unwrap().referential_attribute =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "associative" => {
                                    self.inner.write().unwrap().associative =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "referent" => {
                                    self.inner.write().unwrap().referent =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "AnAssociativeReferent".into()
    }

    fn close(self) {}
}

impl Display for AnAssociativeReferentProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AnAssociativeReferent({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(
            f,
            "	referential_attribute: {:?},",
            self.inner.read().unwrap().referential_attribute
        )?;
        writeln!(
            f,
            "	associative: {:?},",
            self.inner.read().unwrap().associative
        )?;
        writeln!(f, "	referent: {:?},", self.inner.read().unwrap().referent)?;
        writeln!(f, "}})")
    }
}

const ASSOCIATIVE_ID: Uuid = uuid!("17de0bb6-ee65-5516-b8eb-9a9a35e5fedd");

#[derive(Clone, Debug)]
pub struct AssociativeProxy {
    inner: Arc<RwLock<Associative>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for AssociativeProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "number" => {
                                    Ok(FfiValue::Integer(self.inner.read().unwrap().number.into()))
                                }
                                "from" => {
                                    let from = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_associative_referrer(
                                            &self.inner.read().unwrap().from,
                                        )
                                        .unwrap();

                                    let this = AssociativeReferrerProxy {
                                        inner: from,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: ASSOCIATIVE_REFERRER_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "number" => {
                                    self.inner.write().unwrap().number =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "from" => {
                                    self.inner.write().unwrap().from =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Associative".into()
    }

    fn close(self) {}
}

impl Display for AssociativeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Associative({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	number: {:?},", self.inner.read().unwrap().number)?;
        writeln!(f, "	from: {:?},", self.inner.read().unwrap().from)?;
        writeln!(f, "}})")
    }
}

const ASSOCIATIVE_REFERENT_ID: Uuid = uuid!("e38511e6-1f25-503c-bf93-508885852440");

#[derive(Clone, Debug)]
pub struct AssociativeReferentProxy {
    inner: Arc<RwLock<AssociativeReferent>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for AssociativeReferentProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "description" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().description.clone().into(),
                                )),
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "cardinality" => {
                                    let cardinality = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_cardinality(&self.inner.read().unwrap().cardinality)
                                        .unwrap();

                                    let this = CardinalityProxy {
                                        inner: cardinality,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: CARDINALITY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "conditionality" => {
                                    let conditionality = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_conditionality(
                                            &self.inner.read().unwrap().conditionality,
                                        )
                                        .unwrap();

                                    let this = ConditionalityProxy {
                                        inner: conditionality,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: CONDITIONALITY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "description" => {
                                    self.inner.write().unwrap().description =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "cardinality" => {
                                    self.inner.write().unwrap().cardinality =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "conditionality" => {
                                    self.inner.write().unwrap().conditionality =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "AssociativeReferent".into()
    }

    fn close(self) {}
}

impl Display for AssociativeReferentProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AssociativeReferent({{")?;
        writeln!(
            f,
            "	description: {:?},",
            self.inner.read().unwrap().description
        )?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(
            f,
            "	cardinality: {:?},",
            self.inner.read().unwrap().cardinality
        )?;
        writeln!(
            f,
            "	conditionality: {:?},",
            self.inner.read().unwrap().conditionality
        )?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const ASSOCIATIVE_REFERRER_ID: Uuid = uuid!("faa5a05c-7252-5b3d-b415-ad3884269154");

#[derive(Clone, Debug)]
pub struct AssociativeReferrerProxy {
    inner: Arc<RwLock<AssociativeReferrer>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for AssociativeReferrerProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "cardinality" => {
                                    let cardinality = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_cardinality(&self.inner.read().unwrap().cardinality)
                                        .unwrap();

                                    let this = CardinalityProxy {
                                        inner: cardinality,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: CARDINALITY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "cardinality" => {
                                    self.inner.write().unwrap().cardinality =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "AssociativeReferrer".into()
    }

    fn close(self) {}
}

impl Display for AssociativeReferrerProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AssociativeReferrer({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(
            f,
            "	cardinality: {:?},",
            self.inner.read().unwrap().cardinality
        )?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const ATTRIBUTE_ID: Uuid = uuid!("63777957-b6bc-5253-b16b-6ff390f10dba");

#[derive(Clone, Debug)]
pub struct AttributeProxy {
    inner: Arc<RwLock<Attribute>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for AttributeProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "name" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().name.clone().into(),
                                )),
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "ty" => {
                                    let ty = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_ty(&self.inner.read().unwrap().ty)
                                        .unwrap();

                                    let this = TyProxy {
                                        inner: ty,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: TY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "name" => {
                                    self.inner.write().unwrap().name =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "ty" => {
                                    self.inner.write().unwrap().ty =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Attribute".into()
    }

    fn close(self) {}
}

impl Display for AttributeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Attribute({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	name: {:?},", self.inner.read().unwrap().name)?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "	ty: {:?},", self.inner.read().unwrap().ty)?;
        writeln!(f, "}})")
    }
}

const BINARY_ID: Uuid = uuid!("56c5ed80-25e7-592e-ab36-a306c78ac58b");

#[derive(Clone, Debug)]
pub struct BinaryProxy {
    inner: Arc<RwLock<Binary>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for BinaryProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "number" => {
                                    Ok(FfiValue::Integer(self.inner.read().unwrap().number.into()))
                                }
                                "from" => {
                                    let from = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_referrer(&self.inner.read().unwrap().from)
                                        .unwrap();

                                    let this = ReferrerProxy {
                                        inner: from,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: REFERRER_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "to" => {
                                    let to = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_referent(&self.inner.read().unwrap().to)
                                        .unwrap();

                                    let this = ReferentProxy {
                                        inner: to,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: REFERENT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "number" => {
                                    self.inner.write().unwrap().number =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "from" => {
                                    self.inner.write().unwrap().from =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "to" => {
                                    self.inner.write().unwrap().to =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Binary".into()
    }

    fn close(self) {}
}

impl Display for BinaryProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Binary({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	number: {:?},", self.inner.read().unwrap().number)?;
        writeln!(f, "	from: {:?},", self.inner.read().unwrap().from)?;
        writeln!(f, "	to: {:?},", self.inner.read().unwrap().to)?;
        writeln!(f, "}})")
    }
}

const BOOLEAN_ID: Uuid = uuid!("04fbbc6c-a351-5e6d-b193-191f5510033e");

#[derive(Clone, Debug)]
pub struct BooleanProxy {
    inner: Arc<RwLock<Boolean>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for BooleanProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Boolean".into()
    }

    fn close(self) {}
}

impl Display for BooleanProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Boolean({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const CARDINALITY_ID: Uuid = uuid!("d1b45eb7-b9fb-5fbc-90a5-f48473675fdb");

#[derive(Clone, Debug)]
pub struct CardinalityProxy {
    inner: Arc<RwLock<Cardinality>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for CardinalityProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Cardinality".into()
    }

    fn close(self) {}
}

impl Display for CardinalityProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Cardinality({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const CONDITIONAL_ID: Uuid = uuid!("cbd5902d-d04b-537f-9d6a-547a3b88f9a2");

#[derive(Clone, Debug)]
pub struct ConditionalProxy {
    inner: Arc<RwLock<Conditional>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for ConditionalProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Conditional".into()
    }

    fn close(self) {}
}

impl Display for ConditionalProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Conditional({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const CONDITIONALITY_ID: Uuid = uuid!("438b6783-15d1-5767-af60-900b3738fc9e");

#[derive(Clone, Debug)]
pub struct ConditionalityProxy {
    inner: Arc<RwLock<Conditionality>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for ConditionalityProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Conditionality".into()
    }

    fn close(self) {}
}

impl Display for ConditionalityProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Conditionality({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const EVENT_ID: Uuid = uuid!("dbdfade4-b61a-5e69-ab1a-c4d10e61bedd");

#[derive(Clone, Debug)]
pub struct EventProxy {
    inner: Arc<RwLock<Event>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for EventProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "name" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().name.clone().into(),
                                )),
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "name" => {
                                    self.inner.write().unwrap().name =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Event".into()
    }

    fn close(self) {}
}

impl Display for EventProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Event({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	name: {:?},", self.inner.read().unwrap().name)?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const EXTERNAL_ID: Uuid = uuid!("ab607ed4-66f7-5927-b42e-f48c07a1764a");

#[derive(Clone, Debug)]
pub struct ExternalProxy {
    inner: Arc<RwLock<External>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for ExternalProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "ctor" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().ctor.clone().into(),
                                )),
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "name" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().name.clone().into(),
                                )),
                                "x_path" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().x_path.clone().into(),
                                )),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "ctor" => {
                                    self.inner.write().unwrap().ctor =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "name" => {
                                    self.inner.write().unwrap().name =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "x_path" => {
                                    self.inner.write().unwrap().x_path =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "External".into()
    }

    fn close(self) {}
}

impl Display for ExternalProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "External({{")?;
        writeln!(f, "	ctor: {:?},", self.inner.read().unwrap().ctor)?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	name: {:?},", self.inner.read().unwrap().name)?;
        writeln!(f, "	x_path: {:?},", self.inner.read().unwrap().x_path)?;
        writeln!(f, "}})")
    }
}

const FLOAT_ID: Uuid = uuid!("f3d5c0a4-850d-5071-a7e3-50e53389e3a8");

#[derive(Clone, Debug)]
pub struct FloatProxy {
    inner: Arc<RwLock<Float>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for FloatProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Float".into()
    }

    fn close(self) {}
}

impl Display for FloatProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Float({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const INTEGER_ID: Uuid = uuid!("fae606a2-e37c-5f82-8754-1fc11c09fe4c");

#[derive(Clone, Debug)]
pub struct IntegerProxy {
    inner: Arc<RwLock<Integer>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for IntegerProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Integer".into()
    }

    fn close(self) {}
}

impl Display for IntegerProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Integer({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const ISA_ID: Uuid = uuid!("0cbeeb50-21ce-5e83-9f2e-65d1410d553f");

#[derive(Clone, Debug)]
pub struct IsaProxy {
    inner: Arc<RwLock<Isa>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for IsaProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "number" => {
                                    Ok(FfiValue::Integer(self.inner.read().unwrap().number.into()))
                                }
                                "supertype" => {
                                    let supertype = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_supertype(&self.inner.read().unwrap().supertype)
                                        .unwrap();

                                    let this = SupertypeProxy {
                                        inner: supertype,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: SUPERTYPE_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "number" => {
                                    self.inner.write().unwrap().number =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "supertype" => {
                                    self.inner.write().unwrap().supertype =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Isa".into()
    }

    fn close(self) {}
}

impl Display for IsaProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Isa({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	number: {:?},", self.inner.read().unwrap().number)?;
        writeln!(f, "	supertype: {:?},", self.inner.read().unwrap().supertype)?;
        writeln!(f, "}})")
    }
}

const MANY_ID: Uuid = uuid!("a549f635-38bd-5016-b79f-b03125fbfc02");

#[derive(Clone, Debug)]
pub struct ManyProxy {
    inner: Arc<RwLock<Many>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for ManyProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Many".into()
    }

    fn close(self) {}
}

impl Display for ManyProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Many({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const OBJECT_ID: Uuid = uuid!("7178e7a4-5131-504b-a7b3-c2c0cfedf343");

#[derive(Clone, Debug)]
pub struct ObjectProxy {
    inner: Arc<RwLock<Object>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for ObjectProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "description" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().description.clone().into(),
                                )),
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "key_letters" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().key_letters.clone().into(),
                                )),
                                "name" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().name.clone().into(),
                                )),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "description" => {
                                    self.inner.write().unwrap().description =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "key_letters" => {
                                    self.inner.write().unwrap().key_letters =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "name" => {
                                    self.inner.write().unwrap().name =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Object".into()
    }

    fn close(self) {}
}

impl Display for ObjectProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Object({{")?;
        writeln!(
            f,
            "	description: {:?},",
            self.inner.read().unwrap().description
        )?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(
            f,
            "	key_letters: {:?},",
            self.inner.read().unwrap().key_letters
        )?;
        writeln!(f, "	name: {:?},", self.inner.read().unwrap().name)?;
        writeln!(f, "}})")
    }
}

const ONE_ID: Uuid = uuid!("696b0652-8c4d-56d9-b4dc-0490cd4b2ea0");

#[derive(Clone, Debug)]
pub struct OneProxy {
    inner: Arc<RwLock<One>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for OneProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "One".into()
    }

    fn close(self) {}
}

impl Display for OneProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "One({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const REFERENT_ID: Uuid = uuid!("952d24ad-ce6a-5812-8c6c-33ff9d2b424d");

#[derive(Clone, Debug)]
pub struct ReferentProxy {
    inner: Arc<RwLock<Referent>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for ReferentProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "description" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().description.clone().into(),
                                )),
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "cardinality" => {
                                    let cardinality = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_cardinality(&self.inner.read().unwrap().cardinality)
                                        .unwrap();

                                    let this = CardinalityProxy {
                                        inner: cardinality,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: CARDINALITY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "conditionality" => {
                                    let conditionality = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_conditionality(
                                            &self.inner.read().unwrap().conditionality,
                                        )
                                        .unwrap();

                                    let this = ConditionalityProxy {
                                        inner: conditionality,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: CONDITIONALITY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "description" => {
                                    self.inner.write().unwrap().description =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "cardinality" => {
                                    self.inner.write().unwrap().cardinality =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "conditionality" => {
                                    self.inner.write().unwrap().conditionality =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Referent".into()
    }

    fn close(self) {}
}

impl Display for ReferentProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Referent({{")?;
        writeln!(
            f,
            "	description: {:?},",
            self.inner.read().unwrap().description
        )?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(
            f,
            "	cardinality: {:?},",
            self.inner.read().unwrap().cardinality
        )?;
        writeln!(
            f,
            "	conditionality: {:?},",
            self.inner.read().unwrap().conditionality
        )?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const REFERRER_ID: Uuid = uuid!("9c75abf3-b77e-56ee-a19c-d812898b5eaa");

#[derive(Clone, Debug)]
pub struct ReferrerProxy {
    inner: Arc<RwLock<Referrer>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for ReferrerProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "description" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().description.clone().into(),
                                )),
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "referential_attribute" => Ok(FfiValue::String(
                                    self.inner
                                        .read()
                                        .unwrap()
                                        .referential_attribute
                                        .clone()
                                        .into(),
                                )),
                                "cardinality" => {
                                    let cardinality = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_cardinality(&self.inner.read().unwrap().cardinality)
                                        .unwrap();

                                    let this = CardinalityProxy {
                                        inner: cardinality,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: CARDINALITY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "conditionality" => {
                                    let conditionality = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_conditionality(
                                            &self.inner.read().unwrap().conditionality,
                                        )
                                        .unwrap();

                                    let this = ConditionalityProxy {
                                        inner: conditionality,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: CONDITIONALITY_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "description" => {
                                    self.inner.write().unwrap().description =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "referential_attribute" => {
                                    self.inner.write().unwrap().referential_attribute =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "cardinality" => {
                                    self.inner.write().unwrap().cardinality =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "conditionality" => {
                                    self.inner.write().unwrap().conditionality =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Referrer".into()
    }

    fn close(self) {}
}

impl Display for ReferrerProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Referrer({{")?;
        writeln!(
            f,
            "	description: {:?},",
            self.inner.read().unwrap().description
        )?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(
            f,
            "	referential_attribute: {:?},",
            self.inner.read().unwrap().referential_attribute
        )?;
        writeln!(
            f,
            "	cardinality: {:?},",
            self.inner.read().unwrap().cardinality
        )?;
        writeln!(
            f,
            "	conditionality: {:?},",
            self.inner.read().unwrap().conditionality
        )?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const RELATIONSHIP_ID: Uuid = uuid!("469d77d1-9ede-5919-923d-b007d614af26");

#[derive(Clone, Debug)]
pub struct RelationshipProxy {
    inner: Arc<RwLock<Relationship>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for RelationshipProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Relationship".into()
    }

    fn close(self) {}
}

impl Display for RelationshipProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Relationship({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const STATE_ID: Uuid = uuid!("63af1589-c7cf-50b2-ad7b-d30208ebfec4");

#[derive(Clone, Debug)]
pub struct StateProxy {
    inner: Arc<RwLock<State>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for StateProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "name" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().name.clone().into(),
                                )),
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "name" => {
                                    self.inner.write().unwrap().name =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "State".into()
    }

    fn close(self) {}
}

impl Display for StateProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "State({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	name: {:?},", self.inner.read().unwrap().name)?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const S_STRING_ID: Uuid = uuid!("9803e73c-4984-5179-8460-529fe4ef7921");

#[derive(Clone, Debug)]
pub struct SStringProxy {
    inner: Arc<RwLock<SString>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for SStringProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "SString".into()
    }

    fn close(self) {}
}

impl Display for SStringProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "SString({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const SUBTYPE_ID: Uuid = uuid!("3abf0e04-6c8c-5e25-9638-43d98738ef87");

#[derive(Clone, Debug)]
pub struct SubtypeProxy {
    inner: Arc<RwLock<Subtype>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for SubtypeProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "isa" => {
                                    let isa = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_isa(&self.inner.read().unwrap().isa)
                                        .unwrap();

                                    let this = IsaProxy {
                                        inner: isa,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: ISA_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "isa" => {
                                    self.inner.write().unwrap().isa =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Subtype".into()
    }

    fn close(self) {}
}

impl Display for SubtypeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Subtype({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	isa: {:?},", self.inner.read().unwrap().isa)?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const SUPERTYPE_ID: Uuid = uuid!("a9cc5d3e-8431-5302-9296-1fbd789acf73");

#[derive(Clone, Debug)]
pub struct SupertypeProxy {
    inner: Arc<RwLock<Supertype>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for SupertypeProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "obj_id" => {
                                    let obj_id = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_object(&self.inner.read().unwrap().obj_id)
                                        .unwrap();

                                    let this = ObjectProxy {
                                        inner: obj_id,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: OBJECT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                "obj_id" => {
                                    self.inner.write().unwrap().obj_id =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Supertype".into()
    }

    fn close(self) {}
}

impl Display for SupertypeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Supertype({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	obj_id: {:?},", self.inner.read().unwrap().obj_id)?;
        writeln!(f, "}})")
    }
}

const TY_ID: Uuid = uuid!("b8ec6afc-ddbd-53d6-9be3-e4b738941c2f");

#[derive(Clone, Debug)]
pub struct TyProxy {
    inner: Arc<RwLock<Ty>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for TyProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Ty".into()
    }

    fn close(self) {}
}

impl Display for TyProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Ty({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const S_UUID_ID: Uuid = uuid!("9fcf72a7-a28e-5544-be44-af4de72db6e4");

#[derive(Clone, Debug)]
pub struct SUuidProxy {
    inner: Arc<RwLock<SUuid>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for SUuidProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "SUuid".into()
    }

    fn close(self) {}
}

impl Display for SUuidProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "SUuid({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const UNCONDITIONAL_ID: Uuid = uuid!("ab790409-b7ca-58d0-bb97-7c2ddd7b786f");

#[derive(Clone, Debug)]
pub struct UnconditionalProxy {
    inner: Arc<RwLock<Unconditional>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for UnconditionalProxy {
    fn invoke_func(
        &mut self,
        module: RStr<'_>,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, Error> {
        (|| -> Result<FfiValue, Error> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            match field.as_str() {
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id().into())),
                                _ => Err(Error::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(Error::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            match field.as_str() {
                                field => {
                                    return Err(Error::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber(format!("Invalid field type: {field:?}").into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(Error::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Unconditional".into()
    }

    fn close(self) {}
}

impl Display for UnconditionalProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Unconditional({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}
