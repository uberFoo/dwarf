//! This is the second iteration of the drawing domain. The first sucked.
//!
//! This domain represents the visual aspect of a model.
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

/// Exports the root module of this library.
///
/// This code isn't run until the layout of the type it returns is checked.
pub fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, id, new }.leak_into_prefix()
}

#[sabi_extern_fn]
pub fn name() -> RStr<'static> {
    "merlin".into()
}

#[sabi_extern_fn]
pub fn id() -> RStr<'static> {
    "merlin".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, Error> {
    match (|| {
        if args.len() == 0 {
            Ok(MerlinStore {
                store: Arc::new(RwLock::new(ObjectStore::new())),
            })
        } else if args.len() == 1 {
            if let FfiValue::String(path) = &args[0] {
                let store = ObjectStore::load(Path::new(&path.as_str())).unwrap();
                Ok(MerlinStore {
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
struct MerlinStore {
    store: Arc<RwLock<ObjectStore>>,
}

impl Display for MerlinStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.store)
    }
}

impl Plugin for MerlinStore {
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

                    "inter_anchor" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(anchor) = args.pop().unwrap() {
                            let anchor = anchor.obj.downcast_into::<AnchorProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_anchor(anchor.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Anchor".into()))
                        }
                    }
                    "exhume_anchor" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let anchor = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_anchor(&id.into())
                                .unwrap();
                            let anchor_proxy = AnchorProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: anchor.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(anchor_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ANCHOR_ID.into(),
                                id: anchor.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_bisection" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(bisection) = args.pop().unwrap() {
                            let bisection =
                                bisection.obj.downcast_into::<BisectionProxy>().unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_bisection(bisection.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Bisection".into()))
                        }
                    }
                    "exhume_bisection" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let bisection = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_bisection(&id.into())
                                .unwrap();
                            let bisection_proxy = BisectionProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: bisection.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(bisection_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: BISECTION_ID.into(),
                                id: bisection.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_x_box" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(x_box) = args.pop().unwrap() {
                            let x_box = x_box.obj.downcast_into::<XBoxProxy>().unwrap();
                            self.store.write().unwrap().inter_x_box(x_box.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid XBox".into()))
                        }
                    }
                    "exhume_x_box" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let x_box =
                                self.store.read().unwrap().exhume_x_box(&id.into()).unwrap();
                            let x_box_proxy = XBoxProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: x_box.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(x_box_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: X_BOX_ID.into(),
                                id: x_box.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_edge" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(edge) = args.pop().unwrap() {
                            let edge = edge.obj.downcast_into::<EdgeProxy>().unwrap();
                            self.store.write().unwrap().inter_edge(edge.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Edge".into()))
                        }
                    }
                    "exhume_edge" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let edge = self.store.read().unwrap().exhume_edge(&id.into()).unwrap();
                            let edge_proxy = EdgeProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: edge.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(edge_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: EDGE_ID.into(),
                                id: edge.read().unwrap().id().into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_glyph" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(glyph) = args.pop().unwrap() {
                            let glyph = glyph.obj.downcast_into::<GlyphProxy>().unwrap();
                            self.store.write().unwrap().inter_glyph(glyph.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Glyph".into()))
                        }
                    }
                    "exhume_glyph" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let glyph =
                                self.store.read().unwrap().exhume_glyph(&id.into()).unwrap();
                            let glyph_proxy = GlyphProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: glyph.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(glyph_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: GLYPH_ID.into(),
                                id: glyph.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_line" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(line) = args.pop().unwrap() {
                            let line = line.obj.downcast_into::<LineProxy>().unwrap();
                            self.store.write().unwrap().inter_line(line.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Line".into()))
                        }
                    }
                    "exhume_line" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let line = self.store.read().unwrap().exhume_line(&id.into()).unwrap();
                            let line_proxy = LineProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: line.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(line_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: LINE_ID.into(),
                                id: line.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_line_segment" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(line_segment) = args.pop().unwrap() {
                            let line_segment = line_segment
                                .obj
                                .downcast_into::<LineSegmentProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_line_segment(line_segment.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid LineSegment".into()))
                        }
                    }
                    "exhume_line_segment" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let line_segment = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_line_segment(&id.into())
                                .unwrap();
                            let line_segment_proxy = LineSegmentProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: line_segment.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(line_segment_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: LINE_SEGMENT_ID.into(),
                                id: line_segment.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_line_segment_point" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(line_segment_point) = args.pop().unwrap() {
                            let line_segment_point = line_segment_point
                                .obj
                                .downcast_into::<LineSegmentPointProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_line_segment_point(line_segment_point.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid LineSegmentPoint".into()))
                        }
                    }
                    "exhume_line_segment_point" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let line_segment_point = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_line_segment_point(&id.into())
                                .unwrap();
                            let line_segment_point_proxy = LineSegmentPointProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: line_segment_point.clone(),
                                store: self.store.clone(),
                            };
                            let plugin =
                                Plugin_TO::from_value(line_segment_point_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: LINE_SEGMENT_POINT_ID.into(),
                                id: line_segment_point.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_point" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(point) = args.pop().unwrap() {
                            let point = point.obj.downcast_into::<PointProxy>().unwrap();
                            self.store.write().unwrap().inter_point(point.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid Point".into()))
                        }
                    }
                    "exhume_point" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let point =
                                self.store.read().unwrap().exhume_point(&id.into()).unwrap();
                            let point_proxy = PointProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: point.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(point_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: POINT_ID.into(),
                                id: point.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_relationship_name" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(relationship_name) = args.pop().unwrap() {
                            let relationship_name = relationship_name
                                .obj
                                .downcast_into::<RelationshipNameProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_relationship_name(relationship_name.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid RelationshipName".into()))
                        }
                    }
                    "exhume_relationship_name" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let relationship_name = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_relationship_name(&id.into())
                                .unwrap();
                            let relationship_name_proxy = RelationshipNameProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: relationship_name.clone(),
                                store: self.store.clone(),
                            };
                            let plugin =
                                Plugin_TO::from_value(relationship_name_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: RELATIONSHIP_NAME_ID.into(),
                                id: relationship_name.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    "inter_relationship_phrase" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(relationship_phrase) = args.pop().unwrap() {
                            let relationship_phrase = relationship_phrase
                                .obj
                                .downcast_into::<RelationshipPhraseProxy>()
                                .unwrap();
                            self.store
                                .write()
                                .unwrap()
                                .inter_relationship_phrase(relationship_phrase.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(Error::Uber("Invalid RelationshipPhrase".into()))
                        }
                    }
                    "exhume_relationship_phrase" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 argument".into()));
                        }
                        if let FfiValue::Uuid(id) = args.pop().unwrap() {
                            let relationship_phrase = self
                                .store
                                .read()
                                .unwrap()
                                .exhume_relationship_phrase(&id.into())
                                .unwrap();
                            let relationship_phrase_proxy = RelationshipPhraseProxy {
                                // 🚧 This bothers me deeply. I know that I've given
                                // this some thought already, and I really need to
                                // document the outcome so that I can stop worrying
                                // over it.
                                inner: relationship_phrase.clone(),
                                store: self.store.clone(),
                            };
                            let plugin =
                                Plugin_TO::from_value(relationship_phrase_proxy, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: RELATIONSHIP_PHRASE_ID.into(),
                                id: relationship_phrase.read().unwrap().id.into(), // a
                                plugin: plugin.clone(),
                            };

                            Ok(FfiValue::ProxyType(proxy))
                        } else {
                            Err(Error::Uber("Invalid id".into()))
                        }
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Anchor" => match func {
                    "new" => {
                        if args.len() != 7 {
                            return Err(Error::Uber("Expected 7 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Anchor, Error> {
                            let id = Uuid::new_v4();
                            let anchor = Anchor {
                                id,
                                offset: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                x_offset: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                y_offset: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                edge: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                glyph: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                x_box: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(anchor)
                        })() {
                            Ok(anchor) => {
                                let anchor = Arc::new(RwLock::new(anchor));
                                self.store.write().unwrap().inter_anchor(anchor.clone());
                                let this = AnchorProxy {
                                    inner: anchor.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: ANCHOR_ID.into(),
                                    id: anchor.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for anchor in self.store.read().unwrap().iter_anchor() {
                            let this = AnchorProxy {
                                inner: anchor.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: ANCHOR_ID.into(),
                                id: anchor.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Bisection" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Bisection, Error> {
                            let id = Uuid::new_v4();
                            let bisection = Bisection {
                                id,
                                offset: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                segment: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(bisection)
                        })() {
                            Ok(bisection) => {
                                let bisection = Arc::new(RwLock::new(bisection));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_bisection(bisection.clone());
                                let this = BisectionProxy {
                                    inner: bisection.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: BISECTION_ID.into(),
                                    id: bisection.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for bisection in self.store.read().unwrap().iter_bisection() {
                            let this = BisectionProxy {
                                inner: bisection.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: BISECTION_ID.into(),
                                id: bisection.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "XBox" => match func {
                    "new" => {
                        if args.len() != 5 {
                            return Err(Error::Uber("Expected 5 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<XBox, Error> {
                            let id = Uuid::new_v4();
                            let x_box = XBox {
                                id,
                                height: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                width: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                x: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                y: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                object: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(x_box)
                        })() {
                            Ok(x_box) => {
                                let x_box = Arc::new(RwLock::new(x_box));
                                self.store.write().unwrap().inter_x_box(x_box.clone());
                                let this = XBoxProxy {
                                    inner: x_box.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: X_BOX_ID.into(),
                                    id: x_box.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for x_box in self.store.read().unwrap().iter_x_box() {
                            let this = XBoxProxy {
                                inner: x_box.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: X_BOX_ID.into(),
                                id: x_box.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Edge" => match func {
                    "new_bottom" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Edge>>, Error> {
                            let bottom = self.store.read().unwrap().exhume_edge(&BOTTOM).unwrap();

                            Ok(bottom)
                        })() {
                            Ok(bottom) => {
                                let this = EdgeProxy {
                                    inner: bottom.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: BOTTOM.into(),
                                    id: bottom.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_left" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Edge>>, Error> {
                            let left = self.store.read().unwrap().exhume_edge(&LEFT).unwrap();

                            Ok(left)
                        })() {
                            Ok(left) => {
                                let this = EdgeProxy {
                                    inner: left.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: LEFT.into(),
                                    id: left.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_right" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Edge>>, Error> {
                            let right = self.store.read().unwrap().exhume_edge(&RIGHT).unwrap();

                            Ok(right)
                        })() {
                            Ok(right) => {
                                let this = EdgeProxy {
                                    inner: right.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: RIGHT.into(),
                                    id: right.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_top" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Arc<RwLock<Edge>>, Error> {
                            let top = self.store.read().unwrap().exhume_edge(&TOP).unwrap();

                            Ok(top)
                        })() {
                            Ok(top) => {
                                let this = EdgeProxy {
                                    inner: top.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: TOP.into(),
                                    id: top.read().unwrap().id().into(),
                                    plugin: plugin.clone(),
                                };
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for edge in self.store.read().unwrap().iter_edge() {
                            let this = EdgeProxy {
                                inner: edge.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: EDGE_ID.into(),
                                id: edge.read().unwrap().id().into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Glyph" => match func {
                    "new_many" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Glyph, Error> {
                            let id = Uuid::new_v4();
                            let many = Glyph {
                                id,
                                subtype: GlyphEnum::Many(
                                    value_args.pop().unwrap().try_into().map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                ),
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(many)
                        })() {
                            Ok(many) => {
                                let many = Arc::new(RwLock::new(many));
                                self.store.write().unwrap().inter_glyph(many.clone());
                                let this = GlyphProxy {
                                    inner: many.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: GLYPH_ID.into(),
                                    id: many.read().unwrap().id.into(), // d
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
                        match (|| -> Result<Glyph, Error> {
                            let id = Uuid::new_v4();
                            let one = Glyph {
                                id,
                                subtype: GlyphEnum::One(
                                    value_args.pop().unwrap().try_into().map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                ),
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(one)
                        })() {
                            Ok(one) => {
                                let one = Arc::new(RwLock::new(one));
                                self.store.write().unwrap().inter_glyph(one.clone());
                                let this = GlyphProxy {
                                    inner: one.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: GLYPH_ID.into(),
                                    id: one.read().unwrap().id.into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_sub" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Glyph, Error> {
                            let id = Uuid::new_v4();
                            let sub = Glyph {
                                id,
                                subtype: GlyphEnum::Sub(
                                    value_args.pop().unwrap().try_into().map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                ),
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(sub)
                        })() {
                            Ok(sub) => {
                                let sub = Arc::new(RwLock::new(sub));
                                self.store.write().unwrap().inter_glyph(sub.clone());
                                let this = GlyphProxy {
                                    inner: sub.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: GLYPH_ID.into(),
                                    id: sub.read().unwrap().id.into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_x_super" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Glyph, Error> {
                            let id = Uuid::new_v4();
                            let x_super = Glyph {
                                id,
                                subtype: GlyphEnum::XSuper(
                                    value_args.pop().unwrap().try_into().map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                ),
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(x_super)
                        })() {
                            Ok(x_super) => {
                                let x_super = Arc::new(RwLock::new(x_super));
                                self.store.write().unwrap().inter_glyph(x_super.clone());
                                let this = GlyphProxy {
                                    inner: x_super.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: GLYPH_ID.into(),
                                    id: x_super.read().unwrap().id.into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for glyph in self.store.read().unwrap().iter_glyph() {
                            let this = GlyphProxy {
                                inner: glyph.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: GLYPH_ID.into(),
                                id: glyph.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Line" => match func {
                    "new" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Line, Error> {
                            let id = Uuid::new_v4();
                            let line = Line {
                                id,
                                relationship: value_args.pop().unwrap().try_into().map_err(
                                    |e| Error::Uber(format!("Error converting value: {e}").into()),
                                )?,
                            };

                            Ok(line)
                        })() {
                            Ok(line) => {
                                let line = Arc::new(RwLock::new(line));
                                self.store.write().unwrap().inter_line(line.clone());
                                let this = LineProxy {
                                    inner: line.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: LINE_ID.into(),
                                    id: line.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for line in self.store.read().unwrap().iter_line() {
                            let this = LineProxy {
                                inner: line.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: LINE_ID.into(),
                                id: line.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "LineSegment" => match func {
                    "new" => {
                        if args.len() != 1 {
                            return Err(Error::Uber("Expected 1 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<LineSegment, Error> {
                            let id = Uuid::new_v4();
                            let line_segment = LineSegment {
                                id,
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(line_segment)
                        })() {
                            Ok(line_segment) => {
                                let line_segment = Arc::new(RwLock::new(line_segment));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_line_segment(line_segment.clone());
                                let this = LineSegmentProxy {
                                    inner: line_segment.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: LINE_SEGMENT_ID.into(),
                                    id: line_segment.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for line_segment in self.store.read().unwrap().iter_line_segment() {
                            let this = LineSegmentProxy {
                                inner: line_segment.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: LINE_SEGMENT_ID.into(),
                                id: line_segment.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "LineSegmentPoint" => match func {
                    "new" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<LineSegmentPoint, Error> {
                            let id = Uuid::new_v4();
                            let line_segment_point = LineSegmentPoint {
                                id,
                                segment: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                point: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(line_segment_point)
                        })() {
                            Ok(line_segment_point) => {
                                let line_segment_point = Arc::new(RwLock::new(line_segment_point));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_line_segment_point(line_segment_point.clone());
                                let this = LineSegmentPointProxy {
                                    inner: line_segment_point.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: LINE_SEGMENT_POINT_ID.into(),
                                    id: line_segment_point.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for line_segment_point in
                            self.store.read().unwrap().iter_line_segment_point()
                        {
                            let this = LineSegmentPointProxy {
                                inner: line_segment_point.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: LINE_SEGMENT_POINT_ID.into(),
                                id: line_segment_point.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "Point" => match func {
                    "new_anchor" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Point, Error> {
                            let id = Uuid::new_v4();
                            let anchor = Point {
                                id,
                                subtype: PointEnum::Anchor(
                                    value_args.pop().unwrap().try_into().map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                ),
                                x: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                y: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(anchor)
                        })() {
                            Ok(anchor) => {
                                let anchor = Arc::new(RwLock::new(anchor));
                                self.store.write().unwrap().inter_point(anchor.clone());
                                let this = PointProxy {
                                    inner: anchor.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: POINT_ID.into(),
                                    id: anchor.read().unwrap().id.into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_bisection" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Point, Error> {
                            let id = Uuid::new_v4();
                            let bisection = Point {
                                id,
                                subtype: PointEnum::Bisection(
                                    value_args.pop().unwrap().try_into().map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                ),
                                x: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                y: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(bisection)
                        })() {
                            Ok(bisection) => {
                                let bisection = Arc::new(RwLock::new(bisection));
                                self.store.write().unwrap().inter_point(bisection.clone());
                                let this = PointProxy {
                                    inner: bisection.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: POINT_ID.into(),
                                    id: bisection.read().unwrap().id.into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "new_inflection" => {
                        if args.len() != 2 {
                            return Err(Error::Uber("Expected 2 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Point, Error> {
                            let id = Uuid::new_v4();
                            let inflection = Point {
                                id,
                                subtype: PointEnum::Inflection(
                                    value_args.pop().unwrap().try_into().map_err(|e| {
                                        Error::Uber(format!("Error converting value: {e}").into())
                                    })?,
                                ),
                                x: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                y: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(inflection)
                        })() {
                            Ok(inflection) => {
                                let inflection = Arc::new(RwLock::new(inflection));
                                self.store.write().unwrap().inter_point(inflection.clone());
                                let this = PointProxy {
                                    inner: inflection.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: POINT_ID.into(),
                                    id: inflection.read().unwrap().id.into(), // d
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for point in self.store.read().unwrap().iter_point() {
                            let this = PointProxy {
                                inner: point.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: POINT_ID.into(),
                                id: point.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "RelationshipName" => match func {
                    "new" => {
                        if args.len() != 5 {
                            return Err(Error::Uber("Expected 5 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<RelationshipName, Error> {
                            let id = Uuid::new_v4();
                            let relationship_name = RelationshipName {
                                id,
                                text: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                x: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                y: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                origin: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(relationship_name)
                        })() {
                            Ok(relationship_name) => {
                                let relationship_name = Arc::new(RwLock::new(relationship_name));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_relationship_name(relationship_name.clone());
                                let this = RelationshipNameProxy {
                                    inner: relationship_name.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: RELATIONSHIP_NAME_ID.into(),
                                    id: relationship_name.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for relationship_name in self.store.read().unwrap().iter_relationship_name()
                        {
                            let this = RelationshipNameProxy {
                                inner: relationship_name.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: RELATIONSHIP_NAME_ID.into(),
                                id: relationship_name.read().unwrap().id.into(), // b
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(Error::Uber(format!("Invalid function: {func:?}").into())),
                },

                "RelationshipPhrase" => match func {
                    "new" => {
                        if args.len() != 5 {
                            return Err(Error::Uber("Expected 5 arguments".into()));
                        }
                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<RelationshipPhrase, Error> {
                            let id = Uuid::new_v4();
                            let relationship_phrase = RelationshipPhrase {
                                id,
                                text: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                x: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                y: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                line: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                                origin: value_args.pop().unwrap().try_into().map_err(|e| {
                                    Error::Uber(format!("Error converting value: {e}").into())
                                })?,
                            };

                            Ok(relationship_phrase)
                        })() {
                            Ok(relationship_phrase) => {
                                let relationship_phrase =
                                    Arc::new(RwLock::new(relationship_phrase));
                                self.store
                                    .write()
                                    .unwrap()
                                    .inter_relationship_phrase(relationship_phrase.clone());
                                let this = RelationshipPhraseProxy {
                                    inner: relationship_phrase.clone(),
                                    store: self.store.clone(),
                                };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    module: module.into(),
                                    uuid: RELATIONSHIP_PHRASE_ID.into(),
                                    id: relationship_phrase.read().unwrap().id.into(), // e
                                    plugin: plugin.clone(),
                                };

                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for relationship_phrase in
                            self.store.read().unwrap().iter_relationship_phrase()
                        {
                            let this = RelationshipPhraseProxy {
                                inner: relationship_phrase.clone(),
                                store: self.store.clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                module: module.into(),
                                uuid: RELATIONSHIP_PHRASE_ID.into(),
                                id: relationship_phrase.read().unwrap().id.into(), // b
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

const ANCHOR_ID: Uuid = uuid!("27edcc78-f257-5a0b-a2e4-c233987e0889");

#[derive(Clone, Debug)]
pub struct AnchorProxy {
    inner: Arc<RwLock<Anchor>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for AnchorProxy {
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
                                "offset" => {
                                    Ok(FfiValue::Float(self.inner.read().unwrap().offset.into()))
                                }
                                "x_offset" => Ok(FfiValue::Integer(
                                    self.inner.read().unwrap().x_offset.into(),
                                )),
                                "y_offset" => Ok(FfiValue::Integer(
                                    self.inner.read().unwrap().y_offset.into(),
                                )),
                                "edge" => {
                                    let edge = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_edge(&self.inner.read().unwrap().edge)
                                        .unwrap();

                                    let this = EdgeProxy {
                                        inner: edge,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: EDGE_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "glyph" => {
                                    let glyph = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_glyph(&self.inner.read().unwrap().glyph)
                                        .unwrap();

                                    let this = GlyphProxy {
                                        inner: glyph,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: GLYPH_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "x_box" => {
                                    let x_box = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_x_box(&self.inner.read().unwrap().x_box)
                                        .unwrap();

                                    let this = XBoxProxy {
                                        inner: x_box,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: X_BOX_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "line" => {
                                    let line = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_line(&self.inner.read().unwrap().line)
                                        .unwrap();

                                    let this = LineProxy {
                                        inner: line,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: LINE_ID.into(),
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
                                "offset" => {
                                    self.inner.write().unwrap().offset =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "x_offset" => {
                                    self.inner.write().unwrap().x_offset =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "y_offset" => {
                                    self.inner.write().unwrap().y_offset =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "edge" => {
                                    self.inner.write().unwrap().edge =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "glyph" => {
                                    self.inner.write().unwrap().glyph =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "x_box" => {
                                    self.inner.write().unwrap().x_box =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "line" => {
                                    self.inner.write().unwrap().line =
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
        "Anchor".into()
    }

    fn close(self) {}
}

impl Display for AnchorProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Anchor({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	offset: {:?},", self.inner.read().unwrap().offset)?;
        writeln!(f, "	x_offset: {:?},", self.inner.read().unwrap().x_offset)?;
        writeln!(f, "	y_offset: {:?},", self.inner.read().unwrap().y_offset)?;
        writeln!(f, "	edge: {:?},", self.inner.read().unwrap().edge)?;
        writeln!(f, "	glyph: {:?},", self.inner.read().unwrap().glyph)?;
        writeln!(f, "	x_box: {:?},", self.inner.read().unwrap().x_box)?;
        writeln!(f, "	line: {:?},", self.inner.read().unwrap().line)?;
        writeln!(f, "}})")
    }
}

const BISECTION_ID: Uuid = uuid!("f6496c3c-adfa-5cf5-80b3-21bf2f0d7040");

#[derive(Clone, Debug)]
pub struct BisectionProxy {
    inner: Arc<RwLock<Bisection>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for BisectionProxy {
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
                                "offset" => {
                                    Ok(FfiValue::Float(self.inner.read().unwrap().offset.into()))
                                }
                                "segment" => {
                                    let segment = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_line_segment(&self.inner.read().unwrap().segment)
                                        .unwrap();

                                    let this = LineSegmentProxy {
                                        inner: segment,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: LINE_SEGMENT_ID.into(),
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
                                "offset" => {
                                    self.inner.write().unwrap().offset =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "segment" => {
                                    self.inner.write().unwrap().segment =
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
        "Bisection".into()
    }

    fn close(self) {}
}

impl Display for BisectionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Bisection({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	offset: {:?},", self.inner.read().unwrap().offset)?;
        writeln!(f, "	segment: {:?},", self.inner.read().unwrap().segment)?;
        writeln!(f, "}})")
    }
}

const BOTTOM_ID: Uuid = uuid!("dd577182-9bf1-591f-91eb-9a368ac0db86");

#[derive(Clone, Debug)]
pub struct BottomProxy {
    inner: Arc<RwLock<Bottom>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for BottomProxy {
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
        "Bottom".into()
    }

    fn close(self) {}
}

impl Display for BottomProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Bottom({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const X_BOX_ID: Uuid = uuid!("a27db16f-fea8-5db0-9e1e-30b12486bb75");

#[derive(Clone, Debug)]
pub struct XBoxProxy {
    inner: Arc<RwLock<XBox>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for XBoxProxy {
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
                                "height" => {
                                    Ok(FfiValue::Integer(self.inner.read().unwrap().height.into()))
                                }
                                "id" => Ok(FfiValue::Uuid(self.inner.read().unwrap().id.into())),
                                "width" => {
                                    Ok(FfiValue::Integer(self.inner.read().unwrap().width.into()))
                                }
                                "x" => Ok(FfiValue::Integer(self.inner.read().unwrap().x.into())),
                                "y" => Ok(FfiValue::Integer(self.inner.read().unwrap().y.into())),
                                "object" => {
                                    Err(Error::Uber("Imported object not supported.".into()))
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
                                "height" => {
                                    self.inner.write().unwrap().height =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "width" => {
                                    self.inner.write().unwrap().width =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "x" => {
                                    self.inner.write().unwrap().x =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "y" => {
                                    self.inner.write().unwrap().y =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "object" => {
                                    self.inner.write().unwrap().object =
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
        "XBox".into()
    }

    fn close(self) {}
}

impl Display for XBoxProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "XBox({{")?;
        writeln!(f, "	height: {:?},", self.inner.read().unwrap().height)?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	width: {:?},", self.inner.read().unwrap().width)?;
        writeln!(f, "	x: {:?},", self.inner.read().unwrap().x)?;
        writeln!(f, "	y: {:?},", self.inner.read().unwrap().y)?;
        writeln!(f, "	object: {:?},", self.inner.read().unwrap().object)?;
        writeln!(f, "}})")
    }
}

const EDGE_ID: Uuid = uuid!("d01f2378-3539-5b5f-ad97-0d0558f7d40e");

#[derive(Clone, Debug)]
pub struct EdgeProxy {
    inner: Arc<RwLock<Edge>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for EdgeProxy {
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
        "Edge".into()
    }

    fn close(self) {}
}

impl Display for EdgeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Edge({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const GLYPH_ID: Uuid = uuid!("47ccc17a-dde2-54b8-8d70-33b8aa683b36");

#[derive(Clone, Debug)]
pub struct GlyphProxy {
    inner: Arc<RwLock<Glyph>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for GlyphProxy {
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
                                "line" => {
                                    let line = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_line(&self.inner.read().unwrap().line)
                                        .unwrap();

                                    let this = LineProxy {
                                        inner: line,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: LINE_ID.into(),
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
                                "line" => {
                                    self.inner.write().unwrap().line =
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
        "Glyph".into()
    }

    fn close(self) {}
}

impl Display for GlyphProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Glyph({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	line: {:?},", self.inner.read().unwrap().line)?;
        writeln!(f, "}})")
    }
}

const INFLECTION_ID: Uuid = uuid!("5a71b258-b726-542b-b2f5-050e31b1c6ac");

#[derive(Clone, Debug)]
pub struct InflectionProxy {
    inner: Arc<RwLock<Inflection>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for InflectionProxy {
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
        "Inflection".into()
    }

    fn close(self) {}
}

impl Display for InflectionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Inflection({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const LEFT_ID: Uuid = uuid!("b1469430-1459-57f8-a932-751cc9cdc125");

#[derive(Clone, Debug)]
pub struct LeftProxy {
    inner: Arc<RwLock<Left>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for LeftProxy {
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
        "Left".into()
    }

    fn close(self) {}
}

impl Display for LeftProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Left({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const LINE_ID: Uuid = uuid!("c8778dc8-ae80-5211-99f3-48982bce758e");

#[derive(Clone, Debug)]
pub struct LineProxy {
    inner: Arc<RwLock<Line>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for LineProxy {
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
                                "relationship" => {
                                    Err(Error::Uber("Imported object not supported.".into()))
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
                                "relationship" => {
                                    self.inner.write().unwrap().relationship =
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
        "Line".into()
    }

    fn close(self) {}
}

impl Display for LineProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Line({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(
            f,
            "	relationship: {:?},",
            self.inner.read().unwrap().relationship
        )?;
        writeln!(f, "}})")
    }
}

const LINE_SEGMENT_ID: Uuid = uuid!("f09d5cf8-9778-5b41-a50c-87e8670e93dd");

#[derive(Clone, Debug)]
pub struct LineSegmentProxy {
    inner: Arc<RwLock<LineSegment>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for LineSegmentProxy {
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
                                "line" => {
                                    let line = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_line(&self.inner.read().unwrap().line)
                                        .unwrap();

                                    let this = LineProxy {
                                        inner: line,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: LINE_ID.into(),
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
                                "line" => {
                                    self.inner.write().unwrap().line =
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
        "LineSegment".into()
    }

    fn close(self) {}
}

impl Display for LineSegmentProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "LineSegment({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	line: {:?},", self.inner.read().unwrap().line)?;
        writeln!(f, "}})")
    }
}

const LINE_SEGMENT_POINT_ID: Uuid = uuid!("49615ec3-09d3-54d9-b32c-ebd5741f7af8");

#[derive(Clone, Debug)]
pub struct LineSegmentPointProxy {
    inner: Arc<RwLock<LineSegmentPoint>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for LineSegmentPointProxy {
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
                                "segment" => {
                                    let segment = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_line_segment(&self.inner.read().unwrap().segment)
                                        .unwrap();

                                    let this = LineSegmentProxy {
                                        inner: segment,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: LINE_SEGMENT_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "point" => {
                                    let point = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_point(&self.inner.read().unwrap().point)
                                        .unwrap();

                                    let this = PointProxy {
                                        inner: point,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: POINT_ID.into(),
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
                                "segment" => {
                                    self.inner.write().unwrap().segment =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "point" => {
                                    self.inner.write().unwrap().point =
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
        "LineSegmentPoint".into()
    }

    fn close(self) {}
}

impl Display for LineSegmentPointProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "LineSegmentPoint({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	segment: {:?},", self.inner.read().unwrap().segment)?;
        writeln!(f, "	point: {:?},", self.inner.read().unwrap().point)?;
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

const POINT_ID: Uuid = uuid!("423935ca-86d2-5d0a-ad35-8e7f00663448");

#[derive(Clone, Debug)]
pub struct PointProxy {
    inner: Arc<RwLock<Point>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for PointProxy {
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
                                "x" => Ok(FfiValue::Integer(self.inner.read().unwrap().x.into())),
                                "y" => Ok(FfiValue::Integer(self.inner.read().unwrap().y.into())),
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
                                "x" => {
                                    self.inner.write().unwrap().x =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "y" => {
                                    self.inner.write().unwrap().y =
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
        "Point".into()
    }

    fn close(self) {}
}

impl Display for PointProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Point({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	x: {:?},", self.inner.read().unwrap().x)?;
        writeln!(f, "	y: {:?},", self.inner.read().unwrap().y)?;
        writeln!(f, "}})")
    }
}

const RELATIONSHIP_NAME_ID: Uuid = uuid!("a6cad864-7edb-5a36-a3d9-c43df43fd140");

#[derive(Clone, Debug)]
pub struct RelationshipNameProxy {
    inner: Arc<RwLock<RelationshipName>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for RelationshipNameProxy {
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
                                "text" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().text.clone().into(),
                                )),
                                "x" => Ok(FfiValue::Integer(self.inner.read().unwrap().x.into())),
                                "y" => Ok(FfiValue::Integer(self.inner.read().unwrap().y.into())),
                                "line" => {
                                    let line = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_line(&self.inner.read().unwrap().line)
                                        .unwrap();

                                    let this = LineProxy {
                                        inner: line,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: LINE_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "origin" => {
                                    let origin = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_bisection(&self.inner.read().unwrap().origin)
                                        .unwrap();

                                    let this = BisectionProxy {
                                        inner: origin,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: BISECTION_ID.into(),
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
                                "text" => {
                                    self.inner.write().unwrap().text =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "x" => {
                                    self.inner.write().unwrap().x =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "y" => {
                                    self.inner.write().unwrap().y =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "line" => {
                                    self.inner.write().unwrap().line =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "origin" => {
                                    self.inner.write().unwrap().origin =
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
        "RelationshipName".into()
    }

    fn close(self) {}
}

impl Display for RelationshipNameProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "RelationshipName({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	text: {:?},", self.inner.read().unwrap().text)?;
        writeln!(f, "	x: {:?},", self.inner.read().unwrap().x)?;
        writeln!(f, "	y: {:?},", self.inner.read().unwrap().y)?;
        writeln!(f, "	line: {:?},", self.inner.read().unwrap().line)?;
        writeln!(f, "	origin: {:?},", self.inner.read().unwrap().origin)?;
        writeln!(f, "}})")
    }
}

const RELATIONSHIP_PHRASE_ID: Uuid = uuid!("ba4b2db0-a361-5e9b-a3d4-1aab7ebe55b0");

#[derive(Clone, Debug)]
pub struct RelationshipPhraseProxy {
    inner: Arc<RwLock<RelationshipPhrase>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for RelationshipPhraseProxy {
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
                                "text" => Ok(FfiValue::String(
                                    self.inner.read().unwrap().text.clone().into(),
                                )),
                                "x" => Ok(FfiValue::Integer(self.inner.read().unwrap().x.into())),
                                "y" => Ok(FfiValue::Integer(self.inner.read().unwrap().y.into())),
                                "line" => {
                                    let line = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_line(&self.inner.read().unwrap().line)
                                        .unwrap();

                                    let this = LineProxy {
                                        inner: line,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: LINE_ID.into(),
                                        id: self.inner.read().unwrap().id.into(), // c
                                        plugin: plugin.clone(),
                                    };
                                    Ok(FfiValue::ProxyType(proxy))
                                }
                                "origin" => {
                                    let origin = self
                                        .store
                                        .read()
                                        .unwrap()
                                        .exhume_anchor(&self.inner.read().unwrap().origin)
                                        .unwrap();

                                    let this = AnchorProxy {
                                        inner: origin,
                                        store: self.store.clone(),
                                    };
                                    let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                    let proxy = FfiProxy {
                                        module: module.into(),
                                        uuid: ANCHOR_ID.into(),
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
                                "text" => {
                                    self.inner.write().unwrap().text =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "x" => {
                                    self.inner.write().unwrap().x =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "y" => {
                                    self.inner.write().unwrap().y =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "line" => {
                                    self.inner.write().unwrap().line =
                                        value.try_into().map_err(|e| {
                                            Error::Uber(
                                                format!("Error converting value: {e}").into(),
                                            )
                                        })?
                                }
                                "origin" => {
                                    self.inner.write().unwrap().origin =
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
        "RelationshipPhrase".into()
    }

    fn close(self) {}
}

impl Display for RelationshipPhraseProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "RelationshipPhrase({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id)?;
        writeln!(f, "	text: {:?},", self.inner.read().unwrap().text)?;
        writeln!(f, "	x: {:?},", self.inner.read().unwrap().x)?;
        writeln!(f, "	y: {:?},", self.inner.read().unwrap().y)?;
        writeln!(f, "	line: {:?},", self.inner.read().unwrap().line)?;
        writeln!(f, "	origin: {:?},", self.inner.read().unwrap().origin)?;
        writeln!(f, "}})")
    }
}

const RIGHT_ID: Uuid = uuid!("45385874-931f-5e9c-a5f8-b12558e3a535");

#[derive(Clone, Debug)]
pub struct RightProxy {
    inner: Arc<RwLock<Right>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for RightProxy {
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
        "Right".into()
    }

    fn close(self) {}
}

impl Display for RightProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Right({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const SUB_ID: Uuid = uuid!("146d7a75-c86b-59a7-a52a-ac522d748a47");

#[derive(Clone, Debug)]
pub struct SubProxy {
    inner: Arc<RwLock<Sub>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for SubProxy {
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
        "Sub".into()
    }

    fn close(self) {}
}

impl Display for SubProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Sub({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const X_SUPER_ID: Uuid = uuid!("0cbeeb50-21ce-5e83-9f2e-65d1410d553f");

#[derive(Clone, Debug)]
pub struct XSuperProxy {
    inner: Arc<RwLock<XSuper>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for XSuperProxy {
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
        "XSuper".into()
    }

    fn close(self) {}
}

impl Display for XSuperProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "XSuper({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}

const TOP_ID: Uuid = uuid!("a04b0262-b3be-5721-a8b0-0e790b509243");

#[derive(Clone, Debug)]
pub struct TopProxy {
    inner: Arc<RwLock<Top>>,
    store: Arc<RwLock<ObjectStore>>,
}

impl Plugin for TopProxy {
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
        "Top".into()
    }

    fn close(self) {}
}

impl Display for TopProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Top({{")?;
        writeln!(f, "	id: {:?},", self.inner.read().unwrap().id())?;
        writeln!(f, "}})")
    }
}
