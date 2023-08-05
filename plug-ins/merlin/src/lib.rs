use std::{
    cell::RefCell,
    fmt::{self, Display},
    path::Path,
    rc::Rc,
};

use abi_stable::{
    erased_types::TypeInfo,
    export_root_module,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::{TD_CanDowncast, TD_Opaque},
    std_types::{RBoxError, RErr, ROk, RResult, RStr, RString, RVec},
    DynTrait,
};
use dwarf::{
    chacha::value::{FfiProxy, FfiUuid, FfiValue, Value},
    plug_in::{
        Error as AppError, Plugin, PluginId, PluginModRef, PluginModule, PluginType, Plugin_TO,
        Unsupported,
    },
};
use log::debug;
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};

mod merlin;
use merlin::{ObjectStore, Point, PointEnum, INFLECTION};

///////////////////////////////////////////////////////////////////////////////////

/// Exports the root module of this library.
///
/// This code isn't run until the layout of the type it returns is checked.
#[export_root_module]
fn instantiate_root_module() -> PluginModRef {
    PluginModule { name, id, new }.leak_into_prefix()
}

//////////////////////////////////////////////////////////////////////////////////////

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
pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, AppError> {
    let this = if args.len() == 0 {
        MerlinStore {
            store: ObjectStore::new(),
        }
    } else if args.len() == 1 {
        if let FfiValue::String(path) = &args[0] {
            MerlinStore {
                // 🚧 fix this unwrap
                store: ObjectStore::load(Path::new(&path.as_str())).unwrap(),
            }
        } else {
            return RErr(AppError::Uber("Invalid arguments".into()));
        }
    } else {
        return RErr(AppError::Uber("Invalid arguments".into()));
    };

    ROk(Plugin_TO::from_value(this, TD_Opaque))
}

#[derive(Clone, Debug)]
struct MerlinStore {
    store: ObjectStore,
}

impl Display for MerlinStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //     f.debug_struct("SarzakStore")
        //         .field("store", &self.store)
        //         .field("plugin_id", &self.plugin_id)
        //         .finish()
        write!(f, "{:?}", self.store)
    }
}

impl Plugin for MerlinStore {
    fn invoke_func(
        &mut self,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, AppError> {
        (|| -> Result<FfiValue, AppError> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "Point" => match func {
                    "new_inflection" => {
                        if args.len() != 2 {
                            return Err(AppError::Uber("Expected 2 arguments".into()));
                        }

                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Point, AppError> {
                            let id = Uuid::new_v4();
                            let point = Point {
                                id,
                                subtype: PointEnum::Inflection(INFLECTION),
                                x: value_args.pop().unwrap().try_into().map_err(|e| {
                                    AppError::Uber(format!("Error converting value: {}", e).into())
                                })?,
                                y: value_args.pop().unwrap().try_into().map_err(|e| {
                                    AppError::Uber(format!("Error converting value: {}", e).into())
                                })?,
                            };

                            Ok(point)
                        })() {
                            Ok(point) => {
                                let this = PointProxy { inner: point };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    uuid: POINT_ID.into(),
                                    plugin: plugin.clone(),
                                };
                                self.invoke_func(
                                    "ObjectStore".into(),
                                    "inter_object".into(),
                                    vec![FfiValue::PlugIn(plugin)].into(),
                                )
                                .unwrap();
                                Ok(FfiValue::ProxyType(proxy))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    "instances" => {
                        let mut instances = Vec::new();
                        for point in self.store.iter_point() {
                            let this = PointProxy {
                                inner: (*point.borrow()).clone(),
                            };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                uuid: POINT_ID.into(),
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    func => Err(AppError::Uber(format!("Invalid function: {func:?}").into())),
                },
                "ObjectStore" => match func {
                    "inter_point" => {
                        if args.len() != 1 {
                            return Err(AppError::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(point) = args.pop().unwrap() {
                            let point = point.obj.downcast_into::<PointProxy>().unwrap();
                            self.store
                                .inter_point(Rc::new(RefCell::new(point.inner.clone())));
                            Ok(FfiValue::Empty)
                        } else {
                            Err(AppError::Uber("Invalid Point".into()))
                        }
                    }
                    func => Err(AppError::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(AppError::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "merlin".into()
    }

    fn close(self) {}
}

const POINT_ID: Uuid = uuid!("423935ca-86d2-5d0a-ad35-8e7f00663448");

#[derive(Clone, Debug)]
struct PointProxy {
    inner: Point,
}

impl Display for PointProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //     f.debug_struct("SarzakStore")
        //         .field("store", &self.store)
        //         .field("plugin_id", &self.plugin_id)
        //         .finish()
        write!(f, "{:?}", self.inner)
    }
}

impl Plugin for PointProxy {
    fn invoke_func(
        &mut self,
        ty: RStr<'_>,
        func: RStr<'_>,
        mut args: RVec<FfiValue>,
    ) -> RResult<FfiValue, AppError> {
        (|| -> Result<FfiValue, AppError> {
            let ty = ty.as_str();
            let func = func.as_str();
            debug!("type: {ty}, func: {func}, args: {args:?}");
            match ty {
                "self" => match func {
                    "get_field_value" => {
                        if args.len() != 1 {
                            return Err(AppError::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            let field: &str = field.as_str();
                            match field {
                                "subtype" => match self.inner.subtype {
                                    PointEnum::Inflection(_) => {
                                        Ok(FfiValue::String("Inflection".into()))
                                    }
                                    PointEnum::Bisection(_) => {
                                        Ok(FfiValue::String("Bisection".into()))
                                    }
                                    PointEnum::Anchor(_) => Ok(FfiValue::String("Anchor".into())),
                                },
                                "id" => Ok(FfiValue::Uuid(self.inner.id.into())),
                                "x" => Ok(FfiValue::Integer(self.inner.x.into())),
                                "y" => Ok(FfiValue::Integer(self.inner.y.into())),
                                _ => Err(AppError::Uber("Invalid field".into())),
                            }
                        } else {
                            Err(AppError::Uber("Invalid Object".into()))
                        }
                    }
                    "set_field_value" => {
                        if args.len() != 2 {
                            return Err(AppError::Uber("Expected 2 arguments".into()));
                        }

                        args.reverse();
                        let field = args.pop().unwrap();

                        if let FfiValue::String(field) = field {
                            let value: Value = args.pop().unwrap().into();
                            let field: &str = field.as_str();
                            match field {
                                "x" => {
                                    self.inner.x = value.try_into().map_err(|e| {
                                        AppError::Uber(
                                            format!("Error converting value: {}", e).into(),
                                        )
                                    })?
                                }
                                "y" => {
                                    self.inner.y = value.try_into().map_err(|e| {
                                        AppError::Uber(
                                            format!("Error converting value: {}", e).into(),
                                        )
                                    })?
                                }
                                field => {
                                    return Err(AppError::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(AppError::Uber(
                                format!("Invalid field type: {field:?}").into(),
                            ))
                        }
                    }
                    func => Err(AppError::Uber(format!("Invalid function: {func:?}").into())),
                },
                ty => Err(AppError::Uber(format!("Invalid type {ty:?}").into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Point".into()
    }

    fn close(self) {}
}
