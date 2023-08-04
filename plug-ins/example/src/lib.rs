use std::{
    any::Any,
    collections::VecDeque,
    fmt::{self, Display},
    path::Path,
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
    chacha::value::{FfiProxy, FfiUuid, FfiValue},
    plug_in::{
        Error as AppError, Plugin, PluginId, PluginModRef, PluginModule, PluginType, Plugin_TO,
        Unsupported,
    },
    Value,
};
use log::debug;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

// mod model;
// use model::{Object, ObjectStore};

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
    "sarzak".into()
}

#[sabi_extern_fn]
pub fn id() -> RStr<'static> {
    "sarzak".into()
}

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(args: RVec<FfiValue>) -> RResult<PluginType, AppError> {
    let this = if args.len() == 0 {
        SarzakStore {
            store: ObjectStore::new(),
        }
    } else if args.len() == 1 {
        if let FfiValue::String(path) = &args[0] {
            SarzakStore {
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
struct SarzakStore {
    store: ObjectStore,
}

impl Display for SarzakStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //     f.debug_struct("SarzakStore")
        //         .field("store", &self.store)
        //         .field("plugin_id", &self.plugin_id)
        //         .finish()
        write!(f, "{:?}", self.store)
    }
}

impl Plugin for SarzakStore {
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
                "Object" => match func {
                    "new" => {
                        if args.len() != 3 {
                            return Err(AppError::Uber("Expected 4 arguments".into()));
                        }

                        let mut value_args: Vec<Value> = Vec::new();
                        args.reverse();
                        for arg in args.into_iter() {
                            value_args.push(arg.into());
                        }
                        match (|| -> Result<Object, AppError> {
                            let id = Uuid::new_v4();
                            let obj = Object {
                                description: value_args.pop().unwrap().try_into().map_err(|e| {
                                    AppError::Uber(format!("Error converting value: {}", e).into())
                                })?,
                                id,
                                key_letters: value_args.pop().unwrap().try_into().map_err(|e| {
                                    AppError::Uber(format!("Error converting value: {}", e).into())
                                })?,
                                name: value_args.pop().unwrap().try_into().map_err(|e| {
                                    AppError::Uber(format!("Error converting value: {}", e).into())
                                })?,
                            };

                            Ok(obj)
                        })() {
                            Ok(object) => {
                                let this = ObjectProxy { inner: object };
                                let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                                let proxy = FfiProxy {
                                    uuid: FfiUuid {
                                        inner: "7178e7a4-5131-504b-a7b3-c2c0cfedf343".into(),
                                    },
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
                        for obj in self.store.iter_object() {
                            let this = ObjectProxy { inner: obj.clone() };
                            let plugin = Plugin_TO::from_value(this, TD_CanDowncast);
                            let proxy = FfiProxy {
                                uuid: FfiUuid {
                                    inner: "7178e7a4-5131-504b-a7b3-c2c0cfedf343".into(),
                                },
                                plugin: plugin.clone(),
                            };

                            instances.push(FfiValue::ProxyType(proxy));
                        }
                        Ok(FfiValue::Vector(instances.into()))
                    }
                    _ => Err(AppError::Uber("Invalid function".into())),
                },
                "ObjectStore" => match func {
                    "inter_object" => {
                        if args.len() != 1 {
                            return Err(AppError::Uber("Expected 1 argument".into()));
                        }

                        if let FfiValue::PlugIn(obj) = args.pop().unwrap() {
                            let obj = obj.obj.downcast_into::<ObjectProxy>().unwrap();
                            self.store.inter_object(obj.inner.clone());
                            Ok(FfiValue::Empty)
                        } else {
                            Err(AppError::Uber("Invalid Object".into()))
                        }
                    }
                    _ => Err(AppError::Uber("Invalid function".into())),
                },
                _ => Err(AppError::Uber("Invalid type".into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "sarzak".into()
    }

    fn close(self) {}
}

#[derive(Clone, Debug)]
struct ObjectProxy {
    inner: Object,
}

impl Display for ObjectProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //     f.debug_struct("SarzakStore")
        //         .field("store", &self.store)
        //         .field("plugin_id", &self.plugin_id)
        //         .finish()
        write!(f, "{:?}", self.inner)
    }
}

impl Plugin for ObjectProxy {
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
                                "name" => Ok(FfiValue::String(self.inner.name.clone().into())),
                                "description" => {
                                    Ok(FfiValue::String(self.inner.description.clone().into()))
                                }
                                "key_letters" => {
                                    Ok(FfiValue::String(self.inner.key_letters.clone().into()))
                                }
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

                        if let FfiValue::String(field) = args.pop().unwrap() {
                            let value: Value = args.pop().unwrap().into();
                            let field: &str = field.as_str();
                            match field {
                                "name" => {
                                    self.inner.name = value.try_into().map_err(|e| {
                                        AppError::Uber(
                                            format!("Error converting value: {}", e).into(),
                                        )
                                    })?
                                }
                                "description" => {
                                    self.inner.description = value.try_into().map_err(|e| {
                                        AppError::Uber(
                                            format!("Error converting value: {}", e).into(),
                                        )
                                    })?
                                }

                                "key_letters" => {
                                    self.inner.key_letters = value.try_into().map_err(|e| {
                                        AppError::Uber(
                                            format!("Error converting value: {}", e).into(),
                                        )
                                    })?
                                }
                                _ => {
                                    return Err(AppError::Uber(
                                        format!("Invalid field {field}").into(),
                                    ))
                                }
                            }

                            Ok(FfiValue::Empty)
                        } else {
                            Err(AppError::Uber("Invalid Object".into()))
                        }
                    }
                    _ => Err(AppError::Uber("Invalid function".into())),
                },
                _ => Err(AppError::Uber("Invalid type".into())),
            }
        })()
        .into()
    }

    fn name(&self) -> RStr<'_> {
        "Object".into()
    }

    fn close(self) {}
}
