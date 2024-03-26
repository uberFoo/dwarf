use std::sync::{Arc, Mutex};

use abi_stable::{
    std_types::{RBox, ROption, RResult, RString, RVec},
    StableAbi,
};
use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use uuid::Uuid;

use crate::{
    bubba::value::Value as VmValue,
    chacha::{
        error::{ChaChaError, Result},
        value::{Enum, TupleEnum},
    },
    lu_dog::{ObjectStore as LuDogStore, ValueTypeEnum},
    new_ref,
    plug_in::PluginType,
    s_read, DwarfFloat, DwarfInteger, NewRef, RefType, Value, LAMBDA_FUNCS,
};

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiProxy {
    pub module: RString,
    pub ty: FfiUuid,
    pub id: FfiUuid,
    pub plugin: PluginType,
}

impl std::fmt::Display for FfiProxy {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{{ module: {}, ty: {}, id: {}, plugin: {} }}",
            self.module,
            self.ty,
            self.id,
            self.plugin.name()
        )
    }
}

/// A value that can be passed across FFI boundaries.
///
/// This is a simplified version of the `Value` type, which is used to represent
/// values in the interpreter. Using the `VmValue` type we can send values from
/// the VM as well.
///
#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub enum FfiValue {
    Boolean(bool),
    // Callback(Callback<F>),
    Empty,
    Error(RString),
    Float(DwarfFloat),
    Integer(DwarfInteger),
    Lambda(usize),
    Option(ROption<RBox<Self>>),
    PlugIn(PluginType),
    ProxyType(FfiProxy),
    Range(FfiRange),
    Result(RResult<RBox<Self>, RBox<Self>>),
    String(RString),
    // Table(RHashMap<RString, RefType<Self>>),
    Unknown,
    // UserType(FfiUuid),
    Uuid(FfiUuid),
    Vector(RVec<Self>),
}

impl std::fmt::Display for FfiValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Boolean(bool_) => write!(f, "{bool_}"),
            // Self::Callback(_) => write!(f, "callback"),
            Self::Empty => write!(f, "()"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num}"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::Lambda(n) => write!(f, "lambda {n}"),
            Self::Option(option) => match option {
                ROption::RNone => write!(f, "None"),
                ROption::RSome(value) => write!(f, "Some({value})"),
            },
            Self::PlugIn(plugin) => write!(f, "plugin::{}", plugin.name()),
            Self::ProxyType(proxy) => write!(f, "{proxy}"),
            Self::Range(range) => write!(f, "{range:?}"),
            Self::Result(result) => match result {
                RResult::RErr(err) => write!(f, "Err({err})"),
                RResult::ROk(ok) => write!(f, "Ok({ok})"),
            },
            Self::String(str_) => write!(f, "{str_}"),
            Self::Unknown => write!(f, "<unknown>"),
            // Self::UserType(uuid) => write!(f, "{uuid}"),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
            Self::Vector(vec) => {
                let mut first_time = true;
                write!(f, "[")?;
                for i in vec {
                    if first_time {
                        first_time = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{i}")?;
                }
                write!(f, "]")
            }
        }
    }
}

impl From<String> for FfiValue {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

impl From<Value> for FfiValue {
    fn from(value: Value) -> Self {
        match &value {
            Value::Boolean(bool_) => Self::Boolean(bool_.to_owned()),
            Value::Empty => Self::Empty,
            // Value::Error(e) => Self::Error(e.to_owned().into()),
            Value::Float(num) => Self::Float(num.to_owned()),
            Value::Integer(num) => Self::Integer(num.to_owned()),
            Value::ProxyType {
                module,
                obj_ty,
                id,
                plugin,
            } => Self::ProxyType(FfiProxy {
                module: module.to_owned().into(),
                ty: obj_ty.to_owned().into(),
                id: id.to_owned().into(),
                plugin: s_read!(plugin).clone(),
            }),
            Value::Range(range) => Self::Range(FfiRange {
                start: range.start,
                end: range.end,
            }),
            Value::String(str_) => Self::String(str_.to_owned().into()),
            Value::Uuid(uuid) => Self::Uuid(uuid.to_owned().into()),
            // Value::Vector(vec) => {
            //     Self::Vector(vec.iter().map(|v| s_read!(v).clone().into()).collect())
            // }
            _ => Self::Unknown,
        }
    }
}

impl From<FfiValue> for Value {
    fn from(value: FfiValue) -> Self {
        match value {
            FfiValue::Boolean(bool_) => Self::Boolean(bool_),
            FfiValue::Empty => Self::Empty,
            // FfiValue::Error(e) => Self::Error(e.into()),
            FfiValue::Float(num) => Self::Float(num),
            FfiValue::Integer(num) => Self::Integer(num),
            FfiValue::ProxyType(plugin) => Self::ProxyType {
                module: plugin.module.into(),
                obj_ty: plugin.ty.into(),
                id: plugin.id.into(),
                plugin: new_ref!(PluginType, plugin.plugin),
            },
            FfiValue::Range(range) => Self::Range(range.start..range.end),
            FfiValue::String(str_) => Self::String(str_.into()),
            // FfiValue::UserType(uuid) => Self::UserType(new_ref!(UserType, uuid.into())),
            FfiValue::Uuid(uuid) => Self::Uuid(uuid.into()),
            // FfiValue::Vector(vec) => {
            //     Self::Vector(vec.into_iter().map(|v| new_ref!(Value, v.into())).collect())
            // }
            _ => Self::Unknown,
        }
    }
}

impl From<VmValue> for FfiValue {
    fn from(value: VmValue) -> Self {
        match &value {
            VmValue::Boolean(bool_) => Self::Boolean(bool_.to_owned()),
            VmValue::Empty => Self::Empty,
            VmValue::Float(num) => Self::Float(num.to_owned()),
            lambda @ VmValue::LambdaPointer { .. } => {
                let λ = match LAMBDA_FUNCS.get() {
                    Some(λ) => λ,
                    None => {
                        let λ = Arc::new(Mutex::new(HashMap::default()));
                        let _ = LAMBDA_FUNCS.set(λ);
                        LAMBDA_FUNCS.get().unwrap()
                    }
                };

                let mut λ = λ.lock().unwrap();
                let key = λ.len();
                λ.insert(key, lambda.clone());

                Self::Lambda(key)
            }
            VmValue::Integer(num) => Self::Integer(num.to_owned()),
            // VmValue::ProxyType {
            //     module,
            //     obj_ty,
            //     id,
            //     plugin,
            // } => Self::ProxyType(FfiProxy {
            //     module: module.to_owned().into(),
            //     ty: obj_ty.to_owned().into(),
            //     id: id.to_owned().into(),
            //     plugin: s_read!(plugin).clone(),
            // }),
            VmValue::Range(range) => Self::Range(FfiRange {
                start: range.start,
                end: range.end,
            }),
            VmValue::String(str_) => Self::String(str_.to_owned().into()),
            // Value::Vector(vec) => {
            //     Self::Vector(vec.iter().map(|v| s_read!(v).clone().into()).collect())
            // }
            x => panic!("Unknown FfiValue: {x}"),
        }
    }
}

impl From<FfiValue> for VmValue {
    fn from(value: FfiValue) -> Self {
        match value {
            FfiValue::Boolean(bool_) => Self::Boolean(bool_),
            FfiValue::Empty => Self::Empty,
            FfiValue::Float(num) => Self::Float(num),
            FfiValue::Integer(num) => Self::Integer(num),
            // FfiValue::ProxyType(plugin) => Self::ProxyType {
            //     module: plugin.module.into(),
            //     obj_ty: plugin.ty.into(),
            //     id: plugin.id.into(),
            //     plugin: new_ref!(PluginType, plugin.plugin),
            // },
            FfiValue::Range(range) => Self::Range(range.start..range.end),
            FfiValue::String(str_) => Self::String(str_.into()),
            // FfiValue::Vector(vec) => {
            //     Self::Vector(vec.into_iter().map(|v| new_ref!(Value, v.into())).collect())
            // }
            x => panic!("Unknown FfiValue: {x}"),
        }
    }
}

impl From<(FfiValue, &LuDogStore)> for Value {
    fn from(value: (FfiValue, &LuDogStore)) -> Self {
        let lu_dog = value.1;
        match value.0 {
            FfiValue::Boolean(bool_) => Self::Boolean(bool_),
            FfiValue::Empty => Self::Empty,
            // FfiValue::Error(e) => Self::Error(e.into()),
            FfiValue::Float(num) => Self::Float(num),
            FfiValue::Integer(num) => Self::Integer(num),
            FfiValue::Option(option) => match option {
                ROption::RNone => Self::Empty,
                ROption::RSome(value) => <(FfiValue, &LuDogStore) as Into<Value>>::into((
                    RBox::into_inner(value),
                    lu_dog,
                )),
            },
            FfiValue::ProxyType(plugin) => Self::ProxyType {
                module: plugin.module.into(),
                obj_ty: plugin.ty.into(),
                id: plugin.id.into(),
                plugin: new_ref!(PluginType, plugin.plugin),
            },
            FfiValue::Range(range) => Self::Range(range.start..range.end),
            FfiValue::Result(result) => {
                let Some(ty) = lu_dog.exhume_enumeration_id_by_name("::std::result::Result") else {
                    panic!("Result type not found")
                };
                let ty = lu_dog.exhume_enumeration(&ty).unwrap();
                let Some(ty) = lu_dog.iter_value_type().find(|vt| {
                    if let ValueTypeEnum::Enumeration(id) = s_read!(vt).subtype {
                        let id = lu_dog.exhume_enumeration(&id).unwrap();
                        if s_read!(id).id == s_read!(ty).id {
                            return true;
                        }
                    }
                    false
                }) else {
                    unreachable!()
                };

                let tuple = match result {
                    RResult::RErr(err) => TupleEnum {
                        variant: "Err".to_owned(),
                        value: new_ref!(
                            Value,
                            <(FfiValue, &LuDogStore) as Into<Value>>::into((
                                RBox::into_inner(err),
                                lu_dog,
                            ))
                        ),
                    },
                    RResult::ROk(ok) => TupleEnum {
                        variant: "Ok".to_owned(),
                        value: new_ref!(
                            Value,
                            <(FfiValue, &LuDogStore) as Into<Value>>::into((
                                RBox::into_inner(ok),
                                lu_dog,
                            ))
                        ),
                    },
                };

                Value::Enumeration(Enum::Tuple(
                    (ty.clone(), "Result".to_owned()),
                    new_ref!(TupleEnum<Value>, tuple),
                ))
            }
            FfiValue::String(str_) => Self::String(str_.into()),
            // FfiValue::UserType(uuid) => Self::UserType(new_ref!(UserType, uuid.into())),
            FfiValue::Uuid(uuid) => Self::Uuid(uuid.into()),
            // FfiValue::Vector(vec) => {
            //     Self::Vector(vec.into_iter().map(|v| new_ref!(Value, v.into())).collect())
            // }
            _ => Self::Unknown,
        }
    }
}

impl TryFrom<FfiValue> for String {
    type Error = ChaChaError;

    fn try_from(value: FfiValue) -> Result<Self> {
        match value {
            FfiValue::String(s) => Ok(s.into()),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "String".to_owned(),
            }),
        }
    }
}

impl TryFrom<&FfiValue> for String {
    type Error = ChaChaError;

    fn try_from(value: &FfiValue) -> Result<Self> {
        match value {
            FfiValue::String(s) => Ok(s.to_owned().into()),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "String".to_owned(),
            }),
        }
    }
}

// impl TryFrom<&FfiValue> for Callback<F>
// where
//     F: Fn(FfiValue) -> FfiValue + 'static,
// {
//     type Error = ChaChaError;

//     fn try_from(value: &FfiValue) -> Result<Self> {
//         match value {
//             FfiValue::Callback(c) => Ok(c.to_owned().into()),
//             _ => Err(ChaChaError::Conversion {
//                 src: value.to_string(),
//                 dst: "String".to_owned(),
//             }),
//         }
//     }
// }

impl TryFrom<&FfiValue> for i64 {
    type Error = ChaChaError;

    fn try_from(value: &FfiValue) -> Result<Self> {
        match value {
            FfiValue::Integer(i) => Ok(*i),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "i64".to_owned(),
            }),
        }
    }
}

// #[repr(C)]
// #[derive(Clone, Debug, StableAbi)]
// pub struct Callback<F>
// where
//     F: Fn(FfiValue<F>) -> FfiValue<F> + 'static,
// {
//     callback: RBox<F>,
// }

// impl<F> Callback<F>
// where
//     F: Fn(FfiValue<F>) -> FfiValue<F> + 'static,
// {
//     pub fn new(callback: F) -> Self {
//         let foo = Box::new(callback);
//         let callback = RBox::from_box(foo);
//         Self { callback }
//     }

//     #[sabi_extern_fn]
//     pub fn call(&self, i: FfiValue<F>) -> FfiValue<F> {
//         (self.callback)(i)
//     }
// }

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiRange {
    pub(crate) start: DwarfInteger,
    pub(crate) end: DwarfInteger,
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiUuid {
    pub inner: RString,
}

impl std::fmt::Display for FfiUuid {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{inner}", inner = self.inner)
    }
}

impl From<Uuid> for FfiUuid {
    fn from(uuid: Uuid) -> Self {
        Self {
            inner: uuid.to_string().into(),
        }
    }
}

impl From<FfiUuid> for Uuid {
    fn from(uuid: FfiUuid) -> Self {
        Uuid::parse_str(&uuid.inner).unwrap()
    }
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub enum FfiOption<T> {
    None,
    Some(T),
}

impl<T> From<Option<T>> for FfiOption<T>
where
    T: Clone,
{
    fn from(option: Option<T>) -> Self {
        match option {
            None => Self::None,
            Some(t) => Self::Some(t),
        }
    }
}

impl<T> From<ROption<T>> for FfiOption<T> {
    fn from(option: ROption<T>) -> Self {
        match option {
            ROption::RNone => Self::None,
            ROption::RSome(t) => Self::Some(t),
        }
    }
}

impl<T> From<ROption<RBox<T>>> for FfiOption<T>
where
    T: Clone,
{
    fn from(option: ROption<RBox<T>>) -> Self {
        match option {
            ROption::RNone => Self::None,
            ROption::RSome(t) => Self::Some((*t).clone()),
        }
    }
}
