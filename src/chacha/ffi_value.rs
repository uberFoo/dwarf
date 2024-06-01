use std::{
    collections::HashMap as StdHashMap,
    fmt,
    sync::{Arc, Mutex},
};

use abi_stable::{
    std_types::{RBox, RHashMap, ROption, RResult, RString, RVec, Tuple2},
    StableAbi,
};
use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, Backtrace, Location};
use uuid::Uuid;

use crate::{
    bubba::{
        error::{BubbaError, Error, Result},
        value::Value,
    },
    chacha::{
        value::_struct::StructAttributes,
        value::{Enum, Struct},
    },
    keywords::{ERR, OK, RESULT_TYPE},
    lu_dog::{ValueType, ValueTypeEnum},
    new_ref,
    plug_in::PluginType,
    s_read, DwarfFloat, DwarfInteger, NewRef, RefType, LAMBDA_FUNCS, PATH_SEP,
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
/// values in the interpreter. Using the `Value` type we can send values from
/// the VM as well.
///
#[repr(C)]
#[derive(Clone, Debug, Default, StableAbi)]
pub enum FfiValue {
    /// Boolean
    ///
    /// A boolean value.
    Boolean(bool),
    /// Empty
    ///
    /// This is equivalent to `()`.
    #[default]
    Empty,
    /// Error
    ///
    /// An error message.
    Error(RString),
    /// Float
    ///
    /// A floating point number.
    Float(DwarfFloat),
    /// Integer
    ///
    /// An integer number.
    Integer(DwarfInteger),
    /// Lambda
    ///
    /// A handle to a lambda function. This is used to invoke a lambda function
    /// from dwarf in the plugin.
    Lambda(usize),
    /// List
    ///
    /// A list of values; aka a Vec.
    List(RVec<Self>),
    Map(FfiHashMap),
    /// Option
    ///
    /// An optional value. Note that this is not the same as `Option<T>`, but
    /// uses the FFI-safe `ROption` type.
    Option(ROption<RBox<Self>>),
    /// PlugIn
    ///
    /// A plugin type.
    PlugIn(PluginType),
    ProxyType(FfiProxy),
    /// Range
    ///
    /// A range of integers, with a start and an end.
    Range(FfiRange),
    /// Result
    ///
    /// A result type. Note that this is not the same as `Result<T, E>`, but
    /// uses the FFI-safe `RResult` type.
    Result(RResult<RBox<Self>, RBox<Self>>),
    /// String
    ///
    /// A string. Note that this is not the same as `String`, but uses the
    /// FFI-safe `RString` type.
    String(RString),
    /// Struct
    ///
    /// This is a user defined struct.
    Struct(FfiStruct),
    /// A value of unknown type.
    ///
    /// This is useful for dumping value that maybe we just don't want to deal
    /// with, or that we don't know how to deal with. It's probably a terrible
    /// crutch that will come back and bite me.
    Unknown,
    /// UUID type
    ///
    /// Version 4 UUID.
    Uuid(FfiUuid),
}

impl Unpin for FfiValue {}

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
            Self::List(vec) => {
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
            Self::Map(map) => {
                let mut first_time = true;
                write!(f, "{{")?;
                for Tuple2(k, v) in &map.0 {
                    if first_time {
                        first_time = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{k}: {v}")?;
                }
                write!(f, "}}")
            }
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
            Self::Struct(s) => write!(f, "{s}"),
            Self::Unknown => write!(f, "<unknown>"),
            // Self::UserType(uuid) => write!(f, "{uuid}"),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
        }
    }
}

impl From<String> for FfiValue {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

impl<T> From<HashMap<String, T>> for FfiValue
where
    T: Into<FfiValue>,
{
    fn from(value: HashMap<String, T>) -> Self {
        let map = value
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect::<RHashMap<RString, FfiValue>>();
        Self::Map(FfiHashMap(map))
    }
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiHashMap(pub(crate) RHashMap<RString, FfiValue>);

impl<T> From<HashMap<String, T>> for FfiHashMap
where
    T: Into<FfiValue>,
{
    fn from(value: HashMap<String, T>) -> Self {
        let map = value
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect::<RHashMap<RString, FfiValue>>();
        FfiHashMap(map)
    }
}

impl fmt::Display for FfiHashMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut attrs = self.0.iter().collect::<Vec<_>>();
        attrs.sort_by(|Tuple2(k1, _), Tuple2(k2, _)| k1.cmp(k2));

        let mut out = f.debug_map();
        for Tuple2(k, v) in attrs {
            out.entry(k, &format_args!("{v}"));
        }

        out.finish()
    }
}

impl From<Value> for FfiValue {
    fn from(value: Value) -> Self {
        match &value {
            Value::Boolean(bool_) => Self::Boolean(bool_.to_owned()),
            Value::Enumeration(e) => match e {
                Enum::Struct(s) => {
                    dbg!(s);
                    panic!()
                }
                Enum::Tuple((ty, ty_name), t) => {
                    if ty_name == RESULT_TYPE {
                        let t = s_read!(t);
                        match t.variant.as_str() {
                            ERR => Self::Result(RResult::RErr(RBox::new(
                                s_read!(t.value).clone().into(),
                            ))),
                            OK => Self::Result(RResult::ROk(RBox::new(
                                s_read!(t.value).clone().into(),
                            ))),
                            _ => panic!(),
                        }
                    } else {
                        dbg!(ty, ty_name, t);
                        panic!()
                    }
                }
                Enum::Unit(_ty, ty_name, v) => {
                    let ty_name = ty_name.to_lowercase();
                    Self::String(format!("{ty_name}::{v}").into())
                }
            },
            Value::Empty => Self::Empty,
            Value::Float(num) => Self::Float(num.to_owned()),
            lambda @ Value::LambdaPointer { .. } => {
                let λ = match LAMBDA_FUNCS.get() {
                    Some(λ) => λ,
                    None => {
                        let λ = Arc::new(Mutex::new(HashMap::default()));
                        let _ = LAMBDA_FUNCS.set(λ);
                        LAMBDA_FUNCS.get().unwrap()
                    }
                };

                let lambda_clone = lambda.clone();
                let key = {
                    let mut λ = λ.lock().unwrap();
                    let key = λ.len();
                    λ.insert(key, lambda_clone);
                    key
                };

                Self::Lambda(key)
            }
            Value::List { ty: _, inner } => {
                let inner = s_read!(inner);
                let inner = inner.iter().map(|v| s_read!(v).clone().into()).collect();
                Self::List(inner)
            }
            Value::Integer(num) => Self::Integer(num.to_owned()),
            Value::Plugin((_name, plugin)) => Self::PlugIn(s_read!(plugin).clone()),
            Value::Range(range) => Self::Range(FfiRange {
                start: range.start,
                end: range.end,
            }),
            Value::String(str_) => Self::String(str_.to_owned().into()),
            Value::Struct(s) => Self::Struct(s.into()),
            x => panic!("Unknown FfiValue: {x}"),
        }
    }
}

impl From<FfiValue> for Value {
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
            FfiValue::Map(map) => {
                let inner = map
                    .0
                    .into_iter()
                    .map(|Tuple2(k, v)| (k.into(), new_ref!(Value, v.into())))
                    .collect::<StdHashMap<String, RefType<Value>>>();
                Self::Map {
                    inner: new_ref!(StdHashMap<String, RefType<Value>>, inner),
                }
            }
            FfiValue::Range(range) => Self::Range(range.start..range.end),
            FfiValue::String(str_) => Self::String(str_.into()),
            FfiValue::Struct(s) => Self::Struct(s.into()),
            x => panic!("Unknown FfiValue: {x}"),
        }
    }
}

impl<V> TryFrom<&FfiValue> for StdHashMap<String, V>
where
    V: TryFrom<FfiValue, Error = BubbaError>,
{
    type Error = BubbaError;

    fn try_from(value: &FfiValue) -> Result<Self, <Self as TryFrom<&FfiValue>>::Error> {
        match value {
            FfiValue::Map(map) => {
                let result: Result<StdHashMap<_, _>, _> = map
                    .0
                    .iter()
                    .map(|Tuple2(k, v)| Ok((k.to_string(), v.to_owned().try_into()?)))
                    .collect();
                result.map_err(|_: BubbaError| BubbaError::Conversion {
                    src: map.to_string(),
                    dst: "HashMap<K, V>".to_owned(),
                    location: location!(),
                    backtrace: Backtrace::capture(),
                })
            }
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "HashMap<K, V>".to_owned(),
                location: location!(),
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

impl<T: TryFrom<FfiValue, Error = BubbaError> + std::fmt::Debug> TryFrom<&FfiValue> for Vec<T> {
    type Error = BubbaError;

    fn try_from(value: &FfiValue) -> Result<Self, Self::Error> {
        match value.clone() {
            FfiValue::List(vec) => {
                let result: Result<Vec<_>, _> = vec.into_iter().map(|v| v.try_into()).collect();
                result.map_err(|_| BubbaError::Conversion {
                    src: value.to_string(),
                    dst: "Vec<T>".to_owned(),
                    location: location!(),
                    backtrace: Backtrace::capture(),
                })
            }
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "Vec<T>".to_owned(),
                location: location!(),
                backtrace: Backtrace::capture(),
            }),
        }
    }
}

impl<T: Into<FfiValue>> From<Vec<T>> for FfiValue {
    fn from(value: Vec<T>) -> Self {
        let vec = value
            .into_iter()
            .map(|v| v.into())
            .collect::<RVec<FfiValue>>();
        FfiValue::List(vec)
    }
}

impl From<()> for FfiValue {
    fn from(_: ()) -> Self {
        FfiValue::Empty
    }
}

impl From<u64> for FfiValue {
    fn from(value: u64) -> Self {
        FfiValue::Integer(value as DwarfInteger)
    }
}

impl TryFrom<FfiValue> for String {
    type Error = BubbaError;

    fn try_from(value: FfiValue) -> Result<Self, Self::Error> {
        match value {
            FfiValue::Boolean(b) => Ok(b.to_string().into()),
            FfiValue::Empty => Ok("()".to_owned()),
            FfiValue::Error(e) => Ok(e.into()),
            FfiValue::Float(f) => Ok(f.to_string().into()),
            FfiValue::Integer(i) => Ok(i.to_string().into()),
            FfiValue::Lambda(l) => Ok(l.to_string().into()),
            FfiValue::Range(r) => Ok(format!("{}..{}", r.start, r.end).into()),
            FfiValue::String(s) => Ok(s.into()),
            FfiValue::Struct(s) => Ok(s.to_string().into()),
            FfiValue::Unknown => Ok("<unknown>".to_owned()),
            FfiValue::Uuid(u) => Ok(u.to_string().into()),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "String".to_owned(),
                location: location!(),
                backtrace: Backtrace::capture(),
            }
            .into()),
        }
    }
}

impl TryFrom<&FfiValue> for String {
    type Error = BubbaError;

    fn try_from(value: &FfiValue) -> Result<Self, Self::Error> {
        match value {
            FfiValue::Boolean(b) => Ok(b.to_string().into()),
            FfiValue::Empty => Ok("()".to_owned()),
            FfiValue::Error(e) => Ok(e.to_owned().into()),
            FfiValue::Float(f) => Ok(f.to_string().into()),
            FfiValue::Integer(i) => Ok(i.to_string().into()),
            FfiValue::Lambda(l) => Ok(l.to_string().into()),
            FfiValue::Range(r) => Ok(format!("{}..{}", r.start, r.end).into()),
            FfiValue::String(s) => Ok(s.to_owned().into()),
            FfiValue::Struct(s) => Ok(s.to_string().into()),
            FfiValue::Unknown => Ok("<unknown>".to_owned()),
            FfiValue::Uuid(u) => Ok(u.to_string().into()),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "String".to_owned(),
                location: location!(),
                backtrace: Backtrace::capture(),
            }
            .into()),
        }
    }
}

impl TryFrom<&FfiValue> for i64 {
    type Error = Error;

    fn try_from(value: &FfiValue) -> Result<Self> {
        match value {
            FfiValue::Integer(i) => Ok(*i),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "i64".to_owned(),
                location: location!(),
                backtrace: Backtrace::capture(),
            }
            .into()),
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiStruct {
    pub(crate) type_name: RString,
    pub(crate) type_: FfiValueType,
    pub(crate) attrs: FfiStructAttributes,
}

impl FfiStruct {
    pub fn get_attr(&self, key: &str) -> Option<&FfiValue> {
        self.attrs.0.get(key)
    }
}

impl fmt::Display for FfiStruct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut attrs = self.attrs.0.iter().collect::<Vec<_>>();
        attrs.sort_by(|Tuple2(k1, _), Tuple2(k2, _)| k1.cmp(k2));

        let name = if let Some(name) = self.type_name.strip_prefix(PATH_SEP) {
            name
        } else {
            &self.type_name
        };

        let mut out = f.debug_struct(name);
        for Tuple2(k, v) in attrs {
            out.field(k, &format_args!("{v}"));
        }

        out.finish()
    }
}

impl<T> From<&Struct<T>> for FfiStruct
where
    T: Clone
        + std::fmt::Debug
        + PartialEq
        + std::fmt::Display
        + std::default::Default
        + Into<FfiValue>,
{
    fn from(value: &Struct<T>) -> Self {
        let ty = value.get_type();
        let ty = &*s_read!(ty);
        Self {
            type_name: value.type_name().into(),
            type_: ty.into(),
            attrs: value.attrs().clone().into(),
        }
    }
}

impl From<FfiStruct> for Struct<Value> {
    fn from(value: FfiStruct) -> Self {
        let attrs = value.attrs.clone();
        let attrs = attrs
            .0
            .into_iter()
            .map(|Tuple2(k, v)| (k.into(), v.into()))
            .collect();

        Struct {
            type_name: value.type_name.into(),
            type_: value.type_.into(),
            attrs: StructAttributes(attrs),
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiStructAttributes(RHashMap<RString, FfiValue>);

impl<T> From<StructAttributes<T>> for FfiStructAttributes
where
    T: Clone
        + std::fmt::Debug
        + PartialEq
        + std::fmt::Display
        + std::default::Default
        + Into<FfiValue>,
{
    fn from(value: StructAttributes<T>) -> Self {
        let attrs = value
            .inner()
            .into_iter()
            .map(|(k, v)| (k.clone().into(), (*v).clone().into()))
            .collect();
        Self(attrs)
    }
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiValueType {
    pub subtype: FfiValueTypeEnum,
    pub bogus: bool,
    pub id: usize,
}

impl From<&ValueType> for FfiValueType {
    fn from(value: &ValueType) -> Self {
        Self {
            subtype: value.subtype.clone().into(),
            bogus: value.bogus,
            id: value.id,
        }
    }
}

impl From<FfiValueType> for ValueType {
    fn from(value: FfiValueType) -> Self {
        Self {
            subtype: value.subtype.into(),
            bogus: value.bogus,
            id: value.id,
        }
    }
}

impl From<FfiValueType> for RefType<ValueType> {
    fn from(value: FfiValueType) -> Self {
        new_ref!(ValueType, value.into())
    }
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub enum FfiValueTypeEnum {
    AnyList(FfiUuid),
    Char(FfiUuid),
    Empty(FfiUuid),
    EnumGeneric(usize),
    Enumeration(usize),
    FuncGeneric(usize),
    Function(usize),
    XFuture(usize),
    Import(usize),
    Lambda(usize),
    List(usize),
    Map(usize),
    ZObjectStore(usize),
    XPlugin(usize),
    Range(FfiUuid),
    WoogStruct(usize),
    StructGeneric(usize),
    Task(FfiUuid),
    Ty(FfiUuid),
    Unknown(FfiUuid),
}

impl From<ValueTypeEnum> for FfiValueTypeEnum {
    fn from(value: ValueTypeEnum) -> Self {
        match value {
            ValueTypeEnum::AnyList(uuid) => Self::AnyList(uuid.into()),
            ValueTypeEnum::Char(uuid) => Self::Char(uuid.into()),
            ValueTypeEnum::Empty(uuid) => Self::Empty(uuid.into()),
            ValueTypeEnum::EnumGeneric(id) => Self::EnumGeneric(id),
            ValueTypeEnum::Enumeration(id) => Self::Enumeration(id),
            ValueTypeEnum::FuncGeneric(id) => Self::FuncGeneric(id),
            ValueTypeEnum::Function(id) => Self::Function(id),
            ValueTypeEnum::XFuture(id) => Self::XFuture(id),
            ValueTypeEnum::Import(id) => Self::Import(id),
            ValueTypeEnum::Lambda(id) => Self::Lambda(id),
            ValueTypeEnum::List(id) => Self::List(id),
            ValueTypeEnum::Map(id) => Self::Map(id),
            ValueTypeEnum::ZObjectStore(id) => Self::ZObjectStore(id),
            ValueTypeEnum::XPlugin(id) => Self::XPlugin(id),
            ValueTypeEnum::Range(uuid) => Self::Range(uuid.into()),
            ValueTypeEnum::WoogStruct(id) => Self::WoogStruct(id),
            ValueTypeEnum::StructGeneric(id) => Self::StructGeneric(id),
            ValueTypeEnum::Task(uuid) => Self::Task(uuid.into()),
            ValueTypeEnum::Ty(uuid) => Self::Ty(uuid.into()),
            ValueTypeEnum::Unknown(uuid) => Self::Unknown(uuid.into()),
        }
    }
}

impl From<FfiValueTypeEnum> for ValueTypeEnum {
    fn from(value: FfiValueTypeEnum) -> Self {
        match value {
            FfiValueTypeEnum::AnyList(uuid) => ValueTypeEnum::AnyList(uuid.into()),
            FfiValueTypeEnum::Char(uuid) => ValueTypeEnum::Char(uuid.into()),
            FfiValueTypeEnum::Empty(uuid) => ValueTypeEnum::Empty(uuid.into()),
            FfiValueTypeEnum::EnumGeneric(id) => ValueTypeEnum::EnumGeneric(id),
            FfiValueTypeEnum::Enumeration(id) => ValueTypeEnum::Enumeration(id),
            FfiValueTypeEnum::FuncGeneric(id) => ValueTypeEnum::FuncGeneric(id),
            FfiValueTypeEnum::Function(id) => ValueTypeEnum::Function(id),
            FfiValueTypeEnum::XFuture(id) => ValueTypeEnum::XFuture(id),
            FfiValueTypeEnum::Import(id) => ValueTypeEnum::Import(id),
            FfiValueTypeEnum::Lambda(id) => ValueTypeEnum::Lambda(id),
            FfiValueTypeEnum::List(id) => ValueTypeEnum::List(id),
            FfiValueTypeEnum::Map(id) => ValueTypeEnum::Map(id),
            FfiValueTypeEnum::ZObjectStore(id) => ValueTypeEnum::ZObjectStore(id),
            FfiValueTypeEnum::XPlugin(id) => ValueTypeEnum::XPlugin(id),
            FfiValueTypeEnum::Range(uuid) => ValueTypeEnum::Range(uuid.into()),
            FfiValueTypeEnum::WoogStruct(id) => ValueTypeEnum::WoogStruct(id),
            FfiValueTypeEnum::StructGeneric(id) => ValueTypeEnum::StructGeneric(id),
            FfiValueTypeEnum::Task(uuid) => ValueTypeEnum::Task(uuid.into()),
            FfiValueTypeEnum::Ty(uuid) => ValueTypeEnum::Ty(uuid.into()),
            FfiValueTypeEnum::Unknown(uuid) => ValueTypeEnum::Unknown(uuid.into()),
        }
    }
}

impl From<FfiValueTypeEnum> for RefType<ValueTypeEnum> {
    fn from(value: FfiValueTypeEnum) -> Self {
        match value {
            FfiValueTypeEnum::AnyList(uuid) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::AnyList(uuid.into()))
            }
            FfiValueTypeEnum::Char(uuid) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Char(uuid.into()))
            }
            FfiValueTypeEnum::Empty(uuid) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Empty(uuid.into()))
            }
            FfiValueTypeEnum::EnumGeneric(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::EnumGeneric(id))
            }
            FfiValueTypeEnum::Enumeration(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Enumeration(id))
            }
            FfiValueTypeEnum::FuncGeneric(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::FuncGeneric(id))
            }
            FfiValueTypeEnum::Function(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Function(id))
            }
            FfiValueTypeEnum::XFuture(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::XFuture(id))
            }
            FfiValueTypeEnum::Import(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Import(id))
            }
            FfiValueTypeEnum::Lambda(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Lambda(id))
            }
            FfiValueTypeEnum::List(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::List(id))
            }
            FfiValueTypeEnum::Map(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Map(id))
            }
            FfiValueTypeEnum::ZObjectStore(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::ZObjectStore(id))
            }
            FfiValueTypeEnum::XPlugin(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::XPlugin(id))
            }
            FfiValueTypeEnum::Range(uuid) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Range(uuid.into()))
            }
            FfiValueTypeEnum::WoogStruct(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::WoogStruct(id))
            }
            FfiValueTypeEnum::StructGeneric(id) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::StructGeneric(id))
            }
            FfiValueTypeEnum::Task(uuid) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Task(uuid.into()))
            }
            FfiValueTypeEnum::Ty(uuid) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Ty(uuid.into()))
            }
            FfiValueTypeEnum::Unknown(uuid) => {
                new_ref!(ValueTypeEnum, ValueTypeEnum::Unknown(uuid.into()))
            }
        }
    }
}

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
