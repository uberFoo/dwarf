use std::{fmt, ops::Range};

use abi_stable::{
    std_types::{RString, RVec},
    StableAbi,
};
use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use sarzak::lu_dog::ValueTypeEnum;
use uuid::Uuid;

use crate::{
    chacha::error::Result,
    lu_dog::{Function, Lambda, ObjectStore as LuDogStore, ValueType},
    new_ref,
    plug_in::PluginType,
    s_read,
    sarzak::{ObjectStore as SarzakStore, Ty},
    ChaChaError, DwarfFloat, DwarfInteger, NewRef, RefType,
};

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiRange {
    start: DwarfInteger,
    end: DwarfInteger,
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiUuid {
    pub inner: RString,
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
pub struct FfiProxy {
    pub module: RString,
    pub uuid: FfiUuid,
    pub plugin: PluginType,
}

/// This is an actual Value
///
/// This is the type used by the interpreter to represent values.
#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub enum FfiValue {
    Boolean(bool),
    Empty,
    Error(RString),
    Float(DwarfFloat),
    Integer(DwarfInteger),
    PlugIn(PluginType),
    ProxyType(FfiProxy),
    Range(FfiRange),
    String(RString),
    // Table(RHashMap<RString, RefType<Self>>),
    Unknown,
    UserType(FfiUuid),
    Uuid(FfiUuid),
    Vector(RVec<Self>),
}

/// The type of Enumeration Field
///
/// There are three types of enumeration fields: Plain, Struct, and Tuple.
///
/// The field descriptions refer to the following enumeration, `Foo`.
///
/// ```ignore
/// enum Foo {
///     Foo,
///     Bar(int),
///     Baz { qux: string },
/// }
/// ```
///
#[derive(Clone, Debug)]
pub enum EnumFieldVariant {
    /// Plain Enumeration Field
    ///
    /// This sort of enumeration is the simplest. In `Foo`, this refers to the
    /// field `Foo`: `Foo::Foo`. The final field in the path is stored as a
    /// string.
    Plain(String),
    /// Struct Enumeration Field
    ///
    /// This type of field is for when it contains a struct, as `Baz` does above.
    /// That is to say, `Foo::Baz { qux: string }`. We store the final path element
    /// as a string, and the struct as a `RefType<UserStruct>`.
    Struct(String, RefType<UserStruct>),
    /// Tuple Enumeration Field
    ///
    /// This type of field is for when it contains a tuple, as `Bar` does above.
    /// That is to say, `Foo::Bar(int)`. We store the final path element as a
    /// string, and the tuple as a `RefType<Value>`.
    Tuple(String, RefType<Value>),
}

impl PartialEq for EnumFieldVariant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Plain(a), Self::Plain(b)) => a == b,
            (Self::Struct(a, c), Self::Struct(b, d)) => a == b && *s_read!(c) == *s_read!(d),
            (Self::Tuple(a, c), Self::Tuple(b, d)) => a == b && *s_read!(c) == *s_read!(d),
            _ => false,
        }
    }
}

impl Eq for EnumFieldVariant {}

impl fmt::Display for EnumFieldVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Plain(s) => write!(f, "{s}"),
            Self::Struct(name, s) => write!(f, "{name}{}", s_read!(s)),
            Self::Tuple(name, t) => write!(f, "{name}({})", s_read!(t)),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Char(char),
    Empty,
    Enum(RefType<UserEnum>),
    EnumVariant(EnumFieldVariant),
    Error(String),
    Float(DwarfFloat),
    /// Function
    ///
    /// 🚧 I really need to write something here describing, once and for all,
    /// why I need the inner Function to be behind a RefType<<T>>. It seems
    /// excessive, and yet I know I've looked into it before.
    Function(RefType<Function>),
    // Future(RefType<dyn std::future::Future<Output = RefType<Value>>>),
    Integer(DwarfInteger),
    Lambda(RefType<Lambda>),
    Option(Option<RefType<Self>>),
    PlugIn(RefType<PluginType>),
    ProxyType((String, Uuid, PluginType)),
    Range(Range<DwarfInteger>),
    String(String),
    Struct(RefType<UserStruct>),
    Table(HashMap<String, RefType<Self>>),
    Thonk(&'static str, usize),
    Unknown,
    Uuid(uuid::Uuid),
    Vector(Vec<RefType<Self>>),
}

impl From<Value> for FfiValue {
    fn from(value: Value) -> Self {
        match value {
            Value::Boolean(bool_) => Self::Boolean(bool_),
            Value::Empty => Self::Empty,
            Value::Error(e) => Self::Error(e.into()),
            Value::Float(num) => Self::Float(num),
            Value::Integer(num) => Self::Integer(num),
            // Value::PlugIn(plugin) => Self::PlugIn(*plugin),
            Value::ProxyType((module, uuid, plugin)) => Self::ProxyType(FfiProxy {
                module: module.into(),
                uuid: uuid.into(),
                plugin,
            }),
            Value::Range(range) => Self::Range(FfiRange {
                start: range.start,
                end: range.end,
            }),
            Value::String(str_) => Self::String(str_.into()),
            Value::Uuid(uuid) => Self::Uuid(uuid.into()),
            Value::Vector(vec) => {
                Self::Vector(vec.into_iter().map(|v| s_read!(v).clone().into()).collect())
            }
            _ => Self::Unknown,
        }
    }
}

impl From<FfiValue> for Value {
    fn from(value: FfiValue) -> Self {
        match value {
            FfiValue::Boolean(bool_) => Self::Boolean(bool_),
            FfiValue::Empty => Self::Empty,
            FfiValue::Error(e) => Self::Error(e.into()),
            FfiValue::Float(num) => Self::Float(num),
            FfiValue::Integer(num) => Self::Integer(num),
            // FfiValue::PlugIn(store) => Self::PlugIn(store),
            FfiValue::ProxyType(plugin) => {
                Self::ProxyType((plugin.module.into(), plugin.uuid.into(), plugin.plugin))
            }
            FfiValue::Range(range) => Self::Range(range.start..range.end),
            FfiValue::String(str_) => Self::String(str_.into()),
            // FfiValue::UserType(uuid) => Self::UserType(new_ref!(UserType, uuid.into())),
            FfiValue::Uuid(uuid) => Self::Uuid(uuid.into()),
            FfiValue::Vector(vec) => {
                Self::Vector(vec.into_iter().map(|v| new_ref!(Value, v.into())).collect())
            }
            _ => Self::Unknown,
        }
    }
}

impl Value {
    pub fn get_type(&self, sarzak: &SarzakStore, lu_dog: &LuDogStore) -> RefType<ValueType> {
        match &self {
            Value::Boolean(_) => {
                let ty = Ty::new_boolean(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.borrow().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Char(_) => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Char(_) = s_read!(vt).subtype {
                        return vt.clone();
                    }
                }
                unreachable!()
            }
            Value::Empty => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Empty(_) = s_read!(vt).subtype {
                        return vt.clone();
                    }
                }
                unreachable!()
            }
            Value::Enum(ref ut) => s_read!(ut).get_type().clone(),
            Value::Function(ref func) => {
                let func = lu_dog.exhume_function(&s_read!(func).id).unwrap();
                let z = s_read!(func).r1_value_type(lu_dog)[0].clone();
                #[allow(clippy::let_and_return)]
                z
            }
            Value::Float(_) => {
                let ty = Ty::new_float(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.borrow().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Integer(_) => {
                let ty = Ty::new_integer(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.borrow().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Lambda(ref ƛ) => {
                let ƛ = lu_dog.exhume_lambda(&s_read!(ƛ).id).unwrap();
                let ƛ_type = s_read!(ƛ).r1_value_type(lu_dog)[0].clone();
                #[allow(clippy::let_and_return)]
                ƛ_type
            }
            Value::PlugIn(store) => {
                let store = lu_dog
                    .iter_z_object_store()
                    .find(|s| {
                        let s = s_read!(s);
                        s.domain == s_read!(store).name()
                    })
                    .unwrap();
                let ty = s_read!(store).r1_value_type(lu_dog)[0].clone();
                ty
            }
            Value::ProxyType((_module, uuid, _plugin)) => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::WoogStruct(woog) = s_read!(vt).subtype {
                        let woog = lu_dog.exhume_woog_struct(&woog).unwrap();
                        let object = s_read!(woog).object;
                        if let Some(ref obj_id) = object {
                            if uuid == obj_id {
                                return vt.clone();
                            }
                        }
                    }
                }
                unreachable!()
            }
            Value::String(_) => {
                let ty = Ty::new_s_string(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.borrow().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Struct(ref ut) => s_read!(ut).get_type().clone(),
            Value::Uuid(_) => {
                let ty = Ty::new_s_uuid(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.borrow().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Vector(_) => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::List(_) = s_read!(vt).subtype {
                        return vt.clone();
                    }
                }
                unreachable!()
            }
            value => {
                log::error!("Value::get_type() not implemented for {:?}", value);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Unknown(_) = s_read!(vt).subtype {
                        return vt.clone();
                    }
                }
                unreachable!()
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(bool_) => write!(f, "{bool_}"),
            Self::Char(char_) => write!(f, "{char_}"),
            Self::Empty => write!(f, "()"),
            Self::Enum(ty) => write!(f, "{}", s_read!(ty)),
            Self::EnumVariant(var) => write!(f, "{var}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num}"),
            Self::Function(_) => write!(f, "<function>"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::Lambda(_) => write!(f, "<lambda>"),
            Self::PlugIn(store) => write!(f, "Plug-in ({})", s_read!(store).name()),
            Self::Option(option) => match option {
                Some(value) => write!(f, "Some({})", s_read!(value)),
                None => write!(f, "None"),
            },
            // 🚧 swap these out when I'm done
            // Self::ProxyType(_p) => write!(f, "<put the other thing back>"),
            Self::ProxyType((_, _, p)) => write!(f, "{p}"),
            Self::Range(range) => write!(f, "{range:?}"),
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::String(str_) => write!(f, "{str_}"),
            Self::Struct(ty) => write!(f, "{}", s_read!(ty)),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{table:?}"),
            Self::Thonk(name, number) => write!(f, "{name} [{number}]"),
            Self::Unknown => write!(f, "<unknown>"),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
            Self::Vector(vec) => write!(f, "{vec:?}"),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Self::Integer(value as i64)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Self::Integer(value as i64)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Integer(value as i64)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Self::Integer(value as i64)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_owned())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl<T> From<Vec<T>> for Value
where
    T: Into<Value>,
{
    fn from(value: Vec<T>) -> Self {
        let value = value
            .into_iter()
            .map(|v| new_ref!(Value, v.into()))
            .collect();
        Self::Vector(value)
    }
}

impl TryFrom<Value> for Range<DwarfInteger> {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <Range<DwarfInteger> as TryFrom<Value>>::Error> {
        match value {
            Value::Range(range) => Ok(range.start..range.end),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "Uuid".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for Range<usize> {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <Range<usize> as TryFrom<Value>>::Error> {
        match value {
            Value::Range(range) => Ok(range.start as usize..range.end as usize),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "Uuid".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for Uuid {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <Uuid as TryFrom<Value>>::Error> {
        match value {
            Value::Uuid(uuid) => Ok(uuid),
            Value::String(str_) => str_.parse::<Uuid>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "Uuid".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "Uuid".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for Uuid {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <Uuid as TryFrom<&Value>>::Error> {
        match value {
            Value::Uuid(uuid) => Ok(*uuid),
            Value::String(str_) => str_.parse::<Uuid>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "Uuid".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "Uuid".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for usize {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <usize as TryFrom<Value>>::Error> {
        match value {
            Value::Float(num) => Ok(num as usize),
            Value::Integer(num) => Ok(num as usize),
            Value::String(str_) => str_.parse::<usize>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "usize".to_owned(),
            }),
            Value::Thonk(_, num) => Ok(num),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "usize".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for usize {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <usize as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as usize),
            Value::Integer(num) => Ok(*num as usize),
            Value::String(str_) => str_.parse::<usize>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "usize".to_owned(),
            }),
            Value::Thonk(_, num) => Ok(*num),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "usize".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <i64 as TryFrom<Value>>::Error> {
        match value {
            Value::Float(num) => Ok(num as i64),
            Value::Integer(num) => Ok(num),
            Value::String(str_) => str_.parse::<i64>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "i64".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "i64".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for i64 {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <i64 as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as i64),
            Value::Integer(num) => Ok(*num),
            Value::String(str_) => str_.parse::<i64>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "i64".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "i64".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <f64 as TryFrom<Value>>::Error> {
        match value {
            Value::Float(num) => Ok(num),
            Value::Integer(num) => Ok(num as f64),
            Value::String(str_) => str_.parse::<f64>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "f64".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "f64".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for f64 {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <f64 as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num),
            Value::Integer(num) => Ok(*num as f64),
            Value::String(str_) => str_.parse::<f64>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "f64".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "f64".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <String as TryFrom<Value>>::Error> {
        Ok(value.to_string())
    }
}

impl TryFrom<&Value> for String {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <String as TryFrom<&Value>>::Error> {
        Ok(value.to_string())
    }
}

impl TryFrom<Value> for bool {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <bool as TryFrom<Value>>::Error> {
        match value {
            Value::Boolean(bool_) => Ok(bool_),
            Value::Integer(num) => Ok(num != 0),
            Value::String(str_) => str_.parse::<bool>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "bool".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "bool".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for bool {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <bool as TryFrom<&Value>>::Error> {
        match value {
            Value::Boolean(bool_) => Ok(*bool_),
            Value::Integer(num) => Ok(*num != 0),
            Value::String(str_) => str_.parse::<bool>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "bool".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "bool".to_owned(),
            }),
        }
    }
}

/// Addition operator for Value
///
/// Implement the addition trait for Value.
impl std::ops::Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            // (Value::Float(a), Value::String(b)) => Value::String(a.to_string() + &b),
            (Value::String(a), Value::Float(b)) => Value::String(a + &b.to_string()),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            // (Value::Integer(a), Value::String(b)) => Value::String(a.to_string() + &b),
            // (Value::String(a), Value::Integer(b)) => Value::String(a + &b.to_string()),
            (Value::String(a), Value::String(b)) => Value::String(a + &b),
            (Value::Char(a), Value::Char(b)) => Value::String(a.to_string() + &b.to_string()),
            (Value::Char(a), Value::String(b)) => Value::String(a.to_string() + &b),
            (Value::String(a), Value::Char(b)) => Value::String(a + &b.to_string()),
            (Value::Empty, Value::Empty) => Value::Empty,
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a || b),
            // (Value::Boolean(a), Value::String(b)) => Value::String(a.to_string() + &b),
            // (Value::String(a), Value::Boolean(b)) => Value::String(a + &b.to_string()),
            (a, b) => Value::Error(format!("Cannot add {} and {}", a, b)),
        }
    }
}

/// AsRef<str> implementation for Value
///
impl AsRef<str> for Value {
    fn as_ref(&self) -> &str {
        match self {
            Value::Boolean(bool_) => match bool_ {
                true => "true",
                false => "false",
            },
            Value::Empty => "()",
            Value::String(str_) => str_,
            _ => "",
        }
    }
}

/// Subtraction operator for Value
///
/// Implement the Sub trait for Value.
impl std::ops::Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (Value::Empty, Value::Empty) => Value::Empty,
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a && b),
            (a, b) => Value::Error(format!("Cannot subtract {} and {}", a, b)),
        }
    }
}

/// Multiplication operator for Value
///
/// Implement the multiplication trait for Value.
impl std::ops::Mul for Value {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
            (Value::Empty, Value::Empty) => Value::Empty,
            (a, b) => Value::Error(format!("Cannot multiply {} and {}", a, b)),
        }
    }
}

/// Negation Operator
///
/// Implement negation trait for Value
impl std::ops::Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Float(a) => Value::Float(-a),
            Value::Integer(a) => Value::Integer(-a),
            Value::Empty => Value::Empty,
            a => Value::Error(format!("Cannot negate {}", a)),
        }
    }
}

/// Not Operator
///
/// Implement negation trait for Value
impl std::ops::Not for Value {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(a) => Value::Boolean(!a),
            Value::Empty => Value::Empty,
            a => Value::Error(format!("Cannot bang {}", a)),
        }
    }
}

/// Division operator for Value
///
/// Implement the division trait for Value.
impl std::ops::Div for Value {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a / b),
            (Value::Empty, Value::Empty) => Value::Empty,
            (a, b) => Value::Error(format!("Cannot divide {} and {}", a, b)),
        }
    }
}

/// Less than operator for Value
///
///
impl Value {
    pub fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a < b,
            (Value::Float(a), Value::Integer(b)) => a < &(*b as DwarfFloat),
            (Value::Integer(a), Value::Integer(b)) => a < b,
            (Value::Integer(a), Value::Float(b)) => (*a as DwarfFloat) < *b,
            (Value::String(a), Value::String(b)) => a < b,
            (Value::Char(a), Value::Char(b)) => a < b,
            (Value::Empty, Value::Empty) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a < b,
            (_, _) => false, //Value::Error(format!("Cannot compare {} and {}", a, b)),
        }
    }
}
/// Greater than operator for Value
///
///
impl Value {
    pub fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a > b,
            (Value::Float(a), Value::Integer(b)) => a > &(*b as DwarfFloat),
            (Value::Integer(a), Value::Integer(b)) => a > b,
            (Value::Integer(a), Value::Float(b)) => *a as DwarfFloat > *b,
            (Value::String(a), Value::String(b)) => a > b,
            (Value::Char(a), Value::Char(b)) => a > b,
            (Value::Empty, Value::Empty) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a > b,
            (_, _) => false, //Value::Error(format!("Cannot compare {} and {}", a, b)),
        }
    }
}

/// Greater than-equal operator for Value
///
///
impl Value {
    pub fn gte(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a >= b,
            (Value::Float(a), Value::Integer(b)) => a >= &(*b as DwarfFloat),
            (Value::Integer(a), Value::Integer(b)) => a >= b,
            (Value::Integer(a), Value::Float(b)) => *a as DwarfFloat >= *b,
            (Value::String(a), Value::String(b)) => a >= b,
            (Value::Char(a), Value::Char(b)) => a >= b,
            (Value::Empty, Value::Empty) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a >= b,
            (_, _) => false, //Value::Error(format!("Cannot compare {} and {}", a, b)),
        }
    }
}

/// Less than or equal to operator for Value
///
///
impl Value {
    pub fn lte(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a <= b,
            (Value::Float(a), Value::Integer(b)) => a <= &(*b as DwarfFloat),
            (Value::Integer(a), Value::Integer(b)) => a <= b,
            (Value::Integer(a), Value::Float(b)) => (*a as DwarfFloat) <= *b,
            (Value::String(a), Value::String(b)) => a <= b,
            (Value::Char(a), Value::Char(b)) => a <= b,
            (Value::Empty, Value::Empty) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a <= b,
            (_, _) => false, //Value::Error(format!("Cannot compare {} and {}", a, b)),
        }
    }
}

/// Equal operator for Value
///
/// This is testing value equality.
/// Equality is transitive. So, given a = 1, b = 1 a == b is true because there
/// is a single <word> 1 in the machine.
impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Empty, Value::Empty) => true,
            (Value::Enum(a), Value::Enum(b)) => *s_read!(a) == *s_read!(b),
            (Value::EnumVariant(a), Value::EnumVariant(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Float(a), Value::Integer(b)) => a == &(*b as DwarfFloat),
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(a), Value::Float(b)) => (*a as DwarfFloat) == *b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Struct(a), Value::Struct(b)) => *s_read!(a) == *s_read!(b),
            (Value::Uuid(a), Value::Uuid(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => {
                if a.len() != b.len() {
                    return false;
                }

                for (i, v) in a.iter().enumerate() {
                    if !s_read!(v).eq(&s_read!(b[i])) {
                        return false;
                    }
                }

                true
            }
            (_, _) => false, //Value::Error(format!("Cannot compare {} and {}", a, b)),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct UserTypeAttribute(HashMap<String, RefType<Value>>);

impl PartialEq for UserTypeAttribute {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        for (k, v) in self.0.iter() {
            if !other.0.contains_key(k) {
                return false;
            }

            if !s_read!(v).eq(&s_read!(other.0.get(k).unwrap())) {
                return false;
            }
        }

        true
    }
}

impl Eq for UserTypeAttribute {}

#[derive(Clone, Debug)]
pub struct UserEnum {
    type_name: String,
    type_: RefType<ValueType>,
    value: RefType<Value>,
}

impl PartialEq for UserEnum {
    fn eq(&self, other: &Self) -> bool {
        s_read!(self.type_).eq(&s_read!(other.type_))
            && s_read!(self.value).eq(&s_read!(other.value))
    }
}

impl Eq for UserEnum {}

impl UserEnum {
    pub fn new<S: AsRef<str>>(
        type_name: S,
        type_: &RefType<ValueType>,
        value: RefType<Value>,
    ) -> Self {
        Self {
            type_name: type_name.as_ref().to_owned(),
            type_: type_.clone(),
            value,
        }
    }

    pub fn get_value(&self) -> RefType<Value> {
        self.value.clone()
    }

    pub fn get_type(&self) -> &RefType<ValueType> {
        &self.type_
    }
}

impl fmt::Display for UserEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}::{}", self.type_name, s_read!(self.value))
    }
}

#[derive(Clone, Debug)]
pub struct UserStruct {
    type_name: String,
    type_: RefType<ValueType>,
    attrs: UserTypeAttribute,
}

impl PartialEq for UserStruct {
    fn eq(&self, other: &Self) -> bool {
        s_read!(self.type_).eq(&s_read!(other.type_)) && self.attrs.eq(&other.attrs)
    }
}

impl Eq for UserStruct {}

impl UserStruct {
    pub fn new<S: AsRef<str>>(type_name: S, type_: &RefType<ValueType>) -> Self {
        Self {
            type_name: type_name.as_ref().to_owned(),
            type_: type_.clone(),
            attrs: UserTypeAttribute::default(),
        }
    }

    /// Create a field for the user type
    ///
    /// This is called during type definition, from a declaration in a source file.
    pub fn define_field<S: AsRef<str>>(&mut self, name: S, value: RefType<Value>) {
        self.attrs.0.insert(name.as_ref().to_owned(), value);
    }

    pub fn get_field_value<S: AsRef<str>>(&self, name: S) -> Option<&RefType<Value>> {
        self.attrs.0.get(name.as_ref())
    }

    pub fn set_field_value<S: AsRef<str>>(
        &mut self,
        name: S,
        value: RefType<Value>,
    ) -> Option<RefType<Value>> {
        self.attrs.0.insert(name.as_ref().to_owned(), value)
    }

    pub fn get_type(&self) -> &RefType<ValueType> {
        &self.type_
    }
}

impl fmt::Display for UserStruct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut attrs = self.attrs.0.iter().collect::<Vec<_>>();
        attrs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        let mut out = f.debug_struct(&self.type_name);
        for (k, v) in attrs {
            out.field(k, &format_args!("{}", &s_read!(v)));
        }

        out.finish()
    }
}
