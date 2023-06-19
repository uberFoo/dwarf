use std::{any::Any, collections::VecDeque, fmt, ops::Range};

use ansi_term::Colour;
use fxhash::FxHashMap as HashMap;
// use parking_lot::Lock;
use uuid::Uuid;

use crate::{
    lu_dog::{Function, ObjectStore as LuDogStore, ValueType},
    new_ref, s_read,
    sarzak::Ty,
    ChaChaError, DwarfFloat, DwarfInteger, NewRef, RcType, RefType, Result,
};

pub trait StoreProxy: fmt::Display + fmt::Debug + Send + Sync {
    /// Get the name of the type this proxy represents.
    ///
    fn name(&self) -> &str;

    /// Get the UUID of the type this proxy represents.
    ///
    fn struct_uuid(&self) -> Uuid;

    /// Get the UUID of the type of this proxy, in the store.
    ///
    fn store_uuid(&self) -> Uuid;

    fn into_any(&self) -> Box<dyn Any>;

    // fn de_ref(&)

    /// Call a method on the proxy.
    ///
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<RefType<Value>>,
    ) -> Result<(RefType<Value>, RefType<ValueType>)>;

    /// Read an attribute from the proxy.
    ///
    fn get_attr_value(&self, name: &str) -> Result<RefType<Value>>;

    /// Write an attribute to the proxy.
    ///
    fn set_attr_value(&mut self, name: &str, value: RefType<Value>) -> Result<()>;
}

impl StoreProxy for Box<dyn StoreProxy> {
    fn name(&self) -> &str {
        self.as_ref().name()
    }

    fn struct_uuid(&self) -> Uuid {
        self.as_ref().struct_uuid()
    }

    fn store_uuid(&self) -> Uuid {
        self.as_ref().store_uuid()
    }

    fn into_any(&self) -> Box<dyn Any> {
        self.as_ref().into_any()
    }

    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<RefType<Value>>,
    ) -> Result<(RefType<Value>, RefType<ValueType>)> {
        self.as_mut().call(method, args)
    }

    fn get_attr_value(&self, name: &str) -> Result<RefType<Value>> {
        self.as_ref().get_attr_value(name)
    }

    fn set_attr_value(&mut self, name: &str, value: RefType<Value>) -> Result<()> {
        self.as_mut().set_attr_value(name, value)
    }
}

pub type ThreadHandleType =
    RcType<std::thread::JoinHandle<Result<(RefType<Value>, RefType<ValueType>)>>>;

pub struct ThreadHandle {
    join: ThreadHandleType,
    result: (RefType<Value>, RefType<ValueType>),
    complete: bool,
}

impl ThreadHandle {
    // pub fn new_thread<F>(f: F)
    // where
    //     F: Fn() -> Self,
    // {
    //     let handle = thread::spawn(|| f());
    // }

    pub fn new(join: ThreadHandleType, lu_dog: &LuDogStore) -> Self {
        ThreadHandle {
            join,
            result: (
                new_ref!(Value, Value::Unknown),
                ValueType::new_unknown(lu_dog),
            ),
            complete: false,
        }
    }
}

impl Clone for ThreadHandle {
    fn clone(&self) -> Self {
        ThreadHandle {
            join: self.join.clone(),
            result: self.result.clone(),
            complete: self.complete,
        }
    }
}

// impl std::ops::Deref for ThreadHandle {
//     type Target = Result<(RefType<Value>, RefType<ValueType>)>;

//     fn deref(&self) -> &Self::Target {
//         if self.join.is_finished() {}
//         // &self.join.join().unwrap()
//     }
// }

impl fmt::Debug for ThreadHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ThreadHandle({:?})", self.join.thread().id())
    }
}

impl fmt::Display for ThreadHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.join.thread().name().unwrap_or("unnamed"))
    }
}

/// This is an actual Value
///
/// This is the type used by the interpreter to represent values.
#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Char(char),
    Empty,
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
    Option(Option<RefType<Self>>),
    /// User Defined Type Proxy
    ///
    ///  Feels like we'll need to generate some code to make this work.
    ProxyType(RefType<dyn StoreProxy>),
    Range(Range<Box<RefType<Self>>>),
    Reference(RefType<Self>),
    /// WTF was I thinking?
    ///
    /// That means Self. Or, maybe self?
    Reflexive,
    String(String),
    Table(HashMap<String, RefType<Self>>),
    Thonk(&'static str, usize),
    Thread(ThreadHandle),
    Unknown,
    UserType(RefType<UserType>),
    Uuid(uuid::Uuid),
    Vector(Vec<RefType<Self>>),
}

impl Value {
    pub fn get_type(&self, lu_dog: &LuDogStore) -> RefType<ValueType> {
        match &self {
            // Value::Char =>
            Value::Empty => ValueType::new_empty(lu_dog),
            Value::Function(ref func) => {
                let func = lu_dog.exhume_function(&s_read!(func).id).unwrap();
                let z = s_read!(func).r1_value_type(lu_dog)[0].clone();
                z
            }
            Value::Float(ref _float) => {
                let ty = Ty::new_float();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            Value::Integer(ref _int) => {
                let ty = Ty::new_integer();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            Value::ProxyType(ref pt) => lu_dog
                .exhume_value_type(&s_read!(pt).struct_uuid())
                .unwrap(),
            Value::String(ref _str) => {
                let ty = Ty::new_s_string();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            Value::UserType(ref ut) => s_read!(ut).get_type().clone(),
            Value::Uuid(ref _uuid) => {
                let ty = Ty::new_s_uuid();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            value => {
                log::error!("Value::get_type() not implemented for {:?}", value);
                ValueType::new_unknown(lu_dog)
            }
        }
    }
}

// impl std::ops::Deref for Value {
//     type Target = Value;

//     fn deref(&self) -> &Self::Target {
//         match self {
//             Self::Thread(t) => t.deref(),
//             _ => self,
//         }
//     }
// }

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(bool_) => write!(f, "{}", bool_),
            Self::Char(char_) => write!(f, "{}", char_),
            Self::Empty => write!(f, "()"),
            Self::Error(e) => write!(f, "{}: {}", Colour::Red.bold().paint("error"), e),
            Self::Float(num) => write!(f, "{}", num),
            Self::Function(_) => write!(f, "<function>"),
            Self::Integer(num) => write!(f, "{}", num),
            Self::Option(option) => match option {
                Some(value) => write!(f, "Some({})", s_read!(value)),
                None => write!(f, "None"),
            },
            // 🚧 swap these out when I'm done
            Self::ProxyType(p) => write!(f, "<put the other thing back>"),
            // Self::ProxyType(p) => write!(f, "{}", s_read!(p)),
            Self::Range(range) => write!(f, "{:?}", range),
            Self::Reference(value) => write!(f, "&{}", s_read!(value)),
            Self::Reflexive => write!(f, "self"),
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::String(str_) => write!(f, "{}", str_),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{:?}", table),
            Self::Thonk(name, number) => write!(f, "{} [{}]", name, number),
            Self::Thread(thread) => write!(f, "{}", thread),
            Self::Unknown => write!(f, "<unknown>"),
            Self::UserType(ty) => writeln!(f, "{}", s_read!(ty)),
            Self::Uuid(uuid) => write!(f, "{}", uuid),
            Self::Vector(vec) => write!(f, "{:?}", vec),
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

// impl TryFrom<Value> for UserType

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

/// Greater than operator for Value
///
///
impl Value {
    pub fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a > b,
            (Value::Integer(a), Value::Integer(b)) => a > b,
            (Value::String(a), Value::String(b)) => a > b,
            (Value::Char(a), Value::Char(b)) => a > b,
            (Value::Empty, Value::Empty) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a > b,
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
            (Value::Integer(a), Value::Integer(b)) => a <= b,
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
impl Value {
    pub fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Empty, Value::Empty) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Uuid(a), Value::Uuid(b)) => a == b,
            (Value::UserType(a), Value::UserType(b)) => &*s_read!(a) == &*s_read!(b),
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
pub struct UserType {
    type_name: String,
    type_: RefType<ValueType>,
    attrs: UserTypeAttribute,
}

impl PartialEq for UserType {
    fn eq(&self, other: &Self) -> bool {
        s_read!(self.type_).eq(&s_read!(other.type_)) && self.attrs.eq(&other.attrs)
    }
}

impl Eq for UserType {}

impl UserType {
    pub fn new<S: AsRef<str>>(type_name: S, type_: &RefType<ValueType>) -> Self {
        Self {
            type_name: type_name.as_ref().to_owned(),
            type_: type_.clone(),
            attrs: UserTypeAttribute::default(),
        }
    }

    pub fn add_attr<S: AsRef<str>>(&mut self, name: S, value: RefType<Value>) {
        self.attrs.0.insert(name.as_ref().to_owned(), value);
    }

    pub fn get_attr_value<S: AsRef<str>>(&self, name: S) -> Option<&RefType<Value>> {
        self.attrs.0.get(name.as_ref())
    }

    pub fn set_attr_value<S: AsRef<str>>(
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

impl fmt::Display for UserType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = f.debug_struct(&self.type_name);
        let mut attrs = self.attrs.0.iter().collect::<Vec<_>>();
        attrs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        for (k, v) in attrs {
            out.field(k, &s_read!(v));
        }

        out.finish()
    }
}
