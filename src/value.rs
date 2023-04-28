use std::{
    collections::VecDeque,
    fmt,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use fxhash::FxHashMap as HashMap;

use sarzak::lu_dog::{Function, ObjectStore as LuDogStore, ValueType};

use crate::{InnerError, Result, Stack};

pub trait UserType: Clone + fmt::Display + fmt::Debug {
    type Value<T>: fmt::Display + fmt::Debug + Clone;

    fn initialize(stack: &mut Stack<Self>, lu_dog: &mut LuDogStore);

    fn get_type(&self) -> Arc<RwLock<ValueType>>;

    fn call<T>(
        &mut self,
        method: &str,
        args: VecDeque<Self::Value<T>>,
    ) -> Result<(Self::Value<T>, Arc<RwLock<ValueType>>)>;
}

/// This is an actual Value
///
/// This is the type used by the interpreter to represent values.
#[derive(Clone, Debug)]
pub enum Value<T>
where
    T: UserType,
{
    Boolean(bool),
    Empty,
    Error(String),
    Float(f64),
    // 🚧 I need to rething the necessity of this locking.
    Function(Arc<RwLock<Function>>),
    Integer(i64),
    Option(Option<Box<Self>>),
    /// WTF was I thinking?
    ///
    /// That means Self. Or, maybe self?
    Reflexive,
    // StoreType(StoreType),
    // 🚧 I need to rething the necessity of this locking.
    String(Arc<RwLock<String>>),
    Table(HashMap<String, Value<T>>),
    /// User Defined Type
    ///
    ///  Feels like we'll need to generate some code to make this work.
    UserType(T),
    Uuid(uuid::Uuid),
    Vector(Vec<Value<T>>),
}

impl<T> fmt::Display for Value<T>
where
    T: UserType,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(bool_) => write!(f, "{}", bool_),
            Self::Empty => write!(f, "()"),
            Self::Error(e) => write!(f, "{}: {}", Colour::Red.bold().paint("error"), e),
            Self::Float(num) => write!(f, "{}", num),
            Self::Function(_) => write!(f, "<function>"),
            Self::Integer(num) => write!(f, "{}", num),
            Self::Option(option) => match option {
                Some(value) => write!(f, "Some({})", value),
                None => write!(f, "None"),
            },
            Self::Reflexive => write!(f, "self"),
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::String(str_) => write!(f, "{}", str_.read().unwrap()),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{:?}", table),
            Self::UserType(ut) => write!(f, "{}", ut),
            Self::Uuid(uuid) => write!(f, "{}", uuid),
            Self::Vector(vec) => write!(f, "{:?}", vec),
        }
    }
}

impl<T> TryFrom<Value<T>> for i64
where
    T: UserType,
{
    type Error = InnerError;

    fn try_from(value: Value<T>) -> Result<Self, <i64 as TryFrom<Value<T>>>::Error> {
        match value {
            Value::Float(num) => Ok(num as i64),
            Value::Integer(num) => Ok(num),
            Value::String(str_) => Ok(str_.read().unwrap().parse::<i64>().unwrap()),
            _ => Err(InnerError::Conversion {
                src: value.to_string(),
                dst: "i64".to_owned(),
            }),
        }
    }
}

impl<T> TryFrom<Value<T>> for f64
where
    T: UserType,
{
    type Error = InnerError;

    fn try_from(value: Value<T>) -> Result<Self, <f64 as TryFrom<Value<T>>>::Error> {
        match value {
            Value::Float(num) => Ok(num),
            Value::Integer(num) => Ok(num as f64),
            Value::String(str_) => Ok(str_.read().unwrap().parse::<f64>().unwrap()),
            _ => Err(InnerError::Conversion {
                src: value.to_string(),
                dst: "f64".to_owned(),
            }),
        }
    }
}
