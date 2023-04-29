use std::{
    collections::VecDeque,
    fmt,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use fxhash::FxHashMap as HashMap;

use sarzak::lu_dog::{Function, ValueType};
use uuid::Uuid;

use crate::{InnerError, Result};

pub trait StoreProxy: fmt::Display + fmt::Debug {
    fn get_struct_uuid(&self) -> Uuid;

    fn call(
        &mut self,
        method: &str,
        args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)>;
}

/// This is an actual Value
///
/// This is the type used by the interpreter to represent values.
#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Empty,
    Error(String),
    Float(f64),
    // 🚧 I need to rethink the necessity of this locking.
    Function(Arc<RwLock<Function>>),
    Integer(i64),
    Option(Option<Box<Self>>),
    /// WTF was I thinking?
    ///
    /// That means Self. Or, maybe self?
    Reflexive,
    // StoreType(StoreType),
    // 🚧 I need to rethink the necessity of this locking.
    String(Arc<RwLock<String>>),
    Table(HashMap<String, Value>),
    /// User Defined Type
    ///
    ///  Feels like we'll need to generate some code to make this work.
    UserType(Arc<RwLock<dyn StoreProxy>>),
    Uuid(uuid::Uuid),
    Vector(Vec<Value>),
}

impl fmt::Display for Value {
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
            Self::UserType(ut) => write!(f, "{}", ut.read().unwrap()),
            Self::Uuid(uuid) => write!(f, "{}", uuid),
            Self::Vector(vec) => write!(f, "{:?}", vec),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = InnerError;

    fn try_from(value: Value) -> Result<Self, <i64 as TryFrom<Value>>::Error> {
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

impl TryFrom<Value> for f64 {
    type Error = InnerError;

    fn try_from(value: Value) -> Result<Self, <f64 as TryFrom<Value>>::Error> {
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
