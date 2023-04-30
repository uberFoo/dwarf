use std::{
    collections::VecDeque,
    fmt,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use fxhash::FxHashMap as HashMap;

use sarzak::{
    lu_dog::{Function, ObjectStore as LuDogStore, ValueType},
    sarzak::Ty,
};
use uuid::Uuid;

use crate::{interpreter::PrintableValueType, ChaChaError, Result};

pub trait StoreProxy: fmt::Display + fmt::Debug {
    /// Get the UUID of the type this proxy represents.
    ///
    fn get_struct_uuid(&self) -> Uuid;

    /// Get the UUID of the type of this proxy, in the store.
    ///
    fn get_store_uuid(&self) -> Uuid;

    /// Call a method on the proxy.
    ///
    fn call(
        &mut self,
        method: &str,
        args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)>;

    /// Read an attribute from the proxy.
    ///
    fn get_attr_value(&self, name: &str) -> Result<Value>;
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
    Function(Arc<RwLock<Function>>),
    Integer(i64),
    Option(Option<Box<Self>>),
    /// User Defined Type Proxy
    ///
    ///  Feels like we'll need to generate some code to make this work.
    ProxyType(Arc<RwLock<dyn StoreProxy>>),
    /// WTF was I thinking?
    ///
    /// That means Self. Or, maybe self?
    Reflexive,
    String(String),
    Table(HashMap<String, Value>),
    UserType(Arc<RwLock<UserType>>),
    Uuid(uuid::Uuid),
    Vector(Vec<Value>),
}

impl Value {
    pub fn get_type(&self, lu_dog: &LuDogStore) -> Arc<RwLock<ValueType>> {
        match &self {
            Value::Empty => ValueType::new_empty(),
            Value::Function(ref func) => {
                let func = lu_dog.exhume_function(&func.read().unwrap().id).unwrap();
                let z = func.read().unwrap().r1_value_type(lu_dog)[0].clone();
                z
            }
            Value::Integer(ref int) => {
                let ty = Ty::new_integer();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            // Value::StoreType(ref store) => {
            //     debug!("VariableExpression get type for store", store);
            //     store.get_type()
            // }
            Value::String(ref str) => {
                let ty = Ty::new_s_string();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            Value::ProxyType(ref pt) => lu_dog
                .exhume_value_type(&pt.read().unwrap().get_struct_uuid())
                .unwrap(),
            Value::UserType(ref ut) => ut.read().unwrap().get_type().clone(),
            Value::Uuid(ref uuid) => {
                let ty = Ty::new_s_uuid();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            value => ValueType::new_empty(),
        }
    }
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
            Self::ProxyType(p) => write!(f, "{}", p.read().unwrap()),
            Self::Reflexive => write!(f, "self"),
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::String(str_) => write!(f, "{}", str_),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{:?}", table),
            Self::UserType(ty) => writeln!(f, "{}", ty.read().unwrap()),
            Self::Uuid(uuid) => write!(f, "{}", uuid),
            Self::Vector(vec) => write!(f, "{:?}", vec),
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

#[derive(Clone, Debug)]
pub struct UserType {
    id: Uuid,
    type_: Arc<RwLock<ValueType>>,
    attrs: HashMap<String, Value>,
}

impl UserType {
    pub fn new(type_: Arc<RwLock<ValueType>>) -> Self {
        Self {
            id: Uuid::new_v4(),
            type_,
            attrs: HashMap::default(),
        }
    }

    pub fn add_attr<S: AsRef<str>>(&mut self, name: S, value: Value) {
        self.attrs.insert(name.as_ref().to_owned(), value);
    }

    pub fn get_attr_value<S: AsRef<str>>(&self, name: S) -> Option<&Value> {
        self.attrs.get(name.as_ref())
    }

    pub fn get_type(&self) -> &Arc<RwLock<ValueType>> {
        &self.type_
    }
}

impl fmt::Display for UserType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = f.debug_struct(&PrintableValueType(self.type_.clone()).to_string());
        let mut attrs = self.attrs.iter().collect::<Vec<_>>();
        attrs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        for (k, v) in attrs {
            out.field(k, v);
        }

        out.finish()
    }
}
