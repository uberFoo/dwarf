use std::{any::Any, collections::VecDeque, fmt, ops::Range, sync::Arc};

use ansi_term::Colour;
use fxhash::FxHashMap as HashMap;
// use parking_lot::Lock;
use sarzak::sarzak::Ty;
use uuid::Uuid;

use crate::{
    interpreter::{Context, PrintableValueType},
    lu_dog::{Function, ObjectStore as LuDogStore, ValueType},
    s_read, ChaChaError, DwarfFloat, DwarfInteger, RefType, Result,
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
    /// why I need the inner Function to be behind an RefType<<T>>. It seems
    /// excessive, and yet I know I've looked into it before.
    Function(RefType<Function>),
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
            Value::Integer(ref _int) => {
                let ty = Ty::new_integer();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            // Value::StoreType(ref store) => {
            //     debug!("VariableExpression get type for store", store);
            //     store.get_type()
            // }
            Value::String(ref _str) => {
                let ty = Ty::new_s_string();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            Value::ProxyType(ref pt) => lu_dog
                .exhume_value_type(&s_read!(pt).struct_uuid())
                .unwrap(),
            Value::UserType(ref ut) => s_read!(ut).get_type().clone(),
            Value::Uuid(ref _uuid) => {
                let ty = Ty::new_s_uuid();
                lu_dog.exhume_value_type(&ty.id()).unwrap()
            }
            value => {
                // log::error!("Value::get_type() not implemented for {:?}", value);
                ValueType::new_empty(lu_dog)
            }
        }
    }
}

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
            Self::ProxyType(p) => write!(f, "{}", s_read!(p)),
            Self::Range(range) => write!(f, "{:?}", range),
            Self::Reference(value) => write!(f, "&{}", s_read!(value)),
            Self::Reflexive => write!(f, "self"),
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::String(str_) => write!(f, "{}", str_),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{:?}", table),
            Self::Thonk(name, number) => write!(f, "{} [{}]", name, number),
            Self::UserType(ty) => writeln!(f, "{}", s_read!(ty)),
            Self::Uuid(uuid) => write!(f, "{}", uuid),
            Self::Vector(vec) => write!(f, "{:?}", vec),
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
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            (Value::String(a), Value::String(b)) => Value::String(a + &b),
            (Value::Char(a), Value::Char(b)) => Value::String(a.to_string() + &b.to_string()),
            (Value::Char(a), Value::String(b)) => Value::String(a.to_string() + &b),
            (Value::String(a), Value::Char(b)) => Value::String(a + &b.to_string()),
            (Value::Empty, Value::Empty) => Value::Empty,
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a || b),
            (a, b) => {
                dbg!(&a, &b);
                Value::Error(format!("Cannot add {} and {}", a, b))
            }
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
            (a, b) => {
                dbg!(&a, &b);
                Value::Error(format!("Cannot multiply {} and {}", a, b))
            }
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
            (a, b) => {
                dbg!(&a, &b);
                Value::Error(format!("Cannot divide {} and {}", a, b))
            }
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

#[derive(Clone, Debug)]
pub struct UserType {
    type_name: String,
    type_: RefType<ValueType>,
    attrs: HashMap<String, RefType<Value>>,
}

impl UserType {
    pub fn new(type_: &RefType<ValueType>, context: &Context) -> Self {
        Self {
            type_name: PrintableValueType(type_, context).to_string(),
            type_: type_.clone(),
            attrs: HashMap::default(),
        }
    }

    pub fn add_attr<S: AsRef<str>>(&mut self, name: S, value: RefType<Value>) {
        self.attrs.insert(name.as_ref().to_owned(), value);
    }

    pub fn get_attr_value<S: AsRef<str>>(&self, name: S) -> Option<&RefType<Value>> {
        self.attrs.get(name.as_ref())
    }

    pub fn get_type(&self) -> &RefType<ValueType> {
        &self.type_
    }
}

impl fmt::Display for UserType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = f.debug_struct(&self.type_name);
        let mut attrs = self.attrs.iter().collect::<Vec<_>>();
        attrs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        for (k, v) in attrs {
            out.field(k, &s_read!(v));
        }

        out.finish()
    }
}
