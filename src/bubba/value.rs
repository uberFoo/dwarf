#[cfg(feature = "async")]
use std::future::Future;

use std::{fmt, io::Write, ops::Range};

use ansi_term::Colour;
#[cfg(feature = "async")]
use puteketeke::AsyncTask;
use serde::{Deserialize, Serialize};
#[cfg(feature = "async")]
use smol::future;
use uuid::Uuid;

use crate::{
    bubba::error::{BubbaError, Error},
    chacha::value::{EnumVariant, UserStruct},
    lu_dog::{ObjectStore as LuDogStore, ValueType, ValueTypeEnum},
    new_ref,
    plug_in::PluginType,
    s_read,
    sarzak::{ObjectStore as SarzakStore, Ty},
    DwarfFloat, DwarfInteger, NewRef, RefType, VmValueResult,
};

#[derive(Default, Deserialize, Serialize)]
pub enum Value {
    /// Boolean
    ///
    /// True and False
    Boolean(bool),
    /// Char
    ///
    /// A single character
    Char(char),
    #[default]
    /// Empty
    ///
    /// ()
    Empty,
    #[serde(skip)]
    Error(Box<Error>),
    Enumeration(EnumVariant<Self>),
    Float(DwarfFloat),
    Integer(DwarfInteger),
    LambdaPointer {
        name: String,
        frame_size: usize,
        captures: Vec<RefType<Value>>,
    },
    #[serde(skip)]
    Plugin((String, RefType<PluginType>)),
    Range(Range<DwarfInteger>),
    String(String),
    Struct(RefType<UserStruct<Self>>),
    #[serde(skip)]
    #[cfg(feature = "async")]
    Task {
        name: String,
        running: bool,
        task: Option<AsyncTask<'static, VmValueResult>>,
    },
    Uuid(uuid::Uuid),
    ValueType(ValueType),
    Vector {
        ty: RefType<ValueType>,
        inner: RefType<Vec<RefType<Self>>>,
    },
}

// #[cfg(feature = "async")]
// impl Future for Value {
//     type Output = RefType<Value>;

//     fn poll(
//         self: std::pin::Pin<&mut Self>,
//         _cx_: &mut std::task::Context<'_>,
//     ) -> std::task::Poll<Self::Output> {
//         let this = std::pin::Pin::into_inner(self);

//         match this {
//             Self::Task {
//                 name: _,
//                 running: _,
//                 task,
//             } => {
//                 if let Some(task) = task.take() {
//                     match future::block_on(task) {
//                         Ok(value) => std::task::Poll::Ready(value),
//                         Err(e) => {
//                             std::task::Poll::Ready(new_ref!(Value, Value::Error(Box::new(e))))
//                         }
//                     }
//                 } else {
//                     std::task::Poll::Ready(new_ref!(Value, Value::Empty))
//                 }
//             }
//             _ => std::task::Poll::Ready(new_ref!(Value, Value::Empty)),
//         }
//     }
// }

impl Value {
    #[inline]
    pub fn to_inner_string(&self) -> String {
        let mut buf = Vec::new();
        self.inner_string(&mut buf)
            .expect("inner_string returned an error unexpectedly");
        String::from_utf8(buf).expect("inner_string returned invalid UTF-8")
    }

    #[inline]
    fn inner_string(&self, f: &mut Vec<u8>) -> std::io::Result<()> {
        match self {
            Self::Boolean(bool_) => write!(f, "{bool_}"),
            Self::Char(char_) => write!(f, "{char_}"),
            Self::Empty => write!(f, "()"),
            // Self::Enum(ty) => write!(f, "{}", s_read!(ty)),
            Self::Enumeration(var) => write!(f, "{var}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num}"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::LambdaPointer {
                name,
                frame_size,
                captures,
            } => {
                write!(
                    f,
                    "FubarPointer {{ name: {name}, frame_size: {frame_size}, captures: ["
                )?;
                for i in captures {
                    let i = s_read!(i);
                    write!(f, "{i}, ")?;
                }
                write!(f, "] }}")
            }
            Self::Plugin((name, _plugin)) => write!(f, "plugin::{name}"),
            Self::Range(range) => write!(f, "{range:?}"),
            Self::String(str_) => write!(f, "{str_}"),
            Self::Struct(ty) => write!(f, "{}", s_read!(ty)),
            #[cfg(feature = "async")]
            Self::Task {
                name,
                running,
                task,
            } => write!(f, "VmTask `{name}`: {task:?}, running: {running}"),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
            Self::ValueType(ty) => write!(f, "{:?}", ty),
            Self::Vector { ty: _, inner } => {
                let inner = s_read!(inner);
                let mut first_time = true;
                write!(f, "[")?;
                for i in &*inner {
                    if first_time {
                        first_time = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", s_read!(i))?;
                }
                write!(f, "]")
            }
        }
    }

    pub fn get_value_type(&self, sarzak: &SarzakStore, lu_dog: &LuDogStore) -> RefType<ValueType> {
        match &self {
            Value::Boolean(_) => {
                let ty = Ty::new_boolean(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.read().unwrap().id() == _ty {
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
            Value::Enumeration(var) => match var {
                EnumVariant::Unit(t, _, _) => t.clone(),
                EnumVariant::Struct(ut) => s_read!(ut).get_type().clone(),
                EnumVariant::Tuple((ty, _), _) => ty.clone(),
            },
            Value::Float(_) => {
                let ty = Ty::new_float(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.read().unwrap().id() == _ty {
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
                        if ty.read().unwrap().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Plugin((name, _plugin)) => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::XPlugin(id) = s_read!(vt).subtype {
                        let plugin = lu_dog.exhume_x_plugin(&id).unwrap();
                        if s_read!(plugin).name == name.as_str() {
                            return vt.clone();
                        }
                    }
                }
                panic!("Plugin not found: {name}");
            }
            Value::Range(_) => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Range(_) = s_read!(vt).subtype {
                        return vt.clone();
                    }
                }
                unreachable!()
            }
            Value::String(_) => {
                let ty = Ty::new_z_string(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.read().unwrap().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Struct(ref ut) => s_read!(ut).get_type().clone(),
            Value::Uuid(_) => {
                let ty = Ty::new_z_uuid(sarzak);
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                        if ty.read().unwrap().id() == _ty {
                            return vt.clone();
                        }
                    }
                }
                unreachable!()
            }
            Value::Vector { ty, inner: _ } => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::List(id) = s_read!(vt).subtype {
                        let list = lu_dog.exhume_list(&id).unwrap();
                        let list_ty = s_read!(list).r36_value_type(lu_dog)[0].clone();
                        if *s_read!(ty) == *s_read!(list_ty) {
                            return vt.clone();
                        }
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

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Boolean(b) => write!(f, "{b:?}"),
            Self::Char(c) => write!(f, "{c:?}"),
            Self::Empty => write!(f, "()"),
            Self::Enumeration(var) => write!(f, "{var:?}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num:?}"),
            Self::Integer(num) => write!(f, "{num:?}"),
            Self::LambdaPointer {
                name,
                frame_size,
                captures,
            } => {
                write!(
                    f,
                    "FubarPointer {{ name: {name}, frame_size: {frame_size}, captures: ["
                )?;
                for i in captures {
                    let i = s_read!(i);
                    write!(f, "{i}, ")?;
                }
                write!(f, "] }}")
            }
            Self::Plugin((name, _plugin)) => write!(f, "plugin::{name}"),
            Self::Range(range) => write!(f, "{range:?}"),
            Self::String(s) => write!(f, "{s:?}"),
            Self::Struct(ty) => write!(f, "{:?}", s_read!(ty)),
            #[cfg(feature = "async")]
            Self::Task {
                name,
                running,
                task,
            } => write!(f, "VmTask `{name}`: {task:?}, running: {running}"),
            Self::ValueType(ty) => write!(f, "{:?}", ty),
            Self::Uuid(uuid) => write!(f, "{uuid:?}"),
            Self::Vector { ty, inner } => write!(f, "{ty:?}: {inner:?}"),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Boolean(bool_) => Self::Boolean(*bool_),
            Self::Char(char_) => Self::Char(*char_),
            Self::Empty => Self::Empty,
            Self::Enumeration(var) => Self::Enumeration(var.clone()),
            Self::Error(_e) => unimplemented!(),
            Self::Float(num) => Self::Float(*num),
            Self::Integer(num) => Self::Integer(*num),
            Self::LambdaPointer {
                name,
                frame_size,
                captures: captured,
            } => Self::LambdaPointer {
                name: name.to_owned(),
                frame_size: *frame_size,
                captures: captured.clone(),
            },
            Self::Plugin(plugin) => Self::Plugin(plugin.clone()),
            Self::Range(range) => Self::Range(range.clone()),
            Self::String(str_) => Self::String(str_.clone()),
            Self::Struct(ty) => Self::Struct(ty.clone()),
            #[cfg(feature = "async")]
            // Note that cloned values do not inherit the task
            Self::Task {
                name,
                running: _,
                task: _,
            } => Self::Task {
                name: name.to_owned(),
                running: false,
                task: None,
            },
            Self::ValueType(ty) => Self::ValueType(ty.clone()),
            Self::Uuid(uuid) => Self::Uuid(*uuid),
            Self::Vector { ty, inner } => Self::Vector {
                ty: ty.clone(),
                inner: inner.clone(),
            },
        }
    }
}

/// NB: This is what get's spit out of dwarf print statements.
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(bool_) => write!(f, "{bool_}"),
            Self::Char(char_) => write!(f, "{char_}"),
            Self::Empty => write!(f, "()"),
            // Self::Enum(ty) => write!(f, "{}", s_read!(ty)),
            Self::Enumeration(var) => write!(f, "{var}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num}"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::LambdaPointer {
                name,
                frame_size,
                captures,
            } => {
                write!(
                    f,
                    "FubarPointer {{ name: {name}, frame_size: {frame_size}, captures: ["
                )?;
                for i in captures {
                    let i = s_read!(i);
                    write!(f, "{i}, ")?;
                }
                write!(f, "] }}")
            }
            Self::Plugin((name, _plugin)) => write!(f, "plugin::{name}"),
            Self::Range(range) => write!(f, "{range:?}"),
            Self::String(str_) => write!(f, "\"{str_}\""),
            Self::Struct(ty) => write!(f, "{}", s_read!(ty)),
            #[cfg(feature = "async")]
            Self::Task {
                name,
                running,
                task,
            } => write!(f, "VmTask `{name}`: {task:?}, running: {running}"),
            Self::ValueType(ty) => write!(f, "{:?}", ty),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
            Self::Vector { ty: _, inner } => {
                let inner = s_read!(inner);
                let mut first_time = true;
                write!(f, "[")?;
                for i in &*inner {
                    if first_time {
                        first_time = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", s_read!(i))?;
                }
                write!(f, "]")
            }
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
        Self::Integer(value as DwarfInteger)
    }
}

impl From<isize> for Value {
    fn from(value: isize) -> Self {
        Self::Integer(value as DwarfInteger)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Self::Integer(value as DwarfInteger)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Integer(value as DwarfInteger)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Self::Integer(value as DwarfInteger)
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

impl From<Uuid> for Value {
    fn from(value: Uuid) -> Self {
        Self::Uuid(value)
    }
}

impl From<Value> for Option<Uuid> {
    fn from(option: Value) -> Self {
        match option {
            Value::Uuid(uuid) => Some(uuid),
            _ => None,
        }
    }
}

impl From<Range<usize>> for Value {
    fn from(value: Range<usize>) -> Self {
        Self::Range(value.start as DwarfInteger..value.end as DwarfInteger)
    }
}

impl TryFrom<&Value> for ValueType {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <ValueType as TryFrom<&Value>>::Error> {
        match &value {
            Value::ValueType(ty) => Ok(ty.to_owned()),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "ValueType".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for ValueType {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <ValueType as TryFrom<Value>>::Error> {
        match &value {
            Value::ValueType(ty) => Ok(ty.to_owned()),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "ValueType".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for Uuid {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <Uuid as TryFrom<Value>>::Error> {
        match &value {
            Value::Uuid(uuid) => Ok(uuid.to_owned()),
            Value::String(str_) => str_.parse::<Uuid>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "Uuid".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "Uuid".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for Uuid {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <Uuid as TryFrom<&Value>>::Error> {
        match value {
            Value::Uuid(uuid) => Ok(*uuid),
            Value::String(str_) => str_.parse::<Uuid>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "Uuid".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "Uuid".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for Range<DwarfInteger> {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <Range<DwarfInteger> as TryFrom<Value>>::Error> {
        match &value {
            Value::Range(range) => Ok(range.start..range.end),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "range".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for Range<usize> {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <Range<usize> as TryFrom<Value>>::Error> {
        match &value {
            Value::Range(range) => Ok(range.start as usize..range.end as usize),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "range".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for Range<usize> {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <Range<usize> as TryFrom<&Value>>::Error> {
        match value {
            Value::Range(range) => Ok(range.start as usize..range.end as usize),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "range".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for usize {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <usize as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned() as usize),
            Value::Integer(num) => Ok(num.to_owned() as usize),
            Value::String(str_) => str_.parse::<usize>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "usize".to_owned(),
                }
                .into()
            }),
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(BubbaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "usize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index),
            // },
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "usize".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for usize {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <usize as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as usize),
            Value::Integer(num) => Ok(*num as usize),
            Value::String(str_) => str_.parse::<usize>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "usize".to_owned(),
                }
                .into()
            }),
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(BubbaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "usize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index),
            // },
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "usize".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for isize {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <isize as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned() as isize),
            Value::Integer(num) => Ok(num.to_owned() as isize),
            Value::String(str_) => str_.parse::<isize>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "isize".to_owned(),
                }
                .into()
            }),
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(BubbaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "isize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index as isize),
            // },
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "isize".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for isize {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <isize as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as isize),
            Value::Integer(num) => Ok(*num as isize),
            Value::String(str_) => str_.parse::<isize>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "isize".to_owned(),
                }
                .into()
            }),
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(BubbaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "isize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index as isize),
            // },
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "isize".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <i64 as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned() as DwarfInteger),
            Value::Integer(num) => Ok(num.to_owned()),
            Value::String(str_) => str_.parse::<i64>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "i64".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "i64".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for i64 {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <i64 as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as DwarfInteger),
            Value::Integer(num) => Ok(*num),
            Value::String(str_) => str_.parse::<i64>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "i64".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "i64".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for u64 {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <u64 as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned() as u64),
            Value::Integer(num) => Ok(num.to_owned() as u64),
            Value::String(str_) => str_.parse::<u64>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "u64".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "u64".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for u64 {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <u64 as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as u64),
            Value::Integer(num) => Ok(*num as u64),
            Value::String(str_) => str_.parse::<u64>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "u64".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "u64".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <f64 as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned()),
            Value::Integer(num) => Ok(num.to_owned() as f64),
            Value::String(str_) => str_.parse::<f64>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "f64".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "f64".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for f64 {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <f64 as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num),
            Value::Integer(num) => Ok(*num as f64),
            Value::String(str_) => str_.parse::<f64>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "f64".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "f64".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <String as TryFrom<Value>>::Error> {
        Ok(value.to_inner_string())
    }
}

impl TryFrom<&Value> for String {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <String as TryFrom<&Value>>::Error> {
        Ok(value.to_inner_string())
    }
}

impl TryFrom<Value> for bool {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, <bool as TryFrom<Value>>::Error> {
        match &value {
            Value::Boolean(bool_) => Ok(bool_.to_owned()),
            Value::Integer(num) => Ok(num != &0),
            Value::String(str_) => str_.parse::<bool>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "bool".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "bool".to_owned(),
            }
            .into()),
        }
    }
}

impl TryFrom<&Value> for bool {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, <bool as TryFrom<&Value>>::Error> {
        match value {
            Value::Boolean(bool_) => Ok(*bool_),
            Value::Integer(num) => Ok(*num != 0),
            Value::String(str_) => str_.parse::<bool>().map_err(|_| {
                BubbaError::Conversion {
                    src: str_.to_owned(),
                    dst: "bool".to_owned(),
                }
                .into()
            }),
            _ => Err(BubbaError::Conversion {
                src: value.to_string(),
                dst: "bool".to_owned(),
            }
            .into()),
        }
    }
}

/// Addition operator for Value
///
/// Implement the addition trait for Value.
impl std::ops::Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (&self, &other) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            // (Value::Float(a), Value::String(b)) => Value::String(a.to_string() + &b),
            (Value::String(a), Value::Float(b)) => {
                Value::String(a.to_owned() + b.to_string().as_str())
            }
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            // (Value::Integer(a), Value::String(b)) => Value::String(a.to_string() + &b),
            (Value::String(a), Value::Integer(b)) => {
                Value::String(a.to_owned() + b.to_string().as_str())
            }
            (Value::String(a), Value::String(b)) => Value::String(a.to_owned() + b.as_str()),
            (Value::Char(a), Value::Char(b)) => {
                Value::String(a.to_string() + b.to_string().as_str())
            }
            (Value::Char(a), Value::String(b)) => Value::String(a.to_string() + b.as_str()),
            (Value::String(a), Value::Char(b)) => {
                Value::String(a.to_owned() + b.to_string().as_str())
            }
            (Value::Empty, Value::Empty) => Value::Empty,
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(*a || *b),
            // (Value::Boolean(a), Value::String(b)) => Value::String(a.to_string() + &b),
            // (Value::String(a), Value::Boolean(b)) => Value::String(a + &b.to_string()),
            (a, b) => Value::Error(Box::new(
                BubbaError::Addition {
                    left: a.clone(),
                    right: b.clone(),
                }
                .into(),
            )),
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
            (a, b) => Value::Error(Box::new(
                BubbaError::Subtraction {
                    left: a.clone(),
                    right: b.clone(),
                }
                .into(),
            )),
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
            (Value::Float(a), Value::Integer(b)) => Value::Float(a * b as DwarfFloat),
            (Value::Integer(a), Value::Float(b)) => Value::Float(a as DwarfFloat * b),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
            (Value::Empty, Value::Empty) => Value::Empty,
            (a, b) => Value::Error(Box::new(
                BubbaError::Multiplication {
                    left: a.clone(),
                    right: b.clone(),
                }
                .into(),
            )),
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
            a => Value::Error(Box::new(BubbaError::Negation { value: a }.into())),
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
            a => Value::Error(Box::new(BubbaError::Bang { value: a }.into())),
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
            (a, b) => Value::Error(Box::new(
                BubbaError::Division {
                    left: a.clone(),
                    right: b.clone(),
                }
                .into(),
            )),
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
            // (Value::Enum(a), Value::Enum(b)) => *s_read!(a) == *s_read!(b),
            (Value::Enumeration(a), Value::Enumeration(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Float(a), Value::Integer(b)) => a == &(*b as DwarfFloat),
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(a), Value::Float(b)) => (*a as DwarfFloat) == *b,
            (Value::Plugin((a, _)), Value::Plugin((b, _))) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Struct(a), Value::Struct(b)) => *s_read!(a) == *s_read!(b),
            (Value::Uuid(a), Value::Uuid(b)) => a == b,
            (Value::Vector { ty: ty_a, inner: a }, Value::Vector { ty: ty_b, inner: b }) => {
                if *s_read!(ty_a) != *s_read!(ty_b) {
                    return false;
                }

                let a = s_read!(a);
                let b = s_read!(b);

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
