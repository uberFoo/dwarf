#[cfg(feature = "async")]
use std::future::Future;

use std::{fmt, io::Write, ops::Range};

#[cfg(feature = "async")]
use smol::future;

use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use sarzak::lu_dog::ValueTypeEnum;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[cfg(feature = "async")]
use puteketeke::Executor;

#[cfg(feature = "async")]
use crate::{ValueResult, VmValueResult};

use crate::{
    chacha::error::Result,
    lu_dog::{Function, Lambda, ObjectStore as LuDogStore, ValueType, ZObjectStore},
    new_ref,
    plug_in::PluginType,
    s_read,
    sarzak::{ObjectStore as SarzakStore, Ty},
    ChaChaError, Context, DwarfFloat, DwarfInteger, NewRef, RefType, PATH_SEP,
};

pub mod _enum;
pub mod _struct;

pub use _enum::{Enum, TupleEnum};
pub use _struct::Struct;

// 🚧 This can be deleted and replaced with just a String.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ThonkInner {
    Thonk(String),
    Index(usize),
}

#[derive(Default)]
pub enum Value {
    AnyList(RefType<Vec<RefType<Self>>>),
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
    Enumeration(Enum<Self>),
    // #[serde(skip)]
    Error(Box<ChaChaError>),
    Float(DwarfFloat),
    /// Function
    ///
    /// 🚧 I really need to write something here describing, once and for all,
    /// why I need the inner Function to be behind a RefType<<T>>. It seems
    /// excessive, and yet I know I've looked into it before.
    Function(RefType<Function>),
    FubarPointer {
        name: String,
        frame_size: usize,
        captures: Vec<RefType<Value>>,
    },
    #[cfg(feature = "async")]
    // #[serde(skip)]
    Future {
        name: String,
        executor: Executor,
        task: Option<puteketeke::AsyncTask<'static, ValueResult>>,
    },
    Integer(DwarfInteger),
    Lambda(RefType<Lambda>),
    List {
        ty: RefType<ValueType>,
        inner: RefType<Vec<RefType<Self>>>,
    },
    // #[serde(skip)]
    ParsedDwarf(Context),
    // #[serde(skip)]
    Plugin((String, RefType<PluginType>)),
    // #[serde(skip)]
    ProxyType {
        module: String,
        obj_ty: Uuid,
        id: Uuid,
        plugin: RefType<PluginType>,
    },
    // Reference(RefType<Self>),
    Range(Range<DwarfInteger>),
    // #[serde(skip)]
    Store(RefType<ZObjectStore>, RefType<PluginType>),
    String(String),
    Struct(RefType<Struct<Self>>),
    Table(HashMap<String, RefType<Self>>),
    #[cfg(feature = "async")]
    // #[serde(skip)]
    Task {
        worker: Option<puteketeke::Worker>,
        parent: Option<puteketeke::AsyncTask<'static, ValueResult>>,
    },
    // Thonk(ThonkInner),
    Unknown,
    Uuid(uuid::Uuid),
    ValueType(ValueType),
    #[cfg(feature = "async")]
    VmFuture {
        name: String,
        executor: Executor,
        task: Option<puteketeke::AsyncTask<'static, VmValueResult>>,
    },
}

impl Value {
    // pub fn new_thonk(name: String) -> Self {
    //     Self::Thonk(ThonkInner::Thonk(name))
    // }

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
            Self::AnyList(list) => {
                write!(f, "[")?;
                let list = s_read!(list);
                let mut first_time = true;
                for i in &*list {
                    if first_time {
                        first_time = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", s_read!(i))?;
                }
                write!(f, "]")
            }
            Self::Boolean(bool_) => write!(f, "{bool_}"),
            Self::Char(char_) => write!(f, "{char_}"),
            Self::Empty => write!(f, "()"),
            // Self::Enum(ty) => write!(f, "{}", s_read!(ty)),
            Self::Enumeration(var) => write!(f, "{var}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num}"),
            Self::Function(_) => write!(f, "<function>"),
            Self::FubarPointer {
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
            #[cfg(feature = "async")]
            Self::Future {
                name,
                executor,
                task,
            } => write!(f, "Task `{name}`: {task:?}, executor: {executor:?}"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::Lambda(_) => write!(f, "<lambda>"),
            Self::List { ty: _, inner } => {
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
            Self::ParsedDwarf(ctx) => write!(f, "{ctx:#?}"),
            Self::Plugin((name, _plugin)) => write!(f, "plugin::{name}"),
            Self::ProxyType {
                module: _,
                obj_ty: _,
                id: _,
                plugin,
            } => write!(f, "{}", s_read!(plugin)),
            Self::Range(range) => write!(f, "{range:?}"),
            // Self::Reference(value) => {
            // write!(f, "<reference>({})", s_read!(value).to_inner_string())
            // }
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::Store(_store, plugin) => write!(f, "Plug-in ({})", s_read!(plugin).name()),
            Self::String(str_) => write!(f, "{str_}"),
            Self::Struct(ty) => write!(f, "{}", s_read!(ty)),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{table:?}"),
            #[cfg(feature = "async")]
            Self::Task { worker, parent } => write!(f, "Task: {parent:?} running on {worker:?}"),
            // Self::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => write!(f, "{name}"),
            //     ThonkInner::Index(index) => write!(f, "{index}"),
            // },
            Self::Unknown => write!(f, "<unknown>"),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
            Self::ValueType(ty) => write!(f, "{:?}", ty),
            #[cfg(feature = "async")]
            Self::VmFuture {
                name,
                executor,
                task,
            } => write!(f, "VmTask `{name}`: {task:?}, executor: {executor:?}"),
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
                Enum::Unit(t, _, _) => t.clone(),
                Enum::Struct(ut) => s_read!(ut).get_type().clone(),
                Enum::Tuple((ty, _), _) => ty.clone(),
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
            Value::Function(ref func) => {
                let func = lu_dog.exhume_function(&s_read!(func).id).unwrap();
                let z = s_read!(func).r1_value_type(lu_dog)[0].clone();
                #[allow(clippy::let_and_return)]
                z
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
            Value::Lambda(ref ƛ) => {
                let ƛ = lu_dog.exhume_lambda(&s_read!(ƛ).id).unwrap();
                let ƛ_type = s_read!(ƛ).r1_value_type(lu_dog)[0].clone();
                #[allow(clippy::let_and_return)]
                ƛ_type
            }
            Value::List { ty, inner: _ } => {
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
            Value::ProxyType {
                module: _,
                obj_ty: uuid,
                id: _,
                plugin: _,
            } => {
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
            Value::Range(_) => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Range(_) = s_read!(vt).subtype {
                        return vt.clone();
                    }
                }
                unreachable!()
            }
            Value::Store(store, _plugin) => s_read!(store).r1_value_type(lu_dog)[0].clone(),
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
            #[cfg(feature = "async")]
            Value::Task {
                worker: _,
                parent: _,
            } => {
                for vt in lu_dog.iter_value_type() {
                    if let ValueTypeEnum::Task(_) = s_read!(vt).subtype {
                        return vt.clone();
                    }
                }
                unreachable!()
            }
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

#[cfg(feature = "async")]
impl Future for Value {
    type Output = RefType<Value>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        _cx_: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = std::pin::Pin::into_inner(self);

        match this {
            Self::Future {
                name: _,
                executor,
                task,
            } => {
                if let Some(task) = task.take() {
                    executor.start_task(&task);
                    match future::block_on(task) {
                        Ok(value) => std::task::Poll::Ready(value),
                        Err(e) => {
                            std::task::Poll::Ready(new_ref!(Value, Value::Error(Box::new(e))))
                        }
                    }
                } else {
                    std::task::Poll::Ready(new_ref!(Value, Value::Empty))
                }
            }
            Self::Task { worker: _, parent } => {
                if let Some(task) = parent.take() {
                    match future::block_on(task) {
                        Ok(value) => std::task::Poll::Ready(value),
                        Err(e) => {
                            std::task::Poll::Ready(new_ref!(Value, Value::Error(Box::new(e))))
                        }
                    }
                } else {
                    std::task::Poll::Ready(new_ref!(Value, Value::Empty))
                }
            }
            _ => std::task::Poll::Ready(new_ref!(Value, Value::Empty)),
        }
    }
}

// impl Drop for Value {
//     fn drop(&mut self) {
//         match self {
//             Self::Task(_, Some(task)) => {
//                 log::debug!(target: "async", "drop task: {:?}", task);
//                 // future::block_on(task);
//             }
//             _ => {}
//         }
//     }
// }

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::AnyList(list) => {
                write!(f, "[")?;
                let list = s_read!(list);
                let mut first_time = true;
                for i in &*list {
                    if first_time {
                        first_time = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", s_read!(i))?;
                }
                write!(f, "]")
            }
            Self::Boolean(b) => write!(f, "{b:?}"),
            Self::Char(c) => write!(f, "{c:?}"),
            Self::Empty => write!(f, "()"),
            Self::Enumeration(var) => write!(f, "{var:?}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num:?}"),
            Self::Function(func) => write!(f, "{:?}", s_read!(func)),
            Self::FubarPointer {
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
            #[cfg(feature = "async")]
            Self::Future {
                name,
                executor,
                task,
            } => write!(f, "Task `{name}`: {task:?}, executor: {executor:?}"),
            Self::Integer(num) => write!(f, "{num:?}"),
            Self::Lambda(ƛ) => write!(f, "{:?}", s_read!(ƛ)),
            Self::List { ty, inner } => write!(f, "{ty:?}: {inner:?}"),
            Self::ParsedDwarf(ctx) => write!(f, "{ctx:?}"),
            Self::Plugin((name, _plugin)) => write!(f, "plugin::{name}"),
            Self::ProxyType {
                module,
                obj_ty,
                id,
                plugin,
            } => write!(
                f,
                "ProxyType {{ module: {}, obj_ty: {}, id: {}, plugin: {} }}",
                module,
                obj_ty,
                id,
                s_read!(plugin).name()
            ),
            Self::Range(range) => write!(f, "{range:?}"),
            // Self::Reference(value) => {
            //     write!(f, "<reference>({})", s_read!(value).to_inner_string())
            // }
            Self::Store(store, plugin) => write!(
                f,
                "Store {{ store: {:?}, plugin: {} }}",
                s_read!(store),
                s_read!(plugin).name()
            ),
            Self::String(s) => write!(f, "{s:?}"),
            Self::Struct(ty) => write!(f, "{:?}", s_read!(ty)),
            Self::Table(table) => write!(f, "{table:?}"),
            #[cfg(feature = "async")]
            Self::Task { worker, parent } => write!(f, "Task: {parent:?} running on {worker:?}"),
            // Self::Thonk(inner) => write!(f, "{inner:?}"),
            Self::Unknown => write!(f, "<unknown>"),
            Self::Uuid(uuid) => write!(f, "{uuid:?}"),
            Self::ValueType(ty) => write!(f, "{:?}", ty),
            #[cfg(feature = "async")]
            Self::VmFuture {
                name,
                executor,
                task,
            } => write!(f, "VmTask `{name}`: {task:?}, executor: {executor:?}"),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::AnyList(list) => Self::AnyList(list.clone()),
            Self::Boolean(bool_) => Self::Boolean(*bool_),
            Self::Char(char_) => Self::Char(*char_),
            Self::Empty => Self::Empty,
            Self::Enumeration(var) => Self::Enumeration(var.clone()),
            Self::Error(_e) => unimplemented!(),
            Self::Float(num) => Self::Float(*num),
            Self::Function(func) => Self::Function(func.clone()),
            Self::FubarPointer {
                name,
                frame_size,
                captures: captured,
            } => Self::FubarPointer {
                name: name.to_owned(),
                frame_size: *frame_size,
                captures: captured.clone(),
            },
            #[cfg(feature = "async")]
            // Note that cloned values do not inherit the task
            Self::Future {
                name,
                executor,
                task: _,
            } => Self::Future {
                name: name.to_owned(),
                executor: executor.clone(),
                task: None,
            },
            Self::Integer(num) => Self::Integer(*num),
            Self::Lambda(ƛ) => Self::Lambda(ƛ.clone()),
            Self::List { ty, inner } => Self::List {
                ty: ty.clone(),
                inner: inner.clone(),
            },
            Self::ParsedDwarf(ctx) => Self::ParsedDwarf(ctx.clone()),
            Self::Plugin(plugin) => Self::Plugin(plugin.clone()),
            Self::ProxyType {
                module,
                obj_ty,
                id,
                plugin,
            } => Self::ProxyType {
                module: module.clone(),
                obj_ty: *obj_ty,
                id: *id,
                plugin: plugin.clone(),
            },
            Self::Range(range) => Self::Range(range.clone()),
            // Self::Reference(value) => Self::Reference(value.clone()),
            Self::Store(store, plugin) => Self::Store(store.clone(), plugin.clone()),
            Self::String(str_) => Self::String(str_.clone()),
            Self::Struct(ty) => Self::Struct(ty.clone()),
            Self::Table(table) => Self::Table(table.clone()),
            #[cfg(feature = "async")]
            // Note that cloned values do not inherit the task
            Self::Task { worker, parent: _ } => Self::Task {
                worker: worker.clone(),
                parent: None,
            },
            // Self::Thonk(inner) => Self::Thonk(inner.clone()),
            Self::Unknown => Self::Unknown,
            Self::Uuid(uuid) => Self::Uuid(*uuid),
            Self::ValueType(ty) => Self::ValueType(ty.clone()),
            #[cfg(feature = "async")]
            // Note that cloned values do not inherit the task
            Self::VmFuture {
                name,
                executor,
                task: _,
            } => Self::VmFuture {
                name: name.to_owned(),
                executor: executor.clone(),
                task: None,
            },
        }
    }
}

/// NB: This is what get's spit out of dwarf print statements.
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::AnyList(list) => {
                write!(f, "[")?;
                let list = s_read!(list);
                let mut first_time = true;
                for i in &*list {
                    if first_time {
                        first_time = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", s_read!(i))?;
                }
                write!(f, "]")
            }
            Self::Boolean(bool_) => write!(f, "{bool_}"),
            Self::Char(char_) => write!(f, "{char_}"),
            Self::Empty => write!(f, "()"),
            // Self::Enum(ty) => write!(f, "{}", s_read!(ty)),
            Self::Enumeration(var) => write!(f, "{var}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num}"),
            Self::Function(_) => write!(f, "<function>"),
            Self::FubarPointer {
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
            #[cfg(feature = "async")]
            Self::Future {
                name,
                executor,
                task,
            } => write!(f, "Task `{name}`: {task:?}, executor: {executor:?}"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::Lambda(_) => write!(f, "<lambda>"),
            Self::List { ty: _, inner } => {
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
            Self::ParsedDwarf(ctx) => write!(f, "{ctx:#?}"),
            Self::Plugin((name, _plugin)) => write!(f, "plugin::{name}"),
            Self::ProxyType {
                module: _,
                obj_ty: _,
                id: _,
                plugin,
            } => write!(f, "{}", s_read!(plugin)),
            Self::Range(range) => write!(f, "{range:?}"),
            // Self::Reference(value) => {
            //     write!(f, "<reference>({})", s_read!(value).to_inner_string())
            // }
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::Store(_store, plugin) => write!(f, "Plug-in ({})", s_read!(plugin).name()),
            // Self::String(str_) => write!(f, "{str_}"),
            Self::String(str_) => write!(f, "\"{str_}\""),
            Self::Struct(ty) => write!(f, "{}", s_read!(ty)),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{table:?}"),
            #[cfg(feature = "async")]
            Self::Task { worker, parent } => write!(f, "Task: {parent:?} running on {worker:?}"),
            // Self::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => write!(f, "Thonk({name})"),
            //     ThonkInner::Index(index) => write!(f, "Thonk({index})"),
            // },
            Self::Unknown => write!(f, "<unknown>"),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
            Self::ValueType(ty) => write!(f, "{:?}", ty),
            #[cfg(feature = "async")]
            Self::VmFuture {
                name,
                executor,
                task,
            } => write!(f, "VmTask `{name}`: {task:?}, executor: {executor:?}"),
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

impl From<Range<usize>> for Value {
    fn from(value: Range<usize>) -> Self {
        Self::Range(value.start as DwarfInteger..value.end as DwarfInteger)
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

impl TryFrom<&Value> for ValueType {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ValueType as TryFrom<&Value>>::Error> {
        match &value {
            Value::ValueType(ty) => Ok(ty.to_owned()),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ValueType".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for ValueType {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <ValueType as TryFrom<Value>>::Error> {
        match &value {
            Value::ValueType(ty) => Ok(ty.to_owned()),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ValueType".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for Context {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <Context as TryFrom<Value>>::Error> {
        match &value {
            Value::ParsedDwarf(ctx) => Ok(ctx.to_owned()),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "Context".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for Range<DwarfInteger> {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <Range<DwarfInteger> as TryFrom<Value>>::Error> {
        match &value {
            Value::Range(range) => Ok(range.start..range.end),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "range".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for Range<usize> {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <Range<usize> as TryFrom<Value>>::Error> {
        match &value {
            Value::Range(range) => Ok(range.start as usize..range.end as usize),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "range".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for Range<usize> {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <Range<usize> as TryFrom<&Value>>::Error> {
        match value {
            Value::Range(range) => Ok(range.start as usize..range.end as usize),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "range".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for Uuid {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <Uuid as TryFrom<Value>>::Error> {
        match &value {
            Value::Uuid(uuid) => Ok(uuid.to_owned()),
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
        match &value {
            Value::Float(num) => Ok(num.to_owned() as usize),
            Value::Integer(num) => Ok(num.to_owned() as usize),
            Value::String(str_) => str_.parse::<usize>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "usize".to_owned(),
            }),
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(ChaChaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "usize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index),
            // },
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
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(ChaChaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "usize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index),
            // },
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "usize".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for isize {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <isize as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned() as isize),
            Value::Integer(num) => Ok(num.to_owned() as isize),
            Value::String(str_) => str_.parse::<isize>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "isize".to_owned(),
            }),
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(ChaChaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "isize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index as isize),
            // },
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "isize".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for isize {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <isize as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as isize),
            Value::Integer(num) => Ok(*num as isize),
            Value::String(str_) => str_.parse::<isize>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "isize".to_owned(),
            }),
            // Value::Thonk(inner) => match inner {
            //     ThonkInner::Thonk(name) => Err(ChaChaError::Conversion {
            //         src: (*name).to_owned(),
            //         dst: "isize".to_owned(),
            //     }),
            //     ThonkInner::Index(index) => Ok(*index as isize),
            // },
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "isize".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <i64 as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned() as DwarfInteger),
            Value::Integer(num) => Ok(num.to_owned()),
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
            Value::Float(num) => Ok(*num as DwarfInteger),
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

impl TryFrom<Value> for u64 {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <u64 as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned() as u64),
            Value::Integer(num) => Ok(num.to_owned() as u64),
            Value::String(str_) => str_.parse::<u64>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "u64".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "u64".to_owned(),
            }),
        }
    }
}

impl TryFrom<&Value> for u64 {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <u64 as TryFrom<&Value>>::Error> {
        match value {
            Value::Float(num) => Ok(*num as u64),
            Value::Integer(num) => Ok(*num as u64),
            Value::String(str_) => str_.parse::<u64>().map_err(|_| ChaChaError::Conversion {
                src: str_.to_owned(),
                dst: "u64".to_owned(),
            }),
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "u64".to_owned(),
            }),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <f64 as TryFrom<Value>>::Error> {
        match &value {
            Value::Float(num) => Ok(num.to_owned()),
            Value::Integer(num) => Ok(num.to_owned() as f64),
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
        Ok(value.to_inner_string())
    }
}

impl TryFrom<&Value> for String {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <String as TryFrom<&Value>>::Error> {
        Ok(value.to_inner_string())
    }
}

impl TryFrom<Value> for bool {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <bool as TryFrom<Value>>::Error> {
        match &value {
            Value::Boolean(bool_) => Ok(bool_.to_owned()),
            Value::Integer(num) => Ok(num != &0),
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
            (a, b) => Value::Error(Box::new(ChaChaError::Addition {
                left: a.clone(),
                right: b.clone(),
            })),
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
            (a, b) => Value::Error(Box::new(ChaChaError::Subtraction {
                left: a.clone(),
                right: b.clone(),
            })),
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
            (a, b) => Value::Error(Box::new(ChaChaError::Multiplication {
                left: a.clone(),
                right: b.clone(),
            })),
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
            a => Value::Error(Box::new(ChaChaError::Negation { value: a })),
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
            a => Value::Error(Box::new(ChaChaError::Bang { value: a })),
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
            (a, b) => Value::Error(Box::new(ChaChaError::Division {
                left: a.clone(),
                right: b.clone(),
            })),
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
            (
                Value::ProxyType {
                    module: _,
                    obj_ty: _,
                    id: a,
                    plugin: _,
                },
                Value::ProxyType {
                    module: _,
                    obj_ty: _,
                    id: b,
                    plugin: _,
                },
            ) => a == b,
            (Value::List { ty: ty_a, inner: a }, Value::List { ty: ty_b, inner: b }) => {
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
            (Value::Plugin((a, _)), Value::Plugin((b, _))) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Struct(a), Value::Struct(b)) => *s_read!(a) == *s_read!(b),
            (Value::Uuid(a), Value::Uuid(b)) => a == b,
            (_, _) => false, //Value::Error(format!("Cannot compare {} and {}", a, b)),
        }
    }
}
