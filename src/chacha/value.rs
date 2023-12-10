#[cfg(feature = "async")]
use std::future::Future;

use std::{fmt, ops::Range};

#[cfg(feature = "async")]
use smol::future;

use abi_stable::{
    std_types::{RBox, ROption, RString, RVec},
    StableAbi,
};
use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use sarzak::lu_dog::ValueTypeEnum;
use uuid::Uuid;

#[cfg(feature = "async")]
use puteketeke::Executor;

#[cfg(feature = "async")]
use crate::ValueResult;

use crate::{
    chacha::error::Result,
    lu_dog::{Function, Lambda, ObjectStore as LuDogStore, ValueType, ZObjectStore},
    new_ref,
    plug_in::PluginType,
    s_read,
    sarzak::{ObjectStore as SarzakStore, Ty},
    ChaChaError, Context, DwarfFloat, DwarfInteger, NewRef, RefType,
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

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FfiProxy {
    pub module: RString,
    pub uuid: FfiUuid,
    pub id: FfiUuid,
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
    Option(ROption<RBox<Self>>),
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
/// There are three types of enumeration fields: Unit, Struct, and Tuple.
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
pub enum EnumVariant {
    /// Unit Enumeration Field
    ///
    /// This sort of enumeration is the simplest. In `Foo`, this refers to the
    /// field `Foo`: `Foo::Foo`. The final field in the path is stored as a
    /// string.
    ///
    /// The tuple is: (Type, TypeName, FieldValue)
    Unit(RefType<ValueType>, String, String),
    /// Struct Enumeration Field
    ///
    /// This type of field is for when it contains a struct, as `Baz` does above.
    /// That is to say, `Foo::Baz { qux: string }`. We store the final path element
    /// as a string, and the struct as a `RefType<UserStruct>`.
    Struct(RefType<UserStruct>),
    /// Tuple Enumeration Field
    ///
    /// This type of field is for when it contains a tuple, as `Bar` does above.
    /// That is to say, `Foo::Bar(int)`.
    ///
    /// The type is stored as the first element of the tuple, and the variant
    /// as the second.
    Tuple((RefType<ValueType>, String), RefType<TupleEnum>),
}

impl PartialEq for EnumVariant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit(_a, b, e), Self::Unit(_c, d, f)) => b == d && e == f,
            (Self::Struct(a), Self::Struct(b)) => *s_read!(a) == *s_read!(b),
            (Self::Tuple(a, c), Self::Tuple(b, d)) => {
                *s_read!(a.0) == *s_read!(b.0) && *s_read!(c) == *s_read!(d)
            }
            _ => false,
        }
    }
}

impl Eq for EnumVariant {}

impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unit(_, t, s) => write!(f, "{t}::{s}",),
            Self::Struct(s) => write!(f, "{}", s_read!(s)),
            Self::Tuple((_, t), e) => write!(f, "{t}::{}", s_read!(e)),
        }
    }
}

#[derive(Default)]
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
    Enumeration(EnumVariant),
    Error(Box<ChaChaError>),
    Float(DwarfFloat),
    /// Function
    ///
    /// 🚧 I really need to write something here describing, once and for all,
    /// why I need the inner Function to be behind a RefType<<T>>. It seems
    /// excessive, and yet I know I've looked into it before.
    Function(RefType<Function>),
    #[cfg(feature = "async")]
    Future {
        name: String,
        executor: Executor,
        task: Option<puteketeke::AsyncTask<'static, ValueResult>>,
    },
    Integer(DwarfInteger),
    Lambda(RefType<Lambda>),
    ParsedDwarf(Context),
    Plugin(RefType<PluginType>),
    ProxyType {
        module: String,
        obj_ty: Uuid,
        id: Uuid,
        plugin: RefType<PluginType>,
    },
    Range(Range<DwarfInteger>),
    Store(RefType<ZObjectStore>, RefType<PluginType>),
    String(String),
    Struct(RefType<UserStruct>),
    Table(HashMap<String, RefType<Self>>),
    #[cfg(feature = "async")]
    Task {
        worker: Option<puteketeke::Worker>,
        parent: Option<puteketeke::AsyncTask<'static, ValueResult>>,
    },
    Thonk(&'static str, usize),
    TupleEnum(RefType<TupleEnum>),
    Unknown,
    Uuid(uuid::Uuid),
    Vector {
        ty: RefType<ValueType>,
        inner: Vec<RefType<Self>>,
    },
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
            Self::Boolean(b) => write!(f, "{b:?}"),
            Self::Char(c) => write!(f, "{c:?}"),
            Self::Empty => write!(f, "()"),
            Self::Enumeration(var) => write!(f, "{var:?}"),
            Self::Error(e) => write!(f, "{}: {e}", Colour::Red.bold().paint("error")),
            Self::Float(num) => write!(f, "{num:?}"),
            Self::Function(func) => write!(f, "{:?}", s_read!(func)),
            #[cfg(feature = "async")]
            Self::Future {
                name,
                executor,
                task,
            } => write!(f, "Task `{name}`: {task:?}, executor: {executor:?}"),
            Self::Integer(num) => write!(f, "{num:?}"),
            Self::Lambda(ƛ) => write!(f, "{:?}", s_read!(ƛ)),
            Self::ParsedDwarf(ctx) => write!(f, "{ctx:?}"),
            Self::Plugin(plugin) => write!(f, "Plugin: {}", s_read!(plugin).name()),
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
            Self::Thonk(name, number) => write!(f, "{name:?} [{number:?}]"),
            Self::TupleEnum(te) => write!(f, "{:?}", s_read!(te)),
            Self::Unknown => write!(f, "<unknown>"),
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
            Self::Function(func) => Self::Function(func.clone()),
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
            Self::Thonk(name, number) => Self::Thonk(name, *number),
            Self::TupleEnum(te) => Self::TupleEnum(te.clone()),
            Self::Unknown => Self::Unknown,
            Self::Uuid(uuid) => Self::Uuid(*uuid),
            Self::Vector { ty, inner } => Self::Vector {
                ty: ty.clone(),
                inner: inner.clone(),
            },
        }
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
                uuid: obj_ty.to_owned().into(),
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
                obj_ty: plugin.uuid.into(),
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

impl Value {
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
            // Value::Enum(ref ut) => s_read!(ut).get_type().clone(),
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
                let ty = Ty::new_s_string(sarzak);
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
                let ty = Ty::new_s_uuid(sarzak);
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
                let ty = match &s_read!(ty).subtype {
                    ValueTypeEnum::XFuture(id) => {
                        let ty = lu_dog.exhume_x_future(id).unwrap();
                        let ty = s_read!(ty);
                        ty.r2_value_type(lu_dog)[0].clone()
                    }
                    _ => ty.clone(),
                };
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
            Self::Function(_) => write!(f, "<function>"),
            #[cfg(feature = "async")]
            Self::Future {
                name,
                executor,
                task,
            } => write!(f, "Task `{name}`: {task:?}, executor: {executor:?}"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::Lambda(_) => write!(f, "<lambda>"),
            Self::ParsedDwarf(ctx) => write!(f, "{ctx:#?}"),
            Self::Plugin(plugin) => write!(f, "Plugin: {}", s_read!(plugin).name()),
            Self::ProxyType {
                module: _,
                obj_ty: _,
                id: _,
                plugin,
            } => write!(f, "{}", s_read!(plugin)),
            Self::Range(range) => write!(f, "{range:?}"),
            // Self::StoreType(store) => write!(f, "{:?}", store),
            Self::Store(_store, plugin) => write!(f, "Plug-in ({})", s_read!(plugin).name()),
            Self::String(str_) => write!(f, "{str_}"),
            Self::Struct(ty) => write!(f, "{}", s_read!(ty)),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{table:?}"),
            #[cfg(feature = "async")]
            Self::Task { worker, parent } => write!(f, "Task: {parent:?} running on {worker:?}"),
            Self::Thonk(name, number) => write!(f, "{name} [{number}]"),
            Self::TupleEnum(te) => write!(f, "{}", s_read!(te)),
            Self::Unknown => write!(f, "<unknown>"),
            Self::Uuid(uuid) => write!(f, "{uuid}"),
            Self::Vector { ty: _, inner } => {
                let mut first_time = true;
                write!(f, "[")?;
                for i in inner {
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
                dst: "Uuid".to_owned(),
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
                dst: "Uuid".to_owned(),
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
            Value::Thonk(_, num) => Ok(num.to_owned()),
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
        match &value {
            Value::Float(num) => Ok(num.to_owned() as i64),
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
            (Value::String(a), Value::Float(b)) => Value::String(a.to_owned() + &b.to_string()),
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            // (Value::Integer(a), Value::String(b)) => Value::String(a.to_string() + &b),
            // (Value::String(a), Value::Integer(b)) => Value::String(a + &b.to_string()),
            (Value::String(a), Value::String(b)) => Value::String(a.to_owned() + b),
            (Value::Char(a), Value::Char(b)) => Value::String(a.to_string() + &b.to_string()),
            (Value::Char(a), Value::String(b)) => Value::String(a.to_string() + &b),
            (Value::String(a), Value::Char(b)) => Value::String(a.to_owned() + &b.to_string()),
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
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Struct(a), Value::Struct(b)) => *s_read!(a) == *s_read!(b),
            (Value::Uuid(a), Value::Uuid(b)) => a == b,
            (Value::Vector { ty: ty_a, inner: a }, Value::Vector { ty: ty_b, inner: b }) => {
                if *s_read!(ty_a) != *s_read!(ty_b) {
                    return false;
                }

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
pub struct TupleEnum {
    variant: String,
    value: RefType<Value>,
}

impl PartialEq for TupleEnum {
    fn eq(&self, other: &Self) -> bool {
        self.variant == other.variant && s_read!(self.value).eq(&s_read!(other.value))
    }
}

impl Eq for TupleEnum {}

impl TupleEnum {
    pub fn new<S: AsRef<str>>(variant_name: S, value: RefType<Value>) -> Self {
        Self {
            variant: variant_name.as_ref().to_owned(),
            value,
        }
    }

    pub fn value(&self) -> RefType<Value> {
        self.value.clone()
    }

    pub fn variant(&self) -> &str {
        &self.variant
    }
}

impl fmt::Display for TupleEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.variant(), s_read!(self.value))
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

    pub fn type_name(&self) -> &str {
        &self.type_name
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
