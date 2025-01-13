pub mod compiler;
pub mod error;
pub mod instr;
pub mod value;
pub mod vm;

pub use error::Error;
pub use instr::Instruction;
pub use instr::Program;
pub use vm::VM;

pub(crate) const BOOL: &str = "BOOL";
pub(crate) const CHAR: &str = "CHAR";
pub(crate) const EMPTY: &str = "EMPTY";
pub(crate) const FLOAT: &str = "FLOAT";
pub(crate) const INTEGER: &str = "INTEGER";
pub(crate) const RANGE: &str = "RANGE";
pub(crate) const MAP: &str = "MAP";
pub(crate) const STRING: &str = "STRING";
pub(crate) const STRING_ARRAY: &str = "STRING_ARRAY";
pub(crate) const UNKNOWN: &str = "UNKNOWN";
pub(crate) const UUID: &str = "UUID";

pub type RefType<T> = std::sync::Arc<std::sync::RwLock<T>>;

impl<T> NewRef<T> for RefType<T> {
    fn new_ref(value: T) -> RefType<T> {
        std::sync::Arc::new(std::sync::RwLock::new(value))
    }
}

macro_rules! ref_read {
    ($arg:expr) => {
        $arg.read().unwrap()
    };
}

macro_rules! ref_try_read {
    ($arg:expr) => {
        $arg.try_read()
    };
}

macro_rules! ref_write {
    ($arg:expr) => {
        $arg.write().unwrap()
    };
}

macro_rules! ref_to_inner {
    ($arg:expr) => {
        $arg.into_inner().unwrap()
    };
}

macro_rules! new_ref_macro {
    ($type:ty, $value:expr) => {
        <RefType<$type> as crate::bubba::NewRef<$type>>::new_ref($value)
    };
}

pub trait NewRef<T> {
    fn new_ref(value: T) -> RefType<T>;
}

pub(crate) use new_ref_macro as new_ref;
pub(crate) use ref_read as s_read;
pub(crate) use ref_try_read as s_try_read;
pub(crate) use ref_write as s_write;
