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
pub(crate) const INT: &str = "INT";
pub(crate) const RANGE: &str = "RANGE";
pub(crate) const RESULT: &str = "RESULT";
pub(crate) const STRING: &str = "STRING";
pub(crate) const STRING_ARRAY: &str = "STRING_ARRAY";
pub(crate) const UNKNOWN: &str = "UNKNOWN";
pub(crate) const UUID: &str = "UUID";
