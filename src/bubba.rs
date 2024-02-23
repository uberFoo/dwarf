pub mod compiler;
pub mod instr;
pub mod value;
pub mod vm;

pub use instr::Instruction;
pub use instr::Program;
pub use vm::{Error, VM};

pub(crate) const BOOL: &str = "BOOL";
pub(crate) const EMPTY: &str = "EMPTY";
pub(crate) const INT: &str = "INT";
pub(crate) const RANGE: &str = "RANGE";
pub(crate) const RESULT: &str = "RESULT";
pub(crate) const STRING: &str = "STRING";
pub(crate) const STRING_ARRAY: &str = "STRING_ARRAY";
pub(crate) const UNKNOWN: &str = "UNKNOWN";
