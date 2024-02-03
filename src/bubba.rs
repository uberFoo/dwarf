pub mod compiler;
pub mod instr;
pub mod vm;

pub use instr::Instruction;
pub use instr::Program;
pub(crate) use instr::Thonk;
pub use vm::VM;

pub(crate) const STRING: &str = "STRING";
pub(crate) const RESULT: &str = "RESULT";
