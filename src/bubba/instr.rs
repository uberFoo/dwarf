use std::fmt;

use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;

use crate::{s_read, RefType, Value, ValueType};

#[derive(Clone, Debug)]
pub enum Instruction {
    /// Add the top two values on the stack.
    Add,
    /// Call a function with the given arity.
    ///
    /// ## Calling Convention
    ///
    /// 🚧 Draw a picture of the stack and all that.
    ///
    /// ### Here's a functional description of what to do.
    ///
    /// Push a Value onto the stack that contains a Thonk.
    /// Push
    Call(usize),
    /// Duplicate the top of the stack.
    Dup,
    /// Fetch a local variable.
    ///
    /// The parameter is it's distance from the frame pointer, or the index of
    /// the local variable.
    ///
    /// The value of the local variable is pushed onto the stack.
    PushLocal(usize),
    /// Read a field value
    ///
    /// The top of the stack is the name of the field to read. The second value
    /// on the stack is the object from which to read.
    ///
    /// The value read is left on top of the stack.
    FieldRead,
    /// Read several field values
    ///
    /// The first `n` entries of the stack are the names of the fields to read.
    /// The next value on the stack is the object from which to read.
    ///
    /// The values read are left on top of the stack, in the same order as their
    /// field names were on the stack.
    FieldsRead(usize),
    /// Write a field value
    ///
    /// The top of the stack is the value to write. The second value on the
    /// stack is the filed name, and the third value on the stack is the object
    /// to which to write.
    FieldWrite,
    /// Jump to the given offset if the top of the stack is false.
    JumpIfFalse(usize),
    /// Compare the top two values on the stack.
    TestLessThanOrEqual,
    /// Multiply the top two values on the stack.
    Mul,
    /// New UserType
    ///
    /// The first element of the tuple is the name of the user type. The second
    /// is the type ([ValueType]) of the user type. The third is the number of
    /// fields in the user type.
    /// There is a `([String], [ValueType], [Value])`tuple on the stack for each
    /// field.
    /// // 🚧 move these parameters to the stack
    NewUserType(String, RefType<ValueType>, usize),
    /// Write a Value
    ///
    /// This function writes some value to an output stream. The top of the stack
    /// contains the stream number to write to. Currently 0 is stdout, and 1 is
    /// stderr.
    ///
    /// The next value on the stack is the value itself to write.
    Out(usize),
    /// Pop the top value off the stack and store it in a local variable at the
    /// given index.
    PopLocal(usize),
    /// Push a value onto the stack.
    Push(RefType<Value>),
    /// Exit the function
    ///
    /// The value expressed by this instruction is the value at the top of the
    /// stack.
    Return,
    /// Subtract the top two values on the stack.
    Subtract,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let opcode_style = Colour::Blue.italic();
        let operand_style = Colour::Yellow.bold();

        match self {
            Instruction::Add => write!(f, "{}", opcode_style.paint("add")),
            Instruction::Call(arity) => write!(
                f,
                "{} {}",
                opcode_style.paint("call"),
                operand_style.paint(arity.to_string())
            ),
            Instruction::Dup => write!(f, "{}", opcode_style.paint("dup")),
            Instruction::PushLocal(index) => write!(
                f,
                "{} {}",
                opcode_style.paint("push_local"),
                operand_style.paint(index.to_string())
            ),
            Instruction::FieldRead => write!(f, "{}", opcode_style.paint("field_read")),
            Instruction::FieldsRead(count) => write!(
                f,
                "{} {}",
                opcode_style.paint("fields_read"),
                operand_style.paint(count.to_string())
            ),
            Instruction::FieldWrite => write!(f, "{}", opcode_style.paint("field_write")),
            Instruction::JumpIfFalse(offset) => write!(
                f,
                "{} {}",
                opcode_style.paint("jif"),
                operand_style.paint(offset.to_string())
            ),
            Instruction::TestLessThanOrEqual => write!(f, "{}", opcode_style.paint("lte")),
            Instruction::Mul => write!(f, "{}", opcode_style.paint("mul")),
            Instruction::NewUserType(name, _ty, n) => {
                write!(f, "{}{name}({n})", opcode_style.paint("new"))
            }
            Instruction::Out(stream) => write!(
                f,
                "{} {}",
                opcode_style.paint("out "),
                operand_style.paint(stream.to_string())
            ),
            Instruction::PopLocal(index) => write!(
                f,
                "{} {}",
                opcode_style.paint("pop_local"),
                operand_style.paint(index.to_string())
            ),
            Instruction::Push(value) => write!(
                f,
                "{} {}",
                opcode_style.paint("push"),
                operand_style.paint(s_read!(value).to_string())
            ),
            Instruction::Return => write!(f, "{}", opcode_style.paint("ret")),
            Instruction::Subtract => write!(f, "{}", opcode_style.paint("sub")),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    compiler_version: String,
    compiler_build_ts: String,
    thonks: HashMap<String, Thonk>,
}

impl Program {
    pub(crate) fn new(compiler_version: String, build_time: String) -> Self {
        Program {
            compiler_version,
            compiler_build_ts: build_time,
            thonks: HashMap::default(),
        }
    }

    pub(crate) fn add_thonk(&mut self, thonk: Thonk) {
        self.thonks.insert(thonk.name.clone(), thonk);
    }

    pub(crate) fn get_thonk(&self, name: &str) -> Option<&Thonk> {
        self.thonks.get(name)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Thonk> {
        self.thonks.values()
    }

    pub(crate) fn get_thonk_card(&self) -> usize {
        self.thonks.len()
    }

    pub fn compiler_version(&self) -> &str {
        &self.compiler_version
    }

    pub fn compiler_build_ts(&self) -> &str {
        &self.compiler_build_ts
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (name, thonk) in self.thonks.iter() {
            writeln!(f, "{name}:\n{thonk}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Thonk {
    pub(crate) name: String,
    instructions: Vec<Instruction>,
}

impl Thonk {
    pub(crate) fn new(name: String) -> Self {
        Thonk {
            name,
            instructions: Vec::new(),
        }
    }

    pub(crate) fn add_instruction(&mut self, instr: Instruction) -> usize {
        self.instructions.push(instr);
        self.instructions.len() - 1
    }

    pub(crate) fn get_instruction(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }

    pub(crate) fn get_instruction_card(&self) -> usize {
        self.instructions.len()
    }
}

impl fmt::Display for Thonk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.instructions.iter().enumerate() {
            writeln!(f, "{i:08x}:\t {instr}")?;
        }
        Ok(())
    }
}
