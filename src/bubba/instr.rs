use std::fmt;

use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};

use crate::{plug_in::PluginType, s_read, RefType, Span, Value};

#[derive(Clone, Debug)]
pub enum Instruction {
    /// Add the top two values on the stack.
    ///
    /// ## Stack Effect
    ///
    /// The instruction will pop two values from the stack, and push one.
    /// Therefore the stack will be one element shorter after this instruction.
    Add,
    /// Operator And
    ///
    /// Take the first two values off the stack and perform a logical and on them.
    /// The result is pushed onto the stack.
    ///
    /// ## Stack Effect
    ///
    /// The stack is one element shorter after this instruction.
    And,
    /// Call a function with the given arity.
    ///
    /// ## Calling Convention
    ///
    /// 🚧 Draw a picture of the stack and all that.
    ///
    /// ### Here's a functional description of what to do.
    ///
    /// - push a Value::Thonk onto the stack -- this is the call destination
    /// - push the number of locals on the stack
    /// - push the arguments onto the stack in declaration order
    /// - push Instruction::Call(arity) onto the stack
    ///
    ///  ## Stack Effect
    ///
    Call(usize),
    /// Call Destination
    ///
    /// This is a pseudo-instruction that stores the name of the function that
    /// we are calling. It is patched by the VM before execution.
    ///
    CallDestination(RefType<String>),
    CaptureLocal(usize, usize),
    /// Comment Instruction / NOP
    ///
    /// I don't like this because it increases the size of the instruction by 50%
    /// -- from 16 bytes to 24.
    ///
    Comment(RefType<String>),
    /// Deconstruct a struct expression
    ///
    /// Given a struct expression, like Foo::Bar(x, y), this instruction will pop the
    /// top value off the stack, and push the tuple elements onto the stack.
    ///
    /// ## Stack Effect
    ///
    /// The stack is n - 1 elements longer, where n is the path length.
    ///
    DeconstructStructExpression,
    /// Divide the top two values on the stack.
    ///
    Divide,
    /// Duplicate the top of the stack.
    ///
    /// ## Stack Effect
    ///
    /// The instruction will increase the stack depth by one.
    Dup,
    ExtractEnumValue,
    /// Fetch a local variable.
    ///
    /// The parameter is it's distance from the frame pointer, or the index of
    /// the local variable.
    ///
    /// The value of the local variable is pushed onto the stack.
    ///
    /// ## Stack Effect
    ///
    FetchLocal(usize),
    /// Read a field value
    ///
    /// The top of the stack is the name of the field to read. The second value
    /// on the stack is the object from which to read.
    ///
    /// The value read is left on top of the stack.
    ///
    /// ## Stack Effect
    ///
    FieldRead,
    /// Read several field values
    ///
    /// The first `n` entries of the stack are the names of the fields to read.
    /// The next value on the stack is the object from which to read.
    ///
    /// The values read are left on top of the stack, in the same order as their
    /// field names were on the stack.
    ///
    /// ## Stack Effect
    ///
    FieldsRead(usize),
    /// Write a field value
    ///
    /// The top of the stack is the value to write. The second value on the
    /// stack is the field name, and the third value on the stack is the object
    /// to which to write.
    ///
    /// ## Stack Effect
    ///
    FieldWrite,
    /// Stop processing and panic the VM
    ///
    /// ## Stack Effect
    ///
    /// conflagration
    ///
    HaltAndCatchFire,
    /// Jump to the given offset.
    ///
    /// ## Stack Effect
    ///
    /// None
    ///
    Jump(isize),
    /// Jump to the given offset if the top of the stack is false.
    ///
    /// ## Stack Effect
    ///
    /// Pops the value off the top of the stack for the condition.
    ///
    JumpIfFalse(isize),
    /// Jump to the given address if the top of the stack is true.
    ///
    /// ## Stack Effect
    ///
    /// Pops the value off the top of the stack for the condition.
    ///
    JumpIfTrue(isize),
    /// Index into a list
    ///
    /// The top of the stack is the index to read. The second value on the stack
    /// is the list to read from. The result is pushed onto the stack.
    ///
    /// ## Stack Effect
    ///
    /// The stack is one element shorter after this instruction.
    ///
    ListIndex,
    ListLength,
    /// Local Cardinality
    ///
    /// This is a pseudo-instruction to store the number of local variables in
    /// the function. It is patched by the VM before execution.
    ///
    LocalCardinality(RefType<String>),
    MakeLambdaPointer(RefType<String>, usize),
    /// Look up a method
    ///
    /// The top of the stack is a reference to the user defined type upon which
    /// we are performing the lookup. The operand is the method name we are seeking.
    ///
    /// The UDT is removed from the stack and the address of the method is pushed
    /// as well as the number of locals.
    ///
    /// ## Stack Effect
    ///
    /// The stack is one element longer.
    ///
    MethodLookup(RefType<String>),
    /// Multiply the top two values on the stack.
    ///
    /// ## Stack Effect
    ///
    Multiply,
    /// New List
    ///
    /// Create a new list from values on the stack.
    ///
    /// The operand is the number of elements in the list. We'll call that `n`.
    /// The first element in the stack is the type of the list. The next `n`
    /// entries in the stack are the list elements. The list is pushed onto the
    /// stack.
    ///
    /// ## Stack Effect
    ///
    /// `n` + 1 elements are removed from the stack, and a single element is
    /// pushed.
    ///
    NewList(usize),
    /// New Tuple Enum
    ///
    /// The first operand, `n` is the number of tuple fields. It is expected that the
    /// tuple name be the first element on the stack. Next is the enum path. Third
    /// up is the ValueType. The tuple fields are last, according to the first
    /// operand.
    ///
    /// A Value::Enumeration is pushed onto the stack.
    ///
    /// ## Stack Effect
    ///
    /// The stack is `n` + 2? elements shorter after this instruction.
    ///
    NewTupleEnum(usize),
    /// New UserType
    ///
    /// The first operand is the number of fields in the struct. Let's call this
    /// n. The stack shall then contain, in order, the name of the struct, the
    /// ValueType of the struct. For each struct field there shall be a  `[String]`
    /// as the name of the filed, a `[ValueType]` and finally a `[Value]`.
    ///
    /// The new type is pushed onto the stack.
    ///
    /// ## Stack Effect
    ///
    /// The stack is (`n` * 3) + 2? elements shorter after this instruction.
    ///
    NewUserType(usize),
    /// Operator Not
    ///
    /// Take the top value off the stack and perform a logical not on it.
    /// The result is pushed onto the stack.
    ///
    /// ## Stack Effect
    ///
    /// The stack is unchanged after this instruction.
    ///
    Not,
    /// Operator Or
    ///
    /// Take the first two values off the stack and perform a logical or on them.
    /// The result is pushed onto the stack.
    ///
    /// ## Stack Effect
    ///
    /// The stack is one element shorter after this instruction.
    Or,
    /// Write a Value
    ///
    /// This function writes some value to an output stream. The top of the stack
    /// contains the stream number to write to. Currently 0 is stdout, and 1 is
    /// stderr.
    ///
    /// The next value on the stack is the value itself to write.
    ///
    /// ## Stack Effect
    ///
    Out(usize),
    PluginNew(usize),
    /// Pop the value off the top of the stack.
    ///
    /// The value is dropped on the floor and forgotten.
    ///
    /// ## Stack Effect
    ///
    /// The stack is one element shorter after this instruction.
    Pop,
    /// Push a value onto the stack.
    ///
    /// ## Stack Effect
    ///
    Push(RefType<Value>),
    /// Push the arguments to the program onto the stack
    ///
    /// ## Stack Effect
    ///
    /// The stack will be n elements longer, where n is the cardinality of the
    /// arguments.
    PushArgs,
    ///
    /// Pop the top value off the stack and store it in a local variable at the
    /// given index.
    ///
    /// ## Stack Effect
    ///
    StoreLocal(usize),
    /// Compare the top two values on the stack.
    ///
    /// a == b
    ///
    /// Where b is the top of the stack, and a is the next element.
    ///
    /// ## Stack Effect
    ///
    /// The top two values are consumed and compared, with a boolean value
    /// pushed as the result.
    ///
    /// Net effect -1.
    ///
    TestEq,
    /// Greater Than Operator (>)
    ///
    /// ## Stack Effect
    ///
    TestGreaterThan,
    /// Compare the top two values on the stack.
    ///
    /// a < b
    ///
    /// Where b is the top of the stack, and a is the next element.
    ///
    /// ## Stack Effect
    ///
    /// The top two values are consumed and compared, with a boolean value
    /// pushed as the result.
    ///
    /// Net effect -1.
    ///
    TestLessThan,
    /// Compare the top two values on the stack.
    ///
    /// a <= b
    ///
    /// Where b is the top of the stack, and a is the next element.
    ///
    /// ## Stack Effect
    ///
    /// The top two values are consumed and compared, with a boolean value
    /// pushed as the result.
    ///
    /// Net effect -1.
    ///
    TestLessThanOrEqual,
    /// Typecast
    ///
    Typecast(RefType<Value>),
    /// Exit the function
    ///
    /// The value expressed by this instruction is the value at the top of the
    /// stack.
    ///
    /// ## Stack Effect
    ///
    Return,
    /// Subtract the top two values on the stack.
    ///
    /// ## Stack Effect
    ///
    Subtract,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let opcode_style = Colour::Blue.italic();
        let operand_style = Colour::Yellow.bold();

        match self {
            Instruction::Add => write!(f, "{}", opcode_style.paint("add ")),
            Instruction::And => write!(f, "{}", opcode_style.paint("and ")),
            Instruction::Call(arity) => write!(
                f,
                "{} {}",
                opcode_style.paint("call"),
                operand_style.paint(arity.to_string())
            ),
            Instruction::CallDestination(name) => write!(
                f,
                "{} {}",
                opcode_style.paint("calld"),
                operand_style.paint(s_read!(name).to_string())
            ),
            Instruction::CaptureLocal(index, distance) => write!(
                f,
                "{} {} {}",
                opcode_style.paint("capl"),
                operand_style.paint(index.to_string()),
                operand_style.paint(distance.to_string())
            ),
            Instruction::Comment(comment) => write!(
                f,
                "{} {}",
                opcode_style.paint("nop "),
                operand_style.paint(s_read!(comment).to_string())
            ),
            Instruction::DeconstructStructExpression => write!(f, "{}", opcode_style.paint("dse ")),
            Instruction::Divide => write!(f, "{}", opcode_style.paint("div ")),
            Instruction::Dup => write!(f, "{}", opcode_style.paint("dup ")),
            Instruction::ExtractEnumValue => write!(f, "{}", opcode_style.paint("eev ")),
            Instruction::FetchLocal(index) => write!(
                f,
                "{} {}",
                opcode_style.paint("fetch"),
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
            Instruction::HaltAndCatchFire => write!(f, "{}", opcode_style.paint("hcf 🔥")),
            Instruction::Jump(offset) => write!(
                f,
                "{} {}",
                opcode_style.paint("jump"),
                operand_style.paint(offset.to_string())
            ),
            Instruction::JumpIfFalse(offset) => write!(
                f,
                "{} {}",
                opcode_style.paint("jiff"),
                operand_style.paint(offset.to_string())
            ),
            Instruction::JumpIfTrue(address) => write!(
                f,
                "{} {}",
                opcode_style.paint("jift"),
                operand_style.paint(address.to_string())
            ),
            Instruction::ListIndex => write!(f, "{}", opcode_style.paint("idx ")),
            Instruction::ListLength => write!(f, "{}", opcode_style.paint("len ")),
            Instruction::LocalCardinality(name) => write!(
                f,
                "{} {}",
                opcode_style.paint("lc  "),
                operand_style.paint(s_read!(name).to_string())
            ),
            Instruction::MakeLambdaPointer(name, arity) => write!(
                f,
                "{} {} {}",
                opcode_style.paint("mlp "),
                operand_style.paint(s_read!(name).to_string()),
                operand_style.paint(arity.to_string())
            ),
            Instruction::MethodLookup(name) => write!(
                f,
                "{} {}",
                opcode_style.paint("mlu "),
                operand_style.paint(s_read!(name).to_string())
            ),
            Instruction::Multiply => write!(f, "{}", opcode_style.paint("mul ")),
            Instruction::NewList(n) => write!(
                f,
                "{} {}",
                opcode_style.paint("nl  "),
                operand_style.paint(n.to_string())
            ),
            Instruction::NewTupleEnum(n) => write!(
                f,
                "{} {}",
                opcode_style.paint("nte "),
                operand_style.paint(n.to_string())
            ),
            Instruction::NewUserType(n) => write!(
                f,
                "{} {}",
                opcode_style.paint("nut "),
                operand_style.paint(n.to_string())
            ),
            Instruction::Not => write!(f, "{}", opcode_style.paint("not ")),
            Instruction::Or => write!(f, "{}", opcode_style.paint("or  ")),
            Instruction::Out(stream) => write!(
                f,
                "{} {}",
                opcode_style.paint("out "),
                operand_style.paint(stream.to_string())
            ),
            Instruction::PluginNew(arg_count) => write!(
                f,
                "{} {}",
                opcode_style.paint("pnew"),
                operand_style.paint(arg_count.to_string())
            ),
            Instruction::Pop => write!(f, "{}", opcode_style.paint("pop ")),
            Instruction::Push(value) => write!(
                f,
                "{} {}",
                opcode_style.paint("push"),
                operand_style.paint(s_read!(value).to_string())
            ),
            Instruction::PushArgs => write!(f, "{}", opcode_style.paint("parg")),
            Instruction::Return => write!(f, "{}", opcode_style.paint("ret ")),
            Instruction::StoreLocal(index) => write!(
                f,
                "{} {}",
                opcode_style.paint("store"),
                operand_style.paint(index.to_string())
            ),
            Instruction::Subtract => write!(f, "{}", opcode_style.paint("sub ")),
            Instruction::TestEq => write!(f, "{}", opcode_style.paint("eq  ")),
            Instruction::TestGreaterThan => write!(f, "{}", opcode_style.paint("gt  ")),
            Instruction::TestLessThan => write!(f, "{}", opcode_style.paint("lt  ")),
            Instruction::TestLessThanOrEqual => write!(f, "{}", opcode_style.paint("lte ")),
            Instruction::Typecast(name) => write!(
                f,
                "{} {}",
                opcode_style.paint("tc  "),
                operand_style.paint(s_read!(name).to_string())
            ),
        }
    }
}

#[derive(Clone)]
pub struct Program {
    compiler_version: String,
    compiler_build_ts: String,
    // libs: HashMap<String, PluginType>,
    symbols: HashMap<String, Value>,
    thonks: HashMap<String, Thonk>,
}

impl Program {
    pub(crate) fn new(compiler_version: String, build_time: String) -> Self {
        Program {
            compiler_version,
            compiler_build_ts: build_time,
            symbols: HashMap::default(),
            thonks: HashMap::default(),
        }
    }

    pub(crate) fn add_symbol(&mut self, name: String, value: Value) {
        self.symbols.insert(name, value);
    }

    pub(crate) fn get_symbol(&self, name: &str) -> Option<&Value> {
        self.symbols.get(name)
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

    pub(crate) fn get_instruction_count(&self) -> usize {
        self.thonks.values().map(|t| t.get_instruction_card()).sum()
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Symbols:")?;
        for (name, value) in self.symbols.iter() {
            writeln!(f, "{}: {}", name, value)?;
        }
        writeln!(f, "Thonks:")?;
        for (name, thonk) in self.thonks.iter() {
            writeln!(f, "{}: {:?}", name, thonk)?;
        }
        Ok(())
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut offset = 0;
        for (name, thonk) in self.thonks.iter() {
            writeln!(f, "{name} ({}):", thonk.frame_size())?;
            thonk.print_in_program(offset, f)?;
            offset += thonk.get_instruction_card();
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Thonk {
    name: String,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) spans: Vec<Span>,
    frame_size: usize,
}

impl Thonk {
    pub(crate) fn new(name: String) -> Self {
        Thonk {
            name,
            instructions: Vec::new(),
            spans: Vec::new(),
            frame_size: 0,
        }
    }

    pub(crate) fn add_instruction(&mut self, instr: Instruction, span: Option<Span>) -> usize {
        self.instructions.push(instr);
        self.spans.push(span.unwrap_or_default());
        self.instructions.len() - 1
    }

    pub(crate) fn prefix_instruction(&mut self, instr: Instruction, span: Option<Span>) -> usize {
        self.instructions.insert(0, instr);
        self.spans.insert(0, span.unwrap_or_default());
        self.instructions.len() - 1
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn get_instruction_card(&self) -> usize {
        self.instructions.len()
    }

    pub(crate) fn increment_frame_size(&mut self) {
        self.frame_size += 1;
    }

    #[inline]
    pub(crate) fn frame_size(&self) -> usize {
        self.frame_size
    }

    fn print_in_program(&self, start_addr: usize, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.instructions.iter().enumerate() {
            writeln!(f, "{:08x}:\t {instr}", start_addr + i)?;
        }
        Ok(())
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
