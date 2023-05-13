use std::fmt;

use ansi_term::Colour;

use crate::{ChaChaError, Memory, Result, Value};

// pub mod compiler;

#[derive(Clone, Debug)]
pub enum Instruction {
    Add,
    Call(usize),
    Constant(Value),
    FetchLocal(usize),
    JumpIfFalse(usize),
    LessThanOrEqual,
    Pop,
    Push,
    Return,
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
            Instruction::Constant(value) => write!(
                f,
                "{} {}",
                opcode_style.paint("const"),
                operand_style.paint(value.to_string())
            ),
            Instruction::FetchLocal(index) => write!(
                f,
                "{} {}",
                opcode_style.paint("fetch"),
                operand_style.paint(index.to_string())
            ),
            Instruction::JumpIfFalse(offset) => write!(
                f,
                "{} {}",
                opcode_style.paint("jif"),
                operand_style.paint(offset.to_string())
            ),
            Instruction::LessThanOrEqual => write!(f, "{}", opcode_style.paint("lte")),
            Instruction::Pop => write!(f, "{}", opcode_style.paint("pop")),
            Instruction::Push => write!(f, "{}", opcode_style.paint("push")),
            Instruction::Return => write!(f, "{}", opcode_style.paint("ret")),
            Instruction::Subtract => write!(f, "{}", opcode_style.paint("sub")),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Chunk {
    pub(crate) name: String,
    variables: Vec<String>,
    instructions: Vec<Instruction>,
}

impl Chunk {
    pub(crate) fn new(name: String) -> Self {
        Chunk {
            name,
            variables: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub(crate) fn add_variable(&mut self, name: String) -> usize {
        self.variables.push(name);
        self.variables.len() - 1
    }

    pub(crate) fn add_instruction(&mut self, instr: Instruction) -> usize {
        self.instructions.push(instr);
        self.instructions.len() - 1
    }

    pub(crate) fn get_instruction(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.instructions.iter().enumerate() {
            writeln!(f, "{:08x}:\t {}", i, instr)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct CallFrame<'a> {
    ip: usize,
    fp: usize,
    chunk: &'a Chunk,
}

impl<'a> CallFrame<'a> {
    pub(crate) fn new(ip: usize, fp: usize, chunk: &'a Chunk) -> Self {
        CallFrame { ip, fp, chunk }
    }

    fn load_instruction(&mut self) -> Option<&Instruction> {
        let instr = self.chunk.get_instruction(self.ip);
        if instr.is_some() {
            self.ip += 1;
        }

        instr
    }
}

impl<'a> fmt::Display for CallFrame<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ip: {}, fp: {}", self.ip, self.fp)
    }
}

#[derive(Debug)]
pub(crate) struct VM<'a, 'b: 'a> {
    frames: Vec<CallFrame<'a>>,
    stack: Vec<Value>,
    memory: &'b Memory,
}

impl<'a, 'b> VM<'a, 'b> {
    pub(crate) fn new(memory: &'b Memory) -> Self {
        VM {
            frames: Vec::with_capacity(10 * 1024),
            stack: Vec::with_capacity(10 * 1024 * 1024),
            memory,
        }
    }

    pub(crate) fn push_frame(&mut self, frame: CallFrame<'a>) {
        self.frames.push(frame);
    }

    pub(crate) fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub(crate) fn run(&mut self, trace: bool) -> Result<Value> {
        if let Some(mut frame) = self.frames.pop() {
            loop {
                let fp = frame.fp;
                let ip = frame.ip;
                let instr = frame.load_instruction();
                let ip_offset = if let Some(instr) = instr {
                    // let instr = instr.clone();

                    if trace {
                        let len = self.stack.len();
                        for i in 0..len {
                            if i == fp {
                                print!("\t{} ->\t", Colour::Green.bold().paint("fp"));
                            } else {
                                print!("\t\t");
                            }
                            println!("stack {}:\t{}", len - i - 1, self.stack[i]);
                        }
                        println!("");
                        println!("{:08x}:\t{}", ip, instr);
                    }
                    match instr {
                        Instruction::Add => {
                            let b = self.stack.pop().unwrap();
                            let a = self.stack.pop().unwrap();
                            // let b = pop!(self.stack);
                            // let a = pop!(self.stack);
                            let c = a + b;
                            if let Value::Error(e) = c {
                                return Err(ChaChaError::VmPanic { message: e });
                            }
                            self.stack.push(c);

                            0
                        }
                        Instruction::Call(arity) => {
                            let callee = &self.stack[self.stack.len() - arity - 1];
                            if trace {
                                println!("\t\t{}:\t{}", Colour::Green.paint("func:"), callee);
                            }
                            let callee: usize = match callee.try_into() {
                                Ok(callee) => callee,
                                Err(e) => {
                                    return Err::<Value, ChaChaError>(ChaChaError::VmPanic {
                                        message: format!("{}: {}", callee, e),
                                    });
                                }
                            };
                            let chunk = self.memory.get_chunk(callee).unwrap();
                            let frame = CallFrame::new(0, self.stack.len() - arity - 1, chunk);

                            if trace {
                                println!("\t\t{}\t{}", Colour::Green.paint("frame:"), frame);
                            }

                            let fp = frame.fp;
                            self.frames.push(frame);

                            let result = match self.run(trace) {
                                Ok(result) => result,
                                Err(e) => {
                                    return Err::<Value, ChaChaError>(ChaChaError::VmPanic {
                                        message: format!("{}: {}", callee, e),
                                    });
                                }
                            };

                            (fp..self.stack.len()).for_each(|_| {
                                self.stack.pop();
                            });

                            self.stack.push(result);

                            0
                        }
                        Instruction::Constant(value) => {
                            self.stack.push(value.clone());

                            0
                        }
                        Instruction::FetchLocal(index) => {
                            let value = self.stack[fp + index + 1].clone();
                            self.stack.push(value);

                            0
                        }
                        Instruction::JumpIfFalse(offset) => {
                            let condition = self.stack.pop().unwrap();
                            let condition: bool = condition
                                .try_into()
                                .map_err(|e| {
                                    return ChaChaError::VmPanic {
                                        message: format!("{}", e),
                                    };
                                })
                                .unwrap();

                            if !condition {
                                if trace {
                                    println!(
                                        "\t\t{} {}",
                                        Colour::Red.bold().paint("jmp"),
                                        Colour::Yellow.bold().paint(format!("{}", ip + offset + 1))
                                    );
                                }
                                *offset
                            } else {
                                0
                            }
                        }
                        Instruction::LessThanOrEqual => {
                            let b = self.stack.pop().unwrap();
                            let a = self.stack.pop().unwrap();
                            self.stack.push(Value::Boolean(a.lte(&b)));

                            0
                        }
                        Instruction::Return => {
                            return Ok(self.stack.pop().unwrap());
                        }
                        Instruction::Subtract => {
                            let b = self.stack.pop().unwrap();
                            let a = self.stack.pop().unwrap();
                            let c = a - b;
                            if let Value::Error(e) = c {
                                return Err(ChaChaError::VmPanic { message: e });
                            }

                            self.stack.push(c);

                            0
                        }
                        invalid => {
                            return Err(ChaChaError::InvalidInstruction {
                                instr: invalid.clone(),
                            })
                        }
                    }
                } else {
                    return Err(ChaChaError::VmPanic {
                        message: "ip out of bounds".to_string(),
                    });
                };

                frame.ip += ip_offset;
            }
        } else {
            Err(ChaChaError::VmPanic {
                message: "no frames".to_string(),
            })
        }
    }
}

mod tests {
    use super::*;
    use crate::DwarfInteger;

    #[test]
    fn test_instr_constant() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(42)));
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(result.is_err());

        let tos = vm.stack.pop().unwrap();
        let as_int: DwarfInteger = tos.try_into().unwrap();
        assert_eq!(as_int, 42);

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 1);
    }

    #[test]
    fn test_instr_return() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(42)));
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = result.unwrap().try_into().unwrap();
        assert_eq!(as_int, 42);

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 2);
    }

    #[test]
    fn test_instr_add() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(42)));
        chunk.add_instruction(Instruction::Constant(Value::Integer(69)));
        chunk.add_instruction(Instruction::Add);
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = result.unwrap().try_into().unwrap();
        assert_eq!(as_int, 111);

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 4);
    }

    #[test]
    fn test_instr_subtract() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(111)));
        chunk.add_instruction(Instruction::Constant(Value::Integer(69)));
        chunk.add_instruction(Instruction::Subtract);
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = result.unwrap().try_into().unwrap();
        assert_eq!(as_int, 42);

        // assert_eq!(frame.ip, 4);
    }

    #[test]
    fn test_instr_less_than_or_equal() {
        // False Case
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(111)));
        chunk.add_instruction(Instruction::Constant(Value::Integer(69)));
        chunk.add_instruction(Instruction::LessThanOrEqual);
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = result.unwrap().try_into().unwrap();
        assert_eq!(as_bool, false);

        // assert_eq!(frame.ip, 4);

        // True case: less than
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(42)));
        chunk.add_instruction(Instruction::Constant(Value::Integer(69)));
        chunk.add_instruction(Instruction::LessThanOrEqual);
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = result.unwrap().try_into().unwrap();
        assert_eq!(as_bool, true);

        // assert_eq!(frame.ip, 4);

        // True case: equal
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(42)));
        chunk.add_instruction(Instruction::Constant(Value::Integer(42)));
        chunk.add_instruction(Instruction::LessThanOrEqual);
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = result.unwrap().try_into().unwrap();
        assert_eq!(as_bool, true);

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 4);
    }

    #[test]
    fn test_instr_jump_if_false() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        chunk.add_instruction(Instruction::Constant(Value::Integer(69)));
        chunk.add_instruction(Instruction::Constant(Value::Integer(42)));
        chunk.add_instruction(Instruction::LessThanOrEqual);
        chunk.add_instruction(Instruction::JumpIfFalse(2));
        chunk.add_instruction(Instruction::Constant(Value::String(
            "epic fail!".to_string(),
        )));
        chunk.add_instruction(Instruction::Return);
        chunk.add_instruction(Instruction::Constant(Value::String(
            "you rock!".to_string(),
        )));
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let result: String = result.unwrap().try_into().unwrap();
        assert_eq!(result, "you rock!");

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 8);
    }

    #[test]
    fn test_instr_fetch_local() {
        // Simple
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        vm.stack.push(Value::String(
            "this would normally be a function at the top of the call frame".to_string(),
        ));
        vm.stack.push(Value::Integer(42));

        chunk.add_instruction(Instruction::FetchLocal(0));
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.len() == 2);

        assert!(result.is_ok());

        let result: DwarfInteger = result.unwrap().try_into().unwrap();
        assert_eq!(result, 42);

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 2);
    }

    #[test]
    fn test_instr_fetch_local_nested() {
        // Nested
        let memory = Memory::new();
        let mut vm = VM::new(&memory);
        let mut chunk = Chunk::new("test".to_string());

        vm.stack.push(Value::String(
            "this would normally be a function at the top of the call frame".to_string(),
        ));
        vm.stack.push(Value::Integer(-1));
        vm.stack.push(Value::Integer(42));
        vm.stack.push(Value::Integer(-1));

        chunk.add_instruction(Instruction::FetchLocal(1));
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        let frame = CallFrame::new(0, 0, &chunk);
        vm.frames.push(frame);

        let result = vm.run(true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.len() == 4);

        assert!(result.is_ok());

        let result: DwarfInteger = result.unwrap().try_into().unwrap();
        assert_eq!(result, 42);

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 2);
    }

    #[test]
    fn test_instr_call() {
        let mut memory = Memory::new();
        let mut chunk = Chunk::new("fib".to_string());

        // Get the parameter off the stack
        chunk.add_instruction(Instruction::FetchLocal(0));
        chunk.add_instruction(Instruction::Constant(Value::Integer(1)));
        // Chcek if it's <= 1
        chunk.add_instruction(Instruction::LessThanOrEqual);
        chunk.add_instruction(Instruction::JumpIfFalse(2));
        // If false return 1
        chunk.add_instruction(Instruction::Constant(Value::Integer(1)));
        chunk.add_instruction(Instruction::Return);
        // return fidbn-1) + fib(n-2)
        // Load fib
        chunk.add_instruction(Instruction::Constant(Value::Chunk("fib", 0)));
        // load n
        chunk.add_instruction(Instruction::FetchLocal(0));
        // load 1
        chunk.add_instruction(Instruction::Constant(Value::Integer(1)));
        // subtract
        chunk.add_instruction(Instruction::Subtract);
        // Call fib(n-1)
        chunk.add_instruction(Instruction::Call(1));
        // load fib
        chunk.add_instruction(Instruction::Constant(Value::Chunk("fib", 0)));
        // load n
        chunk.add_instruction(Instruction::FetchLocal(0));
        // load 2
        chunk.add_instruction(Instruction::Constant(Value::Integer(2)));
        // subtract
        chunk.add_instruction(Instruction::Subtract);
        // Call fib(n-1)
        chunk.add_instruction(Instruction::Call(1));
        // add
        chunk.add_instruction(Instruction::Add);
        chunk.add_instruction(Instruction::Return);
        println!("{}", chunk);

        // put fib in memory
        let slot = memory.reserve_chunk_slot();
        memory.insert_chunk(chunk.clone(), slot);

        let frame = CallFrame::new(0, 0, &chunk);

        let mut vm = VM::new(&memory);

        // Push the func
        vm.stack.push(Value::String("fib".to_string()));
        // Push the argument
        vm.stack.push(Value::Integer(20));

        vm.frames.push(frame);

        let result = vm.run(false);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert_eq!(vm.stack.len(), 2);

        assert!(result.is_ok());

        let result: DwarfInteger = result.unwrap().try_into().unwrap();
        assert_eq!(result, 10946);

        // let frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 8);
    }
}
