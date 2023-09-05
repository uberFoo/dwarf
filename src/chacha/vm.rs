use std::fmt;

use ansi_term::Colour;

use crate::{
    chacha::{error::Result, memory::Memory, value::UserStruct},
    new_ref, s_read, s_write, ChaChaError, NewRef, RefType, Value, ValueType,
};

#[derive(Clone, Debug)]
pub enum Instruction {
    /// Add the top two values on the stack.
    Add,
    /// Call a function with the given arity.
    Call(usize),
    /// Duplicate the top of the stack.
    Dup,
    /// Fetch a local variable.
    ///
    /// The parameter is it's distance from the frame pointer, I think.
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
    LessThanOrEqual,
    /// Multiply the top two values on the stack.
    Mul,
    /// New UserType
    ///
    /// The first element of the tuple is the name of the user type. The second
    /// is the type ([ValueType]) of the user type. The third is the number of
    /// fields in the user type.
    /// There is a `([String], [ValueType], [Value])`tuple on the stack for each
    /// field.
    NewUserType(String, RefType<ValueType>, usize),
    /// Pop the top value off the stack.
    PopLocal(usize),
    /// Push a value onto the stack.
    Push(RefType<Value>),
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
            Instruction::LessThanOrEqual => write!(f, "{}", opcode_style.paint("lte")),
            Instruction::Mul => write!(f, "{}", opcode_style.paint("mul")),
            Instruction::NewUserType(name, _ty, n) => {
                write!(f, "{}{name}({n})", opcode_style.paint("new"))
            }
            Instruction::PopLocal(index) => write!(
                f,
                "{} {}",
                opcode_style.paint("pop_local"),
                operand_style.paint(index.to_string())
            ),
            // Instruction::Push => write!(f, "{}", opcode_style.paint("push")),
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
pub(crate) struct Thonk {
    pub(crate) _name: String,
    instructions: Vec<Instruction>,
}

impl Thonk {
    pub(crate) fn new(name: String) -> Self {
        Thonk {
            _name: name,
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
}

impl fmt::Display for Thonk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.instructions.iter().enumerate() {
            writeln!(f, "{:08x}:\t {}", i, instr)?;
        }
        Ok(())
    }
}

// #[derive(Debug)]
// pub(crate) struct CallFrame {
//     ip: usize,
//     fp: usize,
//     thonk: Thonk,
// }

// impl CallFrame {
//     pub(crate) fn new(ip: usize, fp: usize, thonk: Thonk) -> Self {
//         CallFrame { ip, fp, thonk }
//     }

//     fn load_instruction(&mut self) -> Option<&Instruction> {
//         let instr = self.thonk.get_instruction(self.ip);
//         if instr.is_some() {
//             self.ip += 1;
//         }

//         instr
//     }
// }

// impl fmt::Display for CallFrame {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "ip: {}, fp: {}", self.ip, self.fp)
//     }
// }

#[derive(Debug)]
pub(crate) struct CallFrame<'a> {
    ip: usize,
    fp: usize,
    thonk: &'a Thonk,
}

impl<'a> CallFrame<'a> {
    pub(crate) fn new(ip: usize, fp: usize, thonk: &'a Thonk) -> Self {
        CallFrame { ip, fp, thonk }
    }

    fn load_instruction(&mut self) -> Option<&Instruction> {
        let instr = self.thonk.get_instruction(self.ip);
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
// pub struct VM<'b> {
pub struct VM<'b> {
    stack: Vec<RefType<Value>>,
    memory: &'b Memory,
}

// impl<'b> VM<'b> {
impl<'b> VM<'b> {
    pub(crate) fn new(memory: &'b Memory) -> Self {
        VM {
            // 🚧 These shouldn't be hard-coded, and they should be configurable.
            stack: Vec::with_capacity(10 * 1024 * 1024),
            memory,
        }
    }

    pub(crate) fn push_stack(&mut self, value: RefType<Value>) {
        self.stack.push(value);
    }

    pub(crate) fn pop_stack(&mut self) -> Option<RefType<Value>> {
        self.stack.pop()
    }

    pub(crate) fn run(&mut self, frame: &mut CallFrame, trace: bool) -> Result<RefType<Value>> {
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
                        println!("stack {}:\t{}", len - i - 1, s_read!(self.stack[i]));
                    }
                    println!();
                    println!("{:08x}:\t{}", ip, instr);
                }
                match instr {
                    Instruction::Add => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        if trace {
                            println!(
                                "\t\t{}\t{},\t{}",
                                Colour::Green.paint("add:"),
                                s_read!(a),
                                s_read!(b),
                            );
                        }
                        let c = s_read!(a).clone() + s_read!(b).clone();
                        if let Value::Error(e) = c {
                            return Err(ChaChaError::VmPanic { message: e });
                        }
                        self.stack.push(new_ref!(Value, c));

                        0
                    }
                    Instruction::Call(arity) => {
                        let callee = &self.stack[self.stack.len() - arity - 1];
                        if trace {
                            println!("\t\t{}:\t{}", Colour::Green.paint("func:"), s_read!(callee));
                        }
                        // let callee: usize = match (&*s_read!(callee)).try_into() {
                        let callee: usize =
                            match <&Value as TryInto<usize>>::try_into(&*s_read!(callee)) {
                                Ok(callee) => callee,
                                Err(e) => {
                                    return Err::<RefType<Value>, ChaChaError>(
                                        ChaChaError::VmPanic {
                                            message: format!("{}: {e}", s_read!(callee)),
                                        },
                                    );
                                }
                            };
                        let thonk = self.memory.get_thonk(callee).unwrap();
                        let mut frame = CallFrame::new(0, self.stack.len() - arity - 1, thonk);

                        if trace {
                            println!("\t\t{}\t{}", Colour::Green.paint("frame:"), frame);
                        }

                        let fp = frame.fp;
                        // self.frames.push(frame);

                        let result = match self.run(&mut frame, trace) {
                            Ok(result) => result,
                            Err(e) => {
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    message: format!("{callee}: {e}"),
                                });
                            }
                        };

                        (fp..self.stack.len()).for_each(|_| {
                            self.stack.pop();
                        });

                        self.stack.push(result);

                        0
                    }
                    Instruction::Dup => {
                        let value = self.stack.pop().unwrap();
                        self.stack.push(value.clone());
                        self.stack.push(value);

                        0
                    }
                    Instruction::FieldRead => {
                        let field = self.stack.pop().unwrap();
                        let ty_ = self.stack.pop().unwrap();
                        match &*s_read!(ty_) {
                            Value::ProxyType {
                                module: _,
                                obj_ty: _,
                                id: _,
                                plugin: _,
                            } => {
                                unimplemented!();
                                // match s_read!(ty_).get_attr_value(s_read!(field).as_ref()) {
                                //     Ok(value) => {
                                //         if trace {
                                //             println!(
                                //                 "\t\t{}\t{}",
                                //                 Colour::Green.paint("field_read:"),
                                //                 s_read!(value)
                                //             );
                                //         }
                                //         self.stack.push(value);
                                //     }
                                //     Err(_e) => {
                                //         return Err::<RefType<Value>, ChaChaError>(
                                //             ChaChaError::VmPanic {
                                //                 message: format!(
                                //                     "Unknown field {} for proxy.",
                                //                     s_read!(field),
                                //                     // s_read!(ty_)
                                //                 ),
                                //             },
                                //         );
                                //     }
                                // }
                            }
                            Value::Struct(ty_) => {
                                match s_read!(ty_).get_field_value(s_read!(field).as_ref()) {
                                    Some(value) => {
                                        if trace {
                                            println!(
                                                "\t\t{}\t{}",
                                                Colour::Green.paint("field_read:"),
                                                s_read!(value)
                                            );
                                        }
                                        self.stack.push(value.clone());
                                    }
                                    None => {
                                        return Err::<RefType<Value>, ChaChaError>(
                                            ChaChaError::VmPanic {
                                                message: format!(
                                                    "Unknown field {} for {}.",
                                                    s_read!(field),
                                                    s_read!(ty_)
                                                ),
                                            },
                                        );
                                    }
                                }
                            }
                            value => {
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    message: format!("Unexpected value type: {value}."),
                                })
                            }
                        }

                        0
                    }
                    Instruction::FieldWrite => {
                        let field = self.stack.pop().unwrap();
                        let ty_ = self.stack.pop().unwrap();
                        let value = self.stack.pop().unwrap();
                        match &*s_read!(ty_) {
                            Value::ProxyType {
                                module: _,
                                obj_ty: _,
                                id: _,
                                plugin: _,
                            } => {
                                unimplemented!();

                                // match s_write!(ty_)
                                //     .set_attr_value(s_read!(field).as_ref(), value.clone())
                                // {
                                //     Ok(_) => {
                                //         if trace {
                                //             println!(
                                //                 "\t\t{}\t{}",
                                //                 Colour::Green.paint("field_write:"),
                                //                 s_read!(value)
                                //             );
                                //         }
                                //     }
                                //     Err(_e) => {
                                //         return Err::<RefType<Value>, ChaChaError>(
                                //             ChaChaError::VmPanic {
                                //                 message: format!(
                                //                     "Unknown field {} for proxy.",
                                //                     s_read!(field),
                                //                     // s_read!(ty_)
                                //                 ),
                                //             },
                                //         );
                                //     }
                                // }
                            }
                            Value::Struct(ty_) => {
                                match s_write!(ty_)
                                    .set_field_value(s_read!(field).as_ref(), value.clone())
                                {
                                    Some(_) => {
                                        if trace {
                                            println!(
                                                "\t\t{}\t{}",
                                                Colour::Green.paint("field_write:"),
                                                s_read!(value)
                                            );
                                        }
                                    }
                                    None => {
                                        return Err::<RefType<Value>, ChaChaError>(
                                            ChaChaError::VmPanic {
                                                message: format!(
                                                    "Unknown field {} for {}.",
                                                    s_read!(field),
                                                    s_read!(ty_)
                                                ),
                                            },
                                        )
                                    }
                                }
                            }
                            value => {
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    message: format!("Unexpected value type: {value}."),
                                })
                            }
                        }

                        0
                    }
                    Instruction::JumpIfFalse(offset) => {
                        let condition = self.stack.pop().unwrap();
                        let condition: bool = (&*s_read!(condition))
                            .try_into()
                            .map_err(|e| ChaChaError::VmPanic {
                                message: format!("{}", e),
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
                        self.stack
                            .push(new_ref!(Value, Value::Boolean(s_read!(a).lte(&s_read!(b)))));

                        0
                    }
                    Instruction::Mul => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        if trace {
                            println!(
                                "\t\t{}\t{},\t{}",
                                Colour::Green.paint("mul:"),
                                s_read!(a),
                                s_read!(b),
                            );
                        }
                        let c = s_read!(a).clone() * s_read!(b).clone();
                        if let Value::Error(e) = c {
                            return Err(ChaChaError::VmPanic { message: e });
                        }
                        self.stack.push(new_ref!(Value, c));

                        0
                    }
                    Instruction::NewUserType(name, ty, n) => {
                        if trace {
                            println!("\t\t{}\t{} {{", Colour::Green.paint("new:"), name);
                        }

                        let mut inst = UserStruct::new(name, ty);

                        for _i in 0..*n {
                            let name = self.stack.pop().unwrap();
                            let value = self.stack.pop().unwrap();

                            inst.define_field(s_read!(name).to_string(), value.clone());
                            if trace {
                                println!("\t\t\t\t{}: {}", s_read!(name), s_read!(value));
                            }
                        }

                        self.stack
                            .push(new_ref!(Value, Value::Struct(new_ref!(UserStruct, inst))));

                        if trace {
                            println!("\t\t\t\t}}");
                        }

                        0
                    }
                    Instruction::PopLocal(index) => {
                        let value = self.stack.pop().unwrap();
                        self.stack[fp + index + 1] = value;

                        0
                    }
                    Instruction::Push(value) => {
                        self.stack.push(value.clone());

                        0
                    }
                    Instruction::PushLocal(index) => {
                        let value = self.stack[fp + index + 1].clone();
                        self.stack.push(value);

                        0
                    }
                    Instruction::Return => {
                        return Ok(self.stack.pop().unwrap());
                    }
                    Instruction::Subtract => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let c = s_read!(a).clone() - s_read!(b).clone();
                        if let Value::Error(e) = c {
                            return Err(ChaChaError::VmPanic { message: e });
                        }

                        self.stack.push(new_ref!(Value, c));

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
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use tracy_client::Client;

    use crate::{
        dwarf::{DwarfFloat, DwarfInteger},
        interpreter::{initialize_interpreter, PrintableValueType},
        Context,
    };

    use super::*;

    #[test]
    fn test_instr_constant() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(result.is_err());

        let tos = vm.stack.pop().unwrap();
        let as_int: DwarfInteger = (&*s_read!(tos)).try_into().unwrap();
        assert_eq!(as_int, 42);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 1);
    }

    #[test]
    fn test_instr_return() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(as_int, 42);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 2);
    }

    #[test]
    fn test_instr_add() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())));
        thonk.add_instruction(Instruction::Add);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(as_int, 111);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 4);
    }

    #[test]
    fn test_instr_subtract() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 111.into())));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())));
        thonk.add_instruction(Instruction::Subtract);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(as_int, 42);

        // assert_eq!(frame.ip, 4);
    }

    #[test]
    fn test_instr_multiply() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())));
        thonk.add_instruction(Instruction::Mul);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(as_int, 2898);
    }

    #[test]
    fn test_instr_less_than_or_equal() {
        // False Case
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 111.into())));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())));
        thonk.add_instruction(Instruction::LessThanOrEqual);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert!(!as_bool);

        // assert_eq!(frame.ip, 4);

        // True case: less than
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())));
        thonk.add_instruction(Instruction::LessThanOrEqual);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert!(as_bool);

        // assert_eq!(frame.ip, 4);

        // True case: equal
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::LessThanOrEqual);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert!(as_bool);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 4);
    }

    #[test]
    fn test_instr_jump_if_false() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::LessThanOrEqual);
        thonk.add_instruction(Instruction::JumpIfFalse(2));
        thonk.add_instruction(Instruction::Push(new_ref!(
            Value,
            Value::String("epic fail!".to_string())
        )));
        thonk.add_instruction(Instruction::Return);
        thonk.add_instruction(Instruction::Push(new_ref!(
            Value,
            Value::String("you rock!".to_string())
        )));
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let result: String = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, "you rock!");

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 8);
    }

    #[test]
    fn test_instr_fetch_local() {
        // Simple
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        vm.stack.push(new_ref!(
            Value,
            "this would normally be a function at the top of the call frame".into()
        ));
        vm.stack.push(new_ref!(Value, 42.into()));

        thonk.add_instruction(Instruction::PushLocal(0));
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.len() == 2);

        assert!(result.is_ok());

        let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_instr_field() {
        use crate::{
            chacha::value::UserStruct,
            lu_dog::{Field, ValueType, WoogStruct},
        };
        use sarzak::sarzak::{ObjectStore as SarzakStore, Ty, MODEL as SARZAK_MODEL};

        Client::start();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ctx = Context::default();
        let struct_ty = {
            let mut lu_dog = s_write!(ctx.lu_dog);

            // We need to create a WoogStruct and add some fields to it
            let foo = WoogStruct::new("Foo".to_owned(), None, &mut lu_dog);
            // let _ = WoogItem::new_woog_struct(source, &mt, lu_dog);
            let struct_ty = ValueType::new_woog_struct(&foo, &mut lu_dog);
            let ty = Ty::new_integer(&sarzak);
            let ty = ValueType::new_ty(&ty, &mut lu_dog);
            let _ = Field::new("bar".to_owned(), &foo, &ty, &mut lu_dog);
            let ty = Ty::new_float(&sarzak);
            let ty = ValueType::new_ty(&ty, &mut lu_dog);
            let _ = Field::new("baz".to_owned(), &foo, &ty, &mut lu_dog);
            struct_ty
        };

        // Now we need an instance.
        let dwarf_home = env::var("DWARF_HOME")
            .unwrap_or_else(|_| {
                let mut home = env::var("HOME").unwrap();
                home.push_str("/.dwarf");
                home
            })
            .into();

        let ctx = initialize_interpreter(dwarf_home, ctx, sarzak).unwrap();
        let ty_name = PrintableValueType(&struct_ty, &ctx);
        let mut foo_inst = UserStruct::new(ty_name.to_string(), &struct_ty);
        foo_inst.define_field("bar", new_ref!(Value, 42.into()));
        foo_inst.define_field("baz", new_ref!(Value, std::f64::consts::PI.into()));

        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        vm.stack.push(new_ref!(
            Value,
            Value::Struct(new_ref!(UserStruct, foo_inst))
        ));
        vm.stack.push(new_ref!(Value, "baz".into()));

        thonk.add_instruction(Instruction::FieldRead);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let result: DwarfFloat = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, std::f64::consts::PI);
    }

    #[test]
    fn test_instr_fetch_local_nested() {
        // Nested
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        vm.stack.push(new_ref!(
            Value,
            "this would normally be a function at the top of the call frame".into()
        ));
        vm.stack
            .push(new_ref!(Value, <i32 as Into<Value>>::into(-1)));
        vm.stack.push(new_ref!(Value, 42.into()));
        vm.stack.push(new_ref!(Value, Value::Integer(-1)));

        thonk.add_instruction(Instruction::PushLocal(1));
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(0, 0, &thonk);
        // vm.frames.push(frame);

        let result = vm.run(&mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.len() == 4);

        assert!(result.is_ok());

        let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, 42);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 2);
    }

    #[test]
    fn test_instr_call() {
        let mut memory = Memory::new();
        let mut thonk = Thonk::new("fib".to_string());

        // Get the parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(0));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
        // Chcek if it's <= 1
        thonk.add_instruction(Instruction::LessThanOrEqual);
        thonk.add_instruction(Instruction::JumpIfFalse(2));
        // If false return 1
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
        thonk.add_instruction(Instruction::Return);
        // return fidbn-1) + fib(n-2)
        // Load fib
        thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Thonk("fib", 0))));
        // load n
        thonk.add_instruction(Instruction::PushLocal(0));
        // load 1
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
        // subtract
        thonk.add_instruction(Instruction::Subtract);
        // Call fib(n-1)
        thonk.add_instruction(Instruction::Call(1));
        // load fib
        thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Thonk("fib", 0))));
        // load n
        thonk.add_instruction(Instruction::PushLocal(0));
        // load 2
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 2.into())));
        // subtract
        thonk.add_instruction(Instruction::Subtract);
        // Call fib(n-1)
        thonk.add_instruction(Instruction::Call(1));
        // add
        thonk.add_instruction(Instruction::Add);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        // put fib in memory
        let slot = memory.0.reserve_thonk_slot();
        memory.0.insert_thonk(thonk.clone(), slot);

        let mut frame = CallFrame::new(0, 0, &thonk);

        let mut vm = VM::new(&memory.0);

        // Push the func
        vm.stack.push(new_ref!(Value, "fib".into()));
        // Push the argument
        vm.stack.push(new_ref!(Value, 20.into()));

        // vm.frames.push(frame);

        let result = vm.run(&mut frame, false);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert_eq!(vm.stack.len(), 2);

        assert!(result.is_ok());

        let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, 10946);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 8);
    }
}
