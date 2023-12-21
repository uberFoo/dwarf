use std::fmt;

use ansi_term::Colour;
use snafu::{location, prelude::*, Location};

use crate::{
    chacha::{
        memory::Memory,
        value::{ThonkInner, UserStruct},
    },
    new_ref, s_read, s_write, ChaChaError, NewRef, RefType, Value,
};

use super::instr::{Instruction, Thonk};

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);

const ERR_CLR: Colour = Colour::Red;
const OK_CLR: Colour = Colour::Green;
const POP_CLR: Colour = Colour::Yellow;
const OTH_CLR: Colour = Colour::Cyan;

#[derive(Debug, Snafu)]
pub(crate) enum BubbaError {
    #[snafu(display("\n{}: invalid instruction: {instr}", ERR_CLR.bold().paint("error")))]
    InvalidInstruction { instr: Instruction },
    #[snafu(display("\n{}: vm panic: {source}", ERR_CLR.bold().paint("error")))]
    VmPanic { source: Box<dyn std::error::Error> },
}

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub(crate) struct CallFrame<'a> {
    ip: usize,
    thonk: &'a Thonk,
}

impl<'a> CallFrame<'a> {
    pub(crate) fn new(thonk: &'a Thonk) -> Self {
        CallFrame { ip: 0, thonk }
    }

    #[inline]
    fn load_instruction(&mut self) -> Option<&Instruction> {
        let instr = self.thonk.get_instruction(self.ip);
        if instr.is_some() {
            self.ip += 1;
        }

        instr
    }

    #[inline]
    fn size(&self) -> usize {
        self.thonk.get_frame_size()
    }
}

impl<'a> fmt::Display for CallFrame<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ip: {}", self.ip)
    }
}

#[derive(Clone, Debug)]
pub struct VM<'b> {
    fp: usize,
    stack: Vec<RefType<Value>>,
    memory: &'b Memory,
}

impl<'b> VM<'b> {
    pub(crate) fn new(memory: &'b Memory) -> Self {
        VM {
            fp: 0,
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

    pub(crate) fn set_fp(&mut self, fp: usize) {
        self.fp = fp;
    }

    pub(crate) fn run(
        &mut self,
        arity: usize,
        frame: &mut CallFrame,
        trace: bool,
    ) -> Result<RefType<Value>> {
        loop {
            let ip = frame.ip;
            let frame_size = frame.size();
            let instr = frame.load_instruction();
            let ip_offset = if let Some(instr) = instr {
                if trace {
                    let len = self.stack.len();
                    for i in 0..len {
                        if i == self.fp {
                            print!("\t{} ->\t", Colour::Green.bold().paint("fp"));
                        } else {
                            print!("\t     \t");
                        }
                        println!("stack {i}:\t{}", s_read!(self.stack[i]));
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
                        // if let Value::Error(e) = &c {
                        //     return Err(BubbaError::VmPanic { cause: Box::new(e) });
                        // }
                        self.stack.push(new_ref!(Value, c));

                        0
                    }
                    Instruction::Call(arity) => {
                        let callee = &self.stack[self.stack.len() - arity - 1];
                        if trace {
                            println!("\t\t{}:\t{}", Colour::Green.paint("func:"), s_read!(callee));
                        }
                        // let callee: usize = match (&*s_read!(callee)).try_into() {
                        let callee = match <&Value as TryInto<String>>::try_into(&*s_read!(callee))
                        {
                            Ok(callee) => callee,
                            Err(e) => {
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::VmPanic {
                                        source: Box::new(e),
                                    }
                                    .into(),
                                );
                            }
                        };
                        // 🚧 I imagine that this dynamic lookup is slow.
                        let thonk = self
                            .memory
                            .get_thonk(
                                self.memory
                                    .thonk_index(callee.clone())
                                    .expect(format!("Panic! Thonk not found: {callee}.").as_str()),
                            )
                            .expect("missing thonk {callee}!");

                        let old_fp = self.fp;
                        for _ in 0..thonk.get_frame_size() {
                            self.stack.push(new_ref!(Value, Value::Empty));
                        }
                        self.fp = self.stack.len();
                        self.stack.push(new_ref!(Value, old_fp.into()));

                        let mut frame = CallFrame::new(thonk);

                        if trace {
                            println!("\t\t{}\t{}", Colour::Green.paint("frame:"), frame);
                        }

                        let result = self.run(*arity, &mut frame, trace)?;

                        // Move the frame pointer
                        self.fp = (&*s_read!(self.stack[self.fp])).try_into().unwrap();

                        // This is clever, I guess. Or maybe it's just hard to read on first glance.
                        // Either way, we are just using fp..stack.len() as an iterator so that we
                        // can just pop our call frame off the stack.
                        (0..arity + thonk.get_frame_size() + 2).for_each(|_| {
                            self.stack.pop();
                        });

                        self.stack.push(result);

                        0
                    }
                    Instruction::Divide => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        if trace {
                            println!(
                                "\t\t{}\t{},\t{}",
                                Colour::Green.paint("div:"),
                                s_read!(a),
                                s_read!(b),
                            );
                        }
                        let c = s_read!(a).clone() / s_read!(b).clone();
                        // if let Value::Error(e) = &c {
                        //     return Err(BubbaError::VmPanic { cause: Box::new(e) });
                        // }
                        self.stack.push(new_ref!(Value, c));

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
                                //         return Err::<RefType<Value>, BubbaError>(
                                //             BubbaError::VmPanic {
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
                                        return Err::<RefType<Value>, Error>(
                                            BubbaError::VmPanic {
                                                source: Box::new(ChaChaError::NoSuchField {
                                                    field: s_read!(field).to_string(),
                                                    ty: s_read!(ty_).to_string(),
                                                }),
                                            }
                                            .into(),
                                        );
                                    }
                                }
                            }
                            value => {
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::VmPanic {
                                        source: Box::new(ChaChaError::BadnessHappened {
                                            message: format!("Unexpected value type: {value}."),
                                            location: location!(),
                                        }),
                                    }
                                    .into(),
                                )
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
                                //         return Err::<RefType<Value>, BubbaError>(
                                //             BubbaError::VmPanic {
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
                                        return Err::<RefType<Value>, Error>(
                                            BubbaError::VmPanic {
                                                source: Box::new(ChaChaError::NoSuchField {
                                                    field: s_read!(field).to_string(),
                                                    ty: s_read!(ty_).to_string(),
                                                }),
                                            }
                                            .into(),
                                        )
                                    }
                                }
                            }
                            value => {
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::VmPanic {
                                        source: Box::new(ChaChaError::BadnessHappened {
                                            message: format!("Unexpected value type: {value}."),
                                            location: location!(),
                                        }),
                                    }
                                    .into(),
                                )
                            }
                        }

                        0
                    }
                    Instruction::JumpIfFalse(offset) => {
                        let condition = self.stack.pop().unwrap();
                        let condition: bool = (&*s_read!(condition))
                            .try_into()
                            .map_err(|e: ChaChaError| BubbaError::VmPanic {
                                source: Box::new(e),
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
                    Instruction::TestLessThanOrEqual => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack
                            .push(new_ref!(Value, Value::Boolean(s_read!(a).lte(&s_read!(b)))));

                        0
                    }
                    Instruction::Multiply => {
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
                        // if let Value::Error(e) = &c {
                        //     return Err(BubbaError::VmPanic { cause: Box::new(e) });
                        // }
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
                    Instruction::Out(stream) => {
                        let value = self.stack.pop().unwrap();
                        let value = s_read!(value);

                        match stream {
                            0 => println!("{value}"),
                            1 => eprintln!("{value}"),
                            _ => {
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::VmPanic {
                                        source: Box::new(ChaChaError::BadnessHappened {
                                            message: format!("Unknown stream: {stream}."),
                                            location: location!(),
                                        }),
                                    }
                                    .into(),
                                )
                            }
                        };

                        0
                    }
                    Instruction::Pop => {
                        self.stack.pop();

                        0
                    }
                    Instruction::PopLocal(index) => {
                        let value = self.stack.pop().unwrap();
                        // We gotta index the stack in reverse order.
                        self.stack[self.fp - arity - frame_size + index] = value;

                        0
                    }
                    Instruction::Push(value) => {
                        self.stack.push(value.clone());

                        0
                    }
                    // The fp is pointing someplace the end of the vec.
                    // Nominally at the Thonk name at the bottom of the stack.
                    // Any locals will cause the fp to be moved up, with the
                    // locals existing between the Thonk name and the fp.
                    Instruction::PushLocal(index) => {
                        // We gotta index the stack in reverse order.
                        let value = self.stack[self.fp - arity - frame_size + index].clone();
                        self.stack.push(value);

                        0
                    }
                    Instruction::Return => {
                        let result = self.stack.pop().unwrap();
                        // dbg!(&self.stack);
                        // for _ in 0..fp {
                        //     self.stack.pop();
                        // }
                        return Ok(result);
                    }
                    Instruction::Subtract => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let c = s_read!(a).clone() - s_read!(b).clone();
                        if trace {
                            println!(
                                "\t\t{}\t{},\t{}",
                                Colour::Green.paint("sub:"),
                                s_read!(a),
                                s_read!(b),
                            );
                        }
                        // if let Value::Error(e) = &c {
                        //     return Err(BubbaError::VmPanic { cause: Box::new(e) });
                        // }

                        self.stack.push(new_ref!(Value, c));

                        0
                    }
                    invalid => {
                        return Err(BubbaError::InvalidInstruction {
                            instr: invalid.clone(),
                        }
                        .into())
                    }
                }
            } else {
                return Err(BubbaError::VmPanic {
                    source: Box::new(ChaChaError::BadnessHappened {
                        message: "ip out of bounds".to_string(),
                        location: location!(),
                    }),
                }
                .into());
            };

            frame.ip += ip_offset;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    #[cfg(feature = "tracy")]
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

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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
        thonk.add_instruction(Instruction::Multiply);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
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

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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

        vm.fp = 2;
        vm.stack.push(new_ref!(Value, Value::Empty));

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(1, &mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.len() == 3);

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

        #[cfg(feature = "tracy")]
        Client::start();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ctx = Context::default();
        let struct_ty = {
            let mut lu_dog = s_write!(ctx.lu_dog);

            // We need to create a WoogStruct and add some fields to it
            let foo = WoogStruct::new("Foo".to_owned(), None, None, &mut lu_dog);
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

        let ctx = initialize_interpreter(2, dwarf_home, ctx).unwrap();
        let ty_name = PrintableValueType(false, struct_ty.clone(), ctx.models());
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

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(0, &mut frame, true);
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
            "this would normally be a function at the top (bottom?) of the call frame".into()
        ));
        vm.stack
            .push(new_ref!(Value, <i32 as Into<Value>>::into(-1)));
        vm.stack.push(new_ref!(Value, 42.into()));
        vm.stack.push(new_ref!(Value, Value::Integer(-1)));

        thonk.add_instruction(Instruction::PushLocal(1));
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        vm.fp = 4;
        vm.stack.push(new_ref!(Value, Value::Empty));

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(3, &mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.len() == 5);

        assert!(result.is_ok());

        let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, 42);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 2);
    }

    #[test]
    fn test_instr_modify_local() {
        let memory = Memory::new();
        let mut vm = VM::new(&memory.0);
        let mut thonk = Thonk::new("test".to_string());

        vm.stack.push(new_ref!(
            Value,
            "this would normally be a function at the top (bottom?) of the call frame".into()
        ));
        vm.stack
            .push(new_ref!(Value, <i32 as Into<Value>>::into(-1)));
        vm.stack.push(new_ref!(Value, Value::Integer(-1)));
        vm.stack.push(new_ref!(Value, Value::Integer(-1)));

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())));
        thonk.add_instruction(Instruction::PopLocal(1));
        thonk.add_instruction(Instruction::PushLocal(1));
        thonk.add_instruction(Instruction::Return);
        println!("{}", thonk);

        vm.fp = 4;
        vm.stack.push(new_ref!(Value, Value::Empty));

        let mut frame = CallFrame::new(&thonk);
        // vm.frames.push(frame);

        let result = vm.run(3, &mut frame, true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.len() == 5);

        assert!(result.is_ok());

        let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_instr_call() {
        let mut memory = Memory::new();
        let mut thonk = Thonk::new("fib".to_string());

        // Get the parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(0));
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
        // Check if it's <= 1
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
        thonk.add_instruction(Instruction::JumpIfFalse(2));
        // If false return 1
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
        thonk.add_instruction(Instruction::Return);
        // return fib(n-1) + fib(n-2)
        // Load fib
        thonk.add_instruction(Instruction::Push(new_ref!(
            Value,
            Value::Thonk(ThonkInner::Thonk("fib".to_owned()))
        )));
        // load n
        thonk.add_instruction(Instruction::PushLocal(0));
        // load 1
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
        // subtract
        thonk.add_instruction(Instruction::Subtract);
        // Call fib(n-1)
        thonk.add_instruction(Instruction::Call(1));
        // load fib
        thonk.add_instruction(Instruction::Push(new_ref!(
            Value,
            Value::Thonk(ThonkInner::Thonk("fib".to_owned()))
        )));
        // load n
        thonk.add_instruction(Instruction::PushLocal(0));
        // load 2
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 2.into())));
        // subtract
        thonk.add_instruction(Instruction::Subtract);
        // Call fib(n-2)
        thonk.add_instruction(Instruction::Call(1));
        // add
        thonk.add_instruction(Instruction::Add);
        thonk.add_instruction(Instruction::Return);
        thonk.increment_frame_size();
        println!("{}", thonk);

        // put fib in memory
        let slot = memory.0.reserve_thonk_slot();
        memory.0.insert_thonk(thonk.clone(), slot);

        let mut frame = CallFrame::new(&thonk);

        let mut vm = VM::new(&memory.0);

        // Push the func
        vm.stack.push(new_ref!(Value, "fib".into()));
        // Push the argument
        vm.stack.push(new_ref!(Value, 20.into()));

        // vm.frames.push(frame);
        vm.fp = 2;
        vm.stack.push(new_ref!(Value, Value::Empty));

        let result = vm.run(0, &mut frame, false);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert_eq!(vm.stack.len(), 3);

        assert!(result.is_ok());

        let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, 10946);

        // let mut frame = vm.frames.pop().unwrap();
        // assert_eq!(frame.ip, 8);
    }
}
