use std::fmt;

use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    chacha::{
        error::Result,
        memory::Memory,
        value::{ThonkInner, UserStruct},
    },
    new_ref, s_read, s_write, ChaChaError, NewRef, RefType, Value,
};

use super::instr::{Instruction, Thonk};

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

#[derive(Clone, Debug)]
pub struct VM<'b> {
    stack: Vec<RefType<Value>>,
    memory: &'b Memory,
}

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
                        // if let Value::Error(e) = &c {
                        //     return Err(ChaChaError::VmPanic { cause: Box::new(e) });
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
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    cause: e.to_string(),
                                });
                            }
                        };
                        let thonk = self
                            .memory
                            .get_thonk(
                                self.memory
                                    .thonk_index(callee.clone())
                                    .expect(format!("Panic! Thonk not found: {callee}.").as_str()),
                            )
                            .expect("missing thonk {callee}!");
                        let mut frame = CallFrame::new(0, self.stack.len() - arity - 1, thonk);

                        if trace {
                            println!("\t\t{}\t{}", Colour::Green.paint("frame:"), frame);
                        }

                        let fp = frame.fp;

                        let result = match self.run(&mut frame, trace) {
                            Ok(result) => result,
                            Err(e) => {
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    cause: e.to_string(),
                                });
                            }
                        };

                        // This is clever, I guess. Or maybe it's just hard to read on first glance.
                        // Either way, we are just using fp..stack.len() as an iterator so that we
                        // can just pop our call frame off the stack.
                        (fp..self.stack.len()).for_each(|_| {
                            self.stack.pop();
                        });

                        dbg!(&self.stack);

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
                                                cause: ChaChaError::NoSuchField {
                                                    field: s_read!(field).to_string(),
                                                    ty: s_read!(ty_).to_string(),
                                                }
                                                .to_string(),
                                            },
                                        );
                                    }
                                }
                            }
                            value => {
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    cause: ChaChaError::BadnessHappened {
                                        message: format!("Unexpected value type: {value}."),
                                        location: location!(),
                                    }
                                    .to_string(),
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
                                                cause: ChaChaError::NoSuchField {
                                                    field: s_read!(field).to_string(),
                                                    ty: s_read!(ty_).to_string(),
                                                }
                                                .to_string(),
                                            },
                                        )
                                    }
                                }
                            }
                            value => {
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    cause: ChaChaError::BadnessHappened {
                                        message: format!("Unexpected value type: {value}."),
                                        location: location!(),
                                    }
                                    .to_string(),
                                })
                            }
                        }

                        0
                    }
                    Instruction::JumpIfFalse(offset) => {
                        let condition = self.stack.pop().unwrap();
                        let condition: bool = (&*s_read!(condition))
                            .try_into()
                            .map_err(|e: ChaChaError| ChaChaError::VmPanic {
                                cause: e.to_string(),
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
                        // if let Value::Error(e) = &c {
                        //     return Err(ChaChaError::VmPanic { cause: Box::new(e) });
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
                                return Err::<RefType<Value>, ChaChaError>(ChaChaError::VmPanic {
                                    cause: ChaChaError::BadnessHappened {
                                        message: format!("Unknown stream: {stream}."),
                                        location: location!(),
                                    }
                                    .to_string(),
                                })
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
                        let result = self.stack.pop().unwrap();
                        dbg!(&result, &self.stack);
                        return Ok(result);
                    }
                    Instruction::Subtract => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        let c = s_read!(a).clone() - s_read!(b).clone();
                        // if let Value::Error(e) = &c {
                        //     return Err(ChaChaError::VmPanic { cause: Box::new(e) });
                        // }

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
                    cause: ChaChaError::BadnessHappened {
                        message: "ip out of bounds".to_string(),
                        location: location!(),
                    }
                    .to_string(),
                });
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
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
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
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
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
        thonk.add_instruction(Instruction::TestLessThanOrEqual);
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

        let ctx = initialize_interpreter(2, dwarf_home, ctx, sarzak).unwrap();
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
            "this would normally be a function at the top (bottom?) of the call frame".into()
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
