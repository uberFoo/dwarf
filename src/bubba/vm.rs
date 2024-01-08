use std::collections::VecDeque;

use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use sarzak::lu_dog::ValueType;
use snafu::{location, prelude::*, Location};

use crate::{
    chacha::value::{EnumVariant, TupleEnum, UserStruct},
    new_ref, s_read, s_write, ChaChaError, DwarfInteger, NewRef, RefType, Span, Value, PATH_SEP,
};

use super::instr::{Instruction, Program};

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);

const ERR_CLR: Colour = Colour::Red;
const _OK_CLR: Colour = Colour::Green;
const _POP_CLR: Colour = Colour::Yellow;
const _OTH_CLR: Colour = Colour::Cyan;

#[derive(Debug, Snafu)]
pub(crate) enum BubbaError {
    #[snafu(display("\n{}: Halt and catch fire...🔥", ERR_CLR.bold().paint("error")))]
    HaltAndCatchFire { file: String, span: Span },
    #[snafu(display("\n{}: invalid instruction: {instr}", ERR_CLR.bold().paint("error")))]
    InvalidInstruction { instr: Instruction },
    #[snafu(display("\n{}: ip out of bounds at {ip}", ERR_CLR.bold().paint("error")))]
    IPOutOfBounds { ip: usize },
    #[snafu(display("\n{}: vm panic: {source}", ERR_CLR.bold().paint("error")))]
    ValueError { source: Box<dyn std::error::Error> },
    #[snafu(display("\n{}: vm panic: {message}", ERR_CLR.bold().paint("error")))]
    VmPanic { message: String },
}

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug)]
pub struct VM {
    /// Instruction Pointer
    ///
    /// This is an isize because we have negative jump offsets.
    ip: isize,
    /// Frame Pointer
    ///
    fp: usize,
    stack: Vec<RefType<Value>>,
    program: Vec<Instruction>,
    source_map: Vec<Span>,
    func_map: HashMap<String, (usize, usize)>,
}

impl VM {
    pub fn new(program: &Program) -> Self {
        let mut vm = VM {
            ip: 0,
            fp: 0,
            // 🚧 This shouldn't be hard-coded, and they should be configurable.
            stack: Vec::with_capacity(10 * 1024 * 1024),
            program: Vec::new(),
            source_map: Vec::new(),
            func_map: HashMap::default(),
        };

        let mut tmp_mem: Vec<Instruction> = Vec::new();
        let mut i = 0;
        for thonk in program.iter() {
            tmp_mem.append(&mut thonk.instructions.clone());
            vm.source_map.append(&mut thonk.spans.clone());
            vm.func_map
                .insert(thonk.get_name().to_owned(), (i, thonk.get_frame_size()));
            i += thonk.get_instruction_card();
        }

        for instr in tmp_mem.iter() {
            match instr {
                Instruction::CallDestination(name) => {
                    let name: String = (&*s_read!(name)).try_into().unwrap();
                    let (ip, _frame_size) =
                        vm.func_map.get(&name).expect("Unknown function: {name}");
                    vm.program.push(Instruction::Push(new_ref!(
                        Value,
                        Value::Integer(*ip as DwarfInteger)
                    )));
                }
                Instruction::LocalCardinality(name) => {
                    let name: String = (&*s_read!(name)).try_into().unwrap();
                    let (_ip, frame_size) = vm.func_map.get(&name).unwrap();
                    vm.program.push(Instruction::Push(new_ref!(
                        Value,
                        Value::Integer(*frame_size as DwarfInteger)
                    )));
                }
                _ => vm.program.push(instr.clone()),
            }
        }

        vm
    }

    fn get_span(&self) -> Span {
        self.source_map[(self.ip - 1) as usize].to_owned()
    }

    pub fn invoke(
        &mut self,
        func_name: &str,
        args: &[RefType<Value>],
        trace: bool,
    ) -> Result<RefType<Value>> {
        let (ip, frame_size) = self.func_map.get(func_name).unwrap();
        let frame_size = *frame_size;

        self.stack
            .push(new_ref!(Value, Value::Integer(*ip as DwarfInteger)));
        self.stack
            .push(new_ref!(Value, Value::Integer(frame_size as DwarfInteger)));

        for arg in args.iter() {
            self.stack.push(arg.clone());
        }

        for _ in 0..frame_size {
            self.stack.push(new_ref!(Value, Value::Empty));
        }

        self.stack.push(new_ref!(Value, Value::Empty));
        self.fp = frame_size + args.len() + 2;

        self.ip = *ip as isize;

        let result = self.inner_run(args.len(), frame_size, trace);

        self.stack.pop(); // fp
        for _ in 0..frame_size {
            self.stack.pop();
        }
        for _ in 0..args.len() {
            self.stack.pop();
        }
        self.stack.pop(); // frame size
        self.stack.pop(); // ip

        result
    }

    fn inner_run(
        &mut self,
        arity: usize,
        local_count: usize,
        trace: bool,
    ) -> Result<RefType<Value>> {
        loop {
            if self.ip as usize >= self.program.len() {
                return Err(BubbaError::IPOutOfBounds {
                    ip: self.ip as usize,
                }
                .into());
            }
            let instr = self.program[self.ip as usize].clone();
            let ip_offset: isize = {
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
                    println!("<{:08x}:\t{instr}", self.ip);
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

                        1
                    }
                    Instruction::And => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        if trace {
                            println!(
                                "\t\t{}\t{},\t{}",
                                Colour::Green.paint("and:"),
                                s_read!(a),
                                s_read!(b),
                            );
                        }
                        let c = Value::Boolean(
                            (&*s_read!(a)).try_into().unwrap()
                                && (&*s_read!(b)).try_into().unwrap(),
                        );
                        // if let Value::Error(e) = &c {
                        //     return Err(BubbaError::VmPanic { cause: Box::new(e) });
                        // }
                        self.stack.push(new_ref!(Value, c));

                        1
                    }
                    Instruction::Call(arity) => {
                        let callee = &self.stack[self.stack.len() - arity - 2];
                        if trace {
                            println!("\t\t{}:\t{}", Colour::Green.paint("func:"), s_read!(callee));
                        }

                        let local_count = &self.stack[self.stack.len() - arity - 1];
                        let local_count: usize =
                            (&*s_read!(local_count)).try_into().map_err(|e| {
                                BubbaError::ValueError {
                                    source: Box::new(e),
                                }
                            })?;

                        let callee: isize =
                            (&*s_read!(callee))
                                .try_into()
                                .map_err(|e| BubbaError::ValueError {
                                    source: Box::new(e),
                                })?;

                        let old_fp = self.fp;
                        let old_ip = self.ip;

                        // The call stack has been setup, but we need to make room
                        // for locals.
                        for _ in 0..local_count {
                            self.stack.push(new_ref!(Value, Value::Empty));
                        }
                        self.fp = self.stack.len();
                        self.stack.push(new_ref!(Value, old_fp.into()));

                        self.ip = callee;
                        let result = self.inner_run(arity, local_count, trace)?;

                        // Move the frame pointer back
                        // self.fp = (&*s_read!(self.stack[self.fp])).try_into().unwrap();
                        self.fp = old_fp;
                        self.ip = old_ip;

                        // This is clever, I guess. Or maybe it's just hard to read on first glance.
                        // Either way, we are just using fp..stack.len() as an iterator so that we
                        // can just pop our call frame off the stack.
                        (0..arity + local_count + 3).for_each(|_| {
                            self.stack.pop();
                        });

                        self.stack.push(result);
                        1
                    }
                    Instruction::Comment(_) => 1,
                    Instruction::DeconstructStructExpression => {
                        fn decode_expression(
                            value: RefType<Value>,
                        ) -> Result<(RefType<Value>, Option<RefType<Value>>)>
                        {
                            let read = s_read!(value);
                            match &*read {
                                Value::Enumeration(value) => match value {
                                    // 🚧 I can't tell if this is gross, or a sweet hack.
                                    // I think I'm referring to using the name as the scrutinee?
                                    EnumVariant::Unit(_, ty, value) => Ok((
                                        new_ref!(Value, Value::String(ty.to_owned())),
                                        Some(new_ref!(Value, Value::String(value.to_owned()))),
                                    )),
                                    // EnumFieldVariant::Struct(value) => (
                                    //     s_read!(value).type_name().to_owned(),
                                    //     Some(s_read!(value).get_value()),
                                    // ),
                                    EnumVariant::Tuple((ty, path), value) => {
                                        let path = path.split(PATH_SEP).collect::<Vec<&str>>();
                                        let mut path = VecDeque::from(path);
                                        let name = path.pop_front().unwrap().to_owned();
                                        if name.is_empty() {
                                            Ok((
                                                new_ref!(
                                                    Value,
                                                    Value::String(
                                                        s_read!(value).variant().to_owned()
                                                    )
                                                ),
                                                Some(s_read!(value).value().clone()),
                                            ))
                                        } else {
                                            Ok((
                                                new_ref!(Value, Value::String(name)),
                                                Some(new_ref!(
                                                    Value,
                                                    Value::Enumeration(EnumVariant::Tuple(
                                                        (
                                                            ty.clone(),
                                                            path.into_iter()
                                                                .collect::<Vec<&str>>()
                                                                .join(PATH_SEP)
                                                        ),
                                                        value.clone(),
                                                    ))
                                                )),
                                            ))
                                        }
                                    }
                                    _ => unimplemented!(),
                                },
                                _ => Ok((value.clone(), None)),
                            }
                        }

                        let mut variant = self.stack.pop().unwrap();
                        while let Ok((name, value)) = decode_expression(variant) {
                            dbg!(&name, &value);
                            self.stack.push(name);
                            if let Some(value) = value {
                                variant = value;
                            } else {
                                break;
                            }
                        }

                        1
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
                        if let Value::Error(e) = c {
                            return Err(BubbaError::ValueError {
                                source: Box::new(e),
                            }
                            .into());
                        }
                        self.stack.push(new_ref!(Value, c));

                        1
                    }
                    Instruction::Dup => {
                        let value = self.stack.pop().unwrap();
                        self.stack.push(value.clone());
                        self.stack.push(value);

                        1
                    }
                    Instruction::ExtractEnumValue => {
                        let user_enum = self.stack.pop().unwrap();
                        let Value::Enumeration(user_enum) = &*s_read!(user_enum) else {
                            return Err(BubbaError::ValueError {
                                source: Box::new(ChaChaError::BadnessHappened {
                                    message: format!("Unexpected value: {user_enum:?}."),
                                    location: location!(),
                                }),
                            }
                            .into());
                        };

                        match user_enum {
                            EnumVariant::Unit(_, _, value) => {
                                self.stack
                                    .push(new_ref!(Value, Value::String(value.to_owned())));
                            }
                            EnumVariant::Tuple(_, value) => {
                                self.stack.push(s_read!(value).value().clone());
                            }
                            _ => unimplemented!(),
                        }

                        1
                    }
                    // The fp is pointing someplace near the end of the vec.
                    // Nominally at one past the Thonk name at the bottom of the stack.
                    // Any locals will cause the fp to be moved up, with the
                    // locals existing between the Thonk name and the fp.
                    Instruction::FetchLocal(index) => {
                        // We gotta index the stack in reverse order.
                        let value = self.stack[self.fp - arity - local_count + index].clone();
                        self.stack.push(value);

                        1
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
                                            BubbaError::ValueError {
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
                                    BubbaError::ValueError {
                                        source: Box::new(ChaChaError::BadnessHappened {
                                            message: format!("Unexpected value: {value}."),
                                            location: location!(),
                                        }),
                                    }
                                    .into(),
                                )
                            }
                        }

                        1
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
                                            BubbaError::ValueError {
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
                                    BubbaError::ValueError {
                                        source: Box::new(ChaChaError::BadnessHappened {
                                            message: format!("Unexpected value type: {value}."),
                                            location: location!(),
                                        }),
                                    }
                                    .into(),
                                )
                            }
                        }

                        1
                    }
                    Instruction::HaltAndCatchFire => {
                        let span = self.stack.pop().unwrap();
                        let span: std::ops::Range<usize> = (&*s_read!(span))
                            .try_into()
                            .map_err(|e: ChaChaError| BubbaError::ValueError {
                                source: Box::new(e),
                            })
                            .unwrap();

                        let file = self.stack.pop().unwrap();
                        let file: String = (&*s_read!(file))
                            .try_into()
                            .map_err(|e: ChaChaError| BubbaError::ValueError {
                                source: Box::new(e),
                            })
                            .unwrap();

                        return Err(BubbaError::HaltAndCatchFire { file, span }.into());
                    }
                    Instruction::Index => {
                        let index = self.stack.pop().unwrap();
                        let list = self.stack.pop().unwrap();
                        let list = s_read!(list);
                        let index = s_read!(index);
                        match &*index {
                            Value::Integer(index) => {
                                let index = *index as usize;
                                if let Value::Vector { ty: _, inner: vec } = &list.clone() {
                                    if index < vec.len() {
                                        self.stack.push(vec[index].clone());
                                    } else {
                                        return Err(BubbaError::ValueError {
                                            source: Box::new(ChaChaError::IndexOutOfBounds {
                                                index,
                                                len: vec.len(),
                                                span: self.get_span(),
                                                location: location!(),
                                            }),
                                        }
                                        .into());
                                    }
                                } else if let Value::String(str) = &*list {
                                    let str = unicode_segmentation::UnicodeSegmentation::graphemes(
                                        str.as_str(),
                                        true,
                                    )
                                    .collect::<Vec<&str>>();

                                    if index < str.len() {
                                        self.stack.push(new_ref!(
                                            Value,
                                            Value::String(str[index..index + 1].join(""),)
                                        ))
                                    } else {
                                        return Err(BubbaError::ValueError {
                                            source: Box::new(ChaChaError::IndexOutOfBounds {
                                                index,
                                                len: str.len(),
                                                span: self.get_span(),
                                                location: location!(),
                                            }),
                                        }
                                        .into());
                                    }
                                } else {
                                    return Err(BubbaError::ValueError {
                                        source: Box::new(ChaChaError::NotIndexable {
                                            span: self.get_span(),
                                            location: location!(),
                                        }),
                                    }
                                    .into());
                                }
                            }
                            // Value::Range(_) => {
                            //     let range: Range<usize> = index.try_into()?;
                            //     let list = eval_expression(list.clone(), context, vm)?;
                            //     let list = s_read!(list);
                            //     if let Value::Vector { ty, inner: vec } = &list.clone() {
                            //         if range.end < vec.len() {
                            //             Ok(new_ref!(
                            //                 Value,
                            //                 Value::Vector {
                            //                     ty: ty.clone(),
                            //                     inner: vec[range].to_owned()
                            //                 }
                            //             ))
                            //         } else {
                            //             let value =
                            //                 &s_read!(index_expr).r11_x_value(&s_read!(lu_dog))[0];
                            //             let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                            //             let read = s_read!(span);
                            //             let span = read.start as usize..read.end as usize;

                            //             Err(ChaChaError::IndexOutOfBounds {
                            //                 index: range.end,
                            //                 len: vec.len(),
                            //                 span,
                            //                 location: location!(),
                            //             })
                            //         }
                            //     } else if let Value::String(str) = &*list {
                            //         let str = unicode_segmentation::UnicodeSegmentation::graphemes(
                            //             str.as_str(),
                            //             true,
                            //         )
                            //         .collect::<Vec<&str>>();

                            //         if range.end < str.len() {
                            //             Ok(new_ref!(Value, Value::String(str[range].join(""),)))
                            //         } else {
                            //             let value =
                            //                 &s_read!(index_expr).r11_x_value(&s_read!(lu_dog))[0];
                            //             let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                            //             let read = s_read!(span);
                            //             let span = read.start as usize..read.end as usize;

                            //             Err(ChaChaError::IndexOutOfBounds {
                            //                 index: range.end,
                            //                 len: str.len(),
                            //                 span,
                            //                 location: location!(),
                            //             })
                            //         }
                            // } else {
                            //     let value = &s_read!(list).r11_x_value(&s_read!(lu_dog))[0];
                            //     let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                            //     let read = s_read!(span);
                            //     let span = read.start as usize..read.end as usize;

                            //     Err(ChaChaError::NotIndexable {
                            //         span,
                            //         location: location!(),
                            //     })
                            // }
                            // }
                            _ => unreachable!(),
                        }

                        1
                    }
                    Instruction::Jump(offset) => {
                        if trace {
                            println!(
                                "\t\t{} {}",
                                Colour::Red.bold().paint("jmp"),
                                Colour::Yellow
                                    .bold()
                                    .paint(format!("0x{:08x}", self.ip + offset + 1))
                            );
                        }
                        offset + 1
                    }
                    Instruction::JumpIfFalse(offset) => {
                        let condition = self.stack.pop().unwrap();
                        let condition: bool = (&*s_read!(condition))
                            .try_into()
                            .map_err(|e: ChaChaError| BubbaError::ValueError {
                                source: Box::new(e),
                            })
                            .unwrap();

                        if !condition {
                            if trace {
                                println!(
                                    "\t\t{} {}",
                                    Colour::Red.bold().paint("jiff"),
                                    Colour::Yellow
                                        .bold()
                                        .paint(format!("0x{:08x}", self.ip + offset + 1))
                                );
                            }
                            offset + 1
                        } else {
                            1
                        }
                    }
                    Instruction::JumpIfTrue(offset) => {
                        let condition = self.stack.pop().unwrap();
                        let condition: bool = (&*s_read!(condition))
                            .try_into()
                            .map_err(|e: ChaChaError| BubbaError::ValueError {
                                source: Box::new(e),
                            })
                            .unwrap();

                        if condition {
                            if trace {
                                println!(
                                    "\t\t{} {}",
                                    Colour::Red.bold().paint("jift"),
                                    Colour::Yellow
                                        .bold()
                                        .paint(format!("0x{:08x}", self.ip + offset + 1))
                                );
                            }
                            offset + 1
                        } else {
                            1
                        }
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

                        1
                    }
                    Instruction::NewList(n) => {
                        if trace {
                            println!("\t\t{}\t{}", Colour::Green.paint("nl:"), n);
                        }

                        let ty = self.stack.pop().unwrap();
                        let ty: ValueType =
                            (&*s_read!(ty))
                                .try_into()
                                .map_err(|e| BubbaError::ValueError {
                                    source: Box::new(e),
                                })?;

                        if trace {
                            println!("\t\t\t\t{:?}", ty);
                        }

                        let ty = new_ref!(ValueType, ty);

                        let mut values = Vec::with_capacity(n);

                        for _i in 0..n {
                            let value = self.stack.pop().unwrap();
                            values.push(value.clone());
                            if trace {
                                println!("\t\t\t\t{}", s_read!(value));
                            }
                        }

                        values.reverse();

                        self.stack
                            .push(new_ref!(Value, Value::Vector { ty, inner: values }));

                        1
                    }
                    Instruction::NewTupleEnum(n) => {
                        if trace {
                            println!("\t\t{}\t{n}", Colour::Green.paint("nte:"));
                        }

                        let variant = self.stack.pop().unwrap();
                        let variant: String = (&*s_read!(variant)).try_into().map_err(|e| {
                            BubbaError::ValueError {
                                source: Box::new(e),
                            }
                        })?;

                        if trace {
                            println!("\t\t\t\t{}", variant);
                        }

                        let path = self.stack.pop().unwrap();
                        let path: String =
                            (&*s_read!(path))
                                .try_into()
                                .map_err(|e| BubbaError::ValueError {
                                    source: Box::new(e),
                                })?;

                        if trace {
                            println!("\t\t\t\t{}", path);
                        }

                        let ty = self.stack.pop().unwrap();
                        let ty: ValueType =
                            (&*s_read!(ty))
                                .try_into()
                                .map_err(|e| BubbaError::ValueError {
                                    source: Box::new(e),
                                })?;

                        if trace {
                            println!("\t\t\t\t{:?}", ty);
                        }

                        let ty = new_ref!(ValueType, ty);

                        let mut values = Vec::with_capacity(n);

                        for _i in 0..n {
                            let value = self.stack.pop().unwrap();
                            values.push(value.clone());
                            if trace {
                                println!("\t\t\t\t{}", s_read!(value));
                            }
                        }

                        if n > 0 {
                            // 🚧 This is temporary until Tuples are sorted.
                            let user_enum = TupleEnum::new(variant, values[0].to_owned());
                            let user_enum = new_ref!(TupleEnum, user_enum);
                            self.stack.push(new_ref!(
                                Value,
                                Value::Enumeration(EnumVariant::Tuple((ty, path), user_enum))
                            ));
                        } else {
                            let user_enum = EnumVariant::Unit(ty, path, variant);
                            self.stack
                                .push(new_ref!(Value, Value::Enumeration(user_enum)));
                        }

                        1
                    }
                    Instruction::NewUserType(n) => {
                        if trace {
                            println!("\t\t{}\t{n} {{", Colour::Green.paint("nut:"));
                        }

                        let name = self.stack.pop().unwrap();
                        let name: String =
                            (&*s_read!(name))
                                .try_into()
                                .map_err(|e| BubbaError::ValueError {
                                    source: Box::new(e),
                                })?;

                        if trace {
                            println!("\t\t\t\t{}", name);
                        }

                        let ty = self.stack.pop().unwrap();
                        let ty: ValueType =
                            (&*s_read!(ty))
                                .try_into()
                                .map_err(|e| BubbaError::ValueError {
                                    source: Box::new(e),
                                })?;

                        if trace {
                            println!("\t\t\t\t{:?}", ty);
                        }

                        let ty = new_ref!(ValueType, ty);

                        let mut inst = UserStruct::new(name, &ty);

                        for _i in 0..n {
                            let name = self.stack.pop().unwrap();
                            let value = self.stack.pop().unwrap();

                            inst.define_field(s_read!(name).to_inner_string(), value.clone());
                            if trace {
                                println!("\t\t\t\t{}: {}", s_read!(name), s_read!(value));
                            }
                        }

                        self.stack
                            .push(new_ref!(Value, Value::Struct(new_ref!(UserStruct, inst))));

                        if trace {
                            println!("\t\t\t\t}}");
                        }

                        1
                    }
                    Instruction::Not => {
                        let value = self.stack.pop().unwrap();
                        let value: bool =
                            (&*s_read!(value))
                                .try_into()
                                .map_err(|e| BubbaError::ValueError {
                                    source: Box::new(e),
                                })?;

                        self.stack.push(new_ref!(Value, Value::Boolean(!value)));

                        1
                    }
                    Instruction::Or => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        if trace {
                            println!(
                                "\t\t{}\t{},\t{}",
                                Colour::Green.paint("or:"),
                                s_read!(a),
                                s_read!(b),
                            );
                        }
                        let c = Value::Boolean(
                            (&*s_read!(a)).try_into().unwrap()
                                || (&*s_read!(b)).try_into().unwrap(),
                        );
                        // if let Value::Error(e) = &c {
                        //     return Err(BubbaError::VmPanic { cause: Box::new(e) });
                        // }
                        self.stack.push(new_ref!(Value, c));

                        1
                    }
                    Instruction::Out(stream) => {
                        let value = self.stack.pop().unwrap();
                        let value = s_read!(value);

                        match stream {
                            0 => println!("{value}"),
                            1 => eprintln!("{value}"),
                            _ => {
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::ValueError {
                                        source: Box::new(ChaChaError::BadnessHappened {
                                            message: format!("Unknown stream: {stream}."),
                                            location: location!(),
                                        }),
                                    }
                                    .into(),
                                )
                            }
                        };

                        1
                    }
                    Instruction::Pop => {
                        self.stack.pop();

                        1
                    }
                    Instruction::Push(value) => {
                        self.stack.push(value.clone());

                        1
                    }
                    Instruction::Return => {
                        let result = self.stack.pop().unwrap();
                        return Ok(result);
                    }
                    // The fp is pointing someplace near the end of the vec.
                    // Nominally at one past the Thonk name at the bottom of the stack.
                    // Any locals will cause the fp to be moved up, with the
                    // locals existing between the Thonk name and the fp.
                    Instruction::StoreLocal(index) => {
                        let value = self.stack.pop().unwrap();
                        // We gotta index into the stack in reverse order from the index.
                        self.stack[self.fp - arity - local_count + index] = value;

                        1
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

                        1
                    }
                    Instruction::TestEq => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack
                            .push(new_ref!(Value, Value::Boolean(*s_read!(a) == *s_read!(b))));

                        1
                    }
                    Instruction::TestLessThan => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack
                            .push(new_ref!(Value, Value::Boolean(s_read!(a).lt(&s_read!(b)))));

                        1
                    }
                    Instruction::TestLessThanOrEqual => {
                        let b = self.stack.pop().unwrap();
                        let a = self.stack.pop().unwrap();
                        self.stack
                            .push(new_ref!(Value, Value::Boolean(s_read!(a).lte(&s_read!(b)))));

                        1
                    }
                    invalid => {
                        return Err(BubbaError::InvalidInstruction {
                            instr: invalid.clone(),
                        }
                        .into())
                    }
                }
            };

            self.ip += ip_offset;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    #[cfg(feature = "tracy")]
    use tracy_client::Client;

    use crate::{
        bubba::instr::Thonk,
        dwarf::{DwarfFloat, DwarfInteger},
        interpreter::{initialize_interpreter, PrintableValueType},
        Context,
    };

    use super::*;

    const VERSION: &str = env!("CARGO_PKG_VERSION");
    pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

    #[test]
    fn instr_constant() {
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(result.is_err());
    }

    #[test]
    fn test_instr_return() {
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
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
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())), None);
        thonk.add_instruction(Instruction::Add, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
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
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 111.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())), None);
        thonk.add_instruction(Instruction::Subtract, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
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
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())), None);
        thonk.add_instruction(Instruction::Multiply, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
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
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 111.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())), None);
        thonk.add_instruction(Instruction::TestLessThanOrEqual, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert!(!as_bool);

        // assert_eq!(frame.ip, 4);

        // True case: less than
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())), None);
        thonk.add_instruction(Instruction::TestLessThanOrEqual, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert!(as_bool);

        // assert_eq!(frame.ip, 4);

        // True case: equal
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::TestLessThanOrEqual, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
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
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::TestLessThanOrEqual, None);
        thonk.add_instruction(Instruction::JumpIfFalse(2), None);
        thonk.add_instruction(
            Instruction::Push(new_ref!(Value, Value::String("epic fail!".to_string()))),
            None,
        );
        thonk.add_instruction(Instruction::Return, None);
        thonk.add_instruction(
            Instruction::Push(new_ref!(Value, Value::String("you rock!".to_string()))),
            None,
        );
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);
        let result = vm.invoke("test", &[], true);
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
    fn test_instr_store_fetch_local() {
        // Simple
        let mut thonk = Thonk::new("test".to_string());
        thonk.increment_frame_size();
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::StoreLocal(0), None);
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);
        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);

        let result = vm.invoke("test", &[], true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_instr_field() {
        use crate::{
            chacha::value::UserStruct,
            lu_dog::{Field, ValueType, WoogStruct},
            PATH_ROOT,
        };
        use sarzak::sarzak::{ObjectStore as SarzakStore, Ty, MODEL as SARZAK_MODEL};

        #[cfg(feature = "tracy")]
        Client::start();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ctx = Context::default();
        let struct_ty = {
            let mut lu_dog = s_write!(ctx.lu_dog);

            // We need to create a WoogStruct and add some fields to it
            let foo = WoogStruct::new(
                "Foo".to_owned(),
                PATH_ROOT.to_owned(),
                None,
                None,
                &mut lu_dog,
            );
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

        let mut thonk = Thonk::new("test".to_string());
        thonk.add_instruction(
            Instruction::Push(new_ref!(
                Value,
                Value::Struct(new_ref!(UserStruct, foo_inst))
            )),
            None,
        );
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "baz".into())), None);
        thonk.add_instruction(Instruction::FieldRead, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);
        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        let mut vm = VM::new(&program);

        let result = vm.invoke("test", &[], true);
        println!("{:?}", result);
        println!("{:?}", vm);

        assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let result: DwarfFloat = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, std::f64::consts::PI);
    }
}
