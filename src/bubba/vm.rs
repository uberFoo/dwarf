use std::path::{Path, PathBuf};

#[cfg(feature = "async")]
use smol::future;

#[cfg(feature = "async")]
use puteketeke::Executor;

#[cfg(feature = "async")]
use once_cell::sync::OnceCell;

#[cfg(feature = "tracy-client")]
use tracy_client::{non_continuous_frame, span, Client};

use abi_stable::{
    library::{lib_header_from_path, LibrarySuffix, RawLibrary},
    std_types::{RBox, RErr, ROk, ROption, RResult},
};
use ansi_term::Colour;
use log::{self, log_enabled, Level::Trace};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use snafu::{location, prelude::*, Location};

use crate::{
    bubba::{value::Value, RESULT, STRING},
    chacha::{
        ffi_value::FfiValue,
        value::{EnumVariant, TupleEnum, UserStruct},
    },
    keywords::INVOKE_FUNC,
    lu_dog::{ValueType, ValueTypeEnum},
    new_ref,
    plug_in::{PluginModRef, PluginType},
    s_read, s_write,
    sarzak::{ObjectStore as SarzakStore, Ty, MODEL as SARZAK_MODEL},
    ChaChaError, DwarfInteger, NewRef, RefType, Span, ERR_CLR, POP_CLR,
};

use super::instr::{Instruction, Program};

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);

#[cfg(feature = "async")]
static mut EXECUTOR: OnceCell<Executor> = OnceCell::new();

#[derive(Debug, Snafu)]
pub(crate) enum BubbaError {
    #[snafu(display("\n{}: addition error: {} + {}", ERR_CLR.bold().paint("error"), left, right))]
    Addition { left: Value, right: Value },
    #[snafu(display("\n{}: negation error", ERR_CLR.bold().paint("error")))]
    Bang { value: Value },
    #[snafu(display("\n{}: could not convert `{}` to `{}`", ERR_CLR.bold().paint("error"), src, dst))]
    Conversion { src: String, dst: String },
    #[snafu(display("\n{}: division error: `{}` ÷ `{}`", ERR_CLR.bold().paint("error"), left, right))]
    Division { left: Value, right: Value },
    #[snafu(display("\n{}: Halt and catch fire...🔥", ERR_CLR.bold().paint("error")))]
    HaltAndCatchFire { file: String, span: Span },
    /// Index out of bounds
    ///
    #[snafu(display("\n{}: index `{}` is out of bounds for array of length `{}`.", ERR_CLR.bold().paint("error"), POP_CLR.paint(index.to_string()), POP_CLR.paint(len.to_string())))]
    IndexOutOfBounds {
        index: usize,
        len: usize,
        span: Span,
    },
    #[snafu(display("\n{}: invalid instruction: {instr}", ERR_CLR.bold().paint("error")))]
    InvalidInstruction { instr: Instruction },
    #[snafu(display("\n{}: ip out of bounds at {ip}", ERR_CLR.bold().paint("error")))]
    IPOutOfBounds { ip: usize },
    #[snafu(display("\n{}: multiplication error: {} × {}", ERR_CLR.bold().paint("error"), left, right))]
    Multiplication { left: Value, right: Value },
    #[snafu(display("\n{}: negation error: {}", ERR_CLR.bold().paint("error"), value))]
    Negation { value: Value },
    #[snafu(display("\n{}: no such field: {} in {}", ERR_CLR.bold().paint("error"), field, ty))]
    NoSuchField { field: String, ty: String },
    #[snafu(display("\n{}: not indexable.", ERR_CLR.bold().paint("error")))]
    NotIndexable { span: Span, location: Location },
    #[snafu(display("\n{}: subtraction error: {} - {}", ERR_CLR.bold().paint("error"), left, right))]
    Subtraction { left: Value, right: Value },
    // #[snafu(display("\n{}: value error: {value}\n\t--> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    // ValueError { value: Value, location: Location },
    #[snafu(display("\n{}: vm panic: {message}\n\t--> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    VmPanic { message: String, location: Location },
}

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug)]
enum StackValue {
    Pointer(RefType<Value>),
    Value(Value),
}

impl StackValue {
    fn into_pointer(self) -> RefType<Value> {
        match self {
            StackValue::Pointer(p) => p,
            StackValue::Value(v) => new_ref!(Value, v),
        }
    }

    #[inline]
    fn into_value(self) -> Value {
        match self {
            StackValue::Pointer(p) => s_read!(p).clone(),
            StackValue::Value(v) => v,
        }
    }
}

impl std::fmt::Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Pointer(p) => write!(f, "{}", *s_read!(p)),
            StackValue::Value(v) => write!(f, "{}", v),
        }
    }
}

impl From<RefType<Value>> for StackValue {
    fn from(p: RefType<Value>) -> Self {
        StackValue::Pointer(p)
    }
}

impl From<Value> for StackValue {
    fn from(v: Value) -> Self {
        StackValue::Value(v)
    }
}

#[derive(Clone)]
pub struct VM {
    /// Instruction Pointer
    ///
    /// This is an isize because we have negative jump offsets.
    // ip: isize,
    /// Frame Pointer
    ///
    // fp: usize,
    // stack: Vec<StackValue>,
    instrs: Vec<Instruction>,
    source_map: Vec<Span>,
    func_map: HashMap<String, (usize, usize)>,
    sarzak: SarzakStore,
    args: RefType<Value>,
    home: PathBuf,
    captures: Option<Vec<RefType<Value>>>,
    program: Program,
    labels: HashMap<String, usize>,
    #[cfg(feature = "async")]
    thread_count: usize,
}

impl std::fmt::Debug for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // writeln!(f, "ip: {}", ip)?;
        // writeln!(f, "fp: {}", fp)?;
        // writeln!(f, "stack: ")?;
        // self.debug_stack(f)?;
        writeln!(f, "func_map: {:?}", self.func_map)?;
        writeln!(f, "args: {:?}", self.args)?;
        writeln!(f, "home_dir: {:?}", self.home)?;
        writeln!(f, "captures: {:?}", self.captures)?;
        writeln!(f, "labels: {:?}", self.labels)?;
        Ok(())
    }
}

impl VM {
    #[cfg(feature = "async")]
    pub fn new(
        program: &Program,
        args: &[RefType<Value>],
        home: &Path,
        thread_count: usize,
    ) -> Self {
        Self::inner_new(program, args, home, thread_count)
    }

    #[cfg(not(feature = "async"))]
    pub fn new(program: &Program, args: &[RefType<Value>], home: &Path) -> Self {
        Self::inner_new(program, args, home, 0)
    }

    fn inner_new(
        program: &Program,
        args: &[RefType<Value>],
        home: &Path,
        thread_count: usize,
    ) -> Self {
        #[cfg(feature = "tracy-client")]
        Client::start();
        // println!("{}", program);
        // dbg!(&program);
        let Some(Value::ValueType(str_ty)) = program.get_symbol(STRING) else {
            panic!("No STRING symbol found.")
        };

        if log_enabled!(target: "vm", Trace) {
            eprintln!("{program}");
        }

        let mut vm = VM {
            // ip: 0,
            // fp: 0,
            // stack: Vec::new(),
            instrs: Vec::new(),
            source_map: Vec::new(),
            func_map: HashMap::default(),
            sarzak: SarzakStore::from_bincode(SARZAK_MODEL).unwrap(),
            // These are the arguments to the program. The type is String.
            args: new_ref!(
                Value,
                Value::Vector {
                    ty: new_ref!(ValueType, str_ty.clone()),
                    inner: new_ref!(Vec<RefType<Value>>, args.to_vec())
                }
            ),
            home: home.to_path_buf(),
            captures: None,
            program: program.clone(),
            labels: HashMap::default(),
            #[cfg(feature = "async")]
            thread_count,
        };

        let mut tmp_mem: Vec<Instruction> = Vec::new();
        let mut i = 0;
        for thonk in program.iter() {
            tmp_mem.append(&mut thonk.instructions().to_owned());
            vm.source_map.append(&mut thonk.spans().to_owned());
            vm.func_map
                .insert(thonk.name().to_owned(), (i, thonk.frame_size()));
            i += thonk.instruction_card();
        }

        let mut missing_symbols = HashSet::default();
        // This is where we patch up the function calls.
        for (addr, instr) in tmp_mem.iter().enumerate() {
            match instr {
                Instruction::Label(name) => {
                    let r_name = &*s_read!(name);
                    vm.labels.insert(r_name.to_owned(), addr);
                    vm.instrs.push(Instruction::Label(name.clone()));
                }
                Instruction::CallDestination(name) => {
                    let name = &*s_read!(name);
                    if let Some((ip, _frame_size)) = vm.func_map.get(name) {
                        vm.instrs.push(Instruction::Push(new_ref!(
                            Value,
                            Value::Integer(*ip as DwarfInteger)
                        )));
                    } else {
                        missing_symbols.insert(name.to_owned());
                    }
                }
                Instruction::LocalCardinality(name) => {
                    let name = &*s_read!(name);
                    if let Some((_ip, frame_size)) = vm.func_map.get(name) {
                        vm.instrs.push(Instruction::Push(new_ref!(
                            Value,
                            Value::Integer(*frame_size as DwarfInteger)
                        )));
                    } else {
                        missing_symbols.insert(name.to_owned());
                    }
                }
                _ => vm.instrs.push(instr.clone()),
            }
        }

        if !missing_symbols.is_empty() {
            panic!("Missing symbols: {:?}", missing_symbols);
        }

        vm
    }

    fn get_span(&self, ip: isize) -> Span {
        self.source_map[(ip - 1) as usize].to_owned()
    }

    pub fn invoke(&mut self, func_name: &str, args: &[RefType<Value>]) -> Result<RefType<Value>> {
        let (ip, frame_size) = self.func_map.get(func_name).unwrap();
        let frame_size = *frame_size;

        let mut stack = Vec::new();

        // Address of the function to invoke.
        stack.push(Value::Integer(*ip as DwarfInteger).into());
        // Number of parameters and locals in the function.
        stack.push(Value::Integer(frame_size as DwarfInteger).into());

        for arg in args.iter() {
            stack.push(arg.clone().into());
        }
        for _ in 0..frame_size - args.len() {
            stack.push(Value::Empty.into());
        }

        // Arity
        stack.push(StackValue::Value(
            Value::Integer(args.len() as DwarfInteger),
        ));
        // Frame size
        stack.push(StackValue::Value(Value::Integer(
            (frame_size + 2) as DwarfInteger,
        )));
        // This is the IP sentinel value.
        stack.push(Value::Empty.into());
        // Setup the frame pointer and it's sentinel.
        stack.push(Value::Empty.into());

        let fp = frame_size + 5;
        let ip = *ip as isize;

        let trace = log_enabled!(target: "vm", Trace);

        let result = self.inner_run(ip, fp, stack, args.len(), frame_size, trace);

        // The FP is taken by the return handling code.
        // stack.pop(); // fp
        // stack.pop(); // ip
        // stack.pop(); // frame size
        // stack.pop(); // arity
        // for _ in 0..frame_size {
        //     stack.pop();
        // }
        // // for _ in 0..args.len() {
        // // stack.pop();
        // // }
        // stack.pop(); // local count
        // stack.pop(); // func addr

        result
    }

    // fn print_stack(&self) {
    //     let len = stack.len();
    //     for i in 0..len {
    //         if i == fp {
    //             eprint!("\t{} ->\t", Colour::Green.bold().paint("fp"));
    //         } else {
    //             eprint!("\t     \t");
    //         }
    //         eprintln!("stack {i}:\t{}", stack[i]);
    //     }
    // }

    // fn debug_stack(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    //     let len = stack.len();
    //     for i in 0..len {
    //         if i == fp {
    //             write!(f, "\t{} ->\t", Colour::Green.bold().paint("fp"))?;
    //         } else {
    //             write!(f, "\t     \t")?;
    //         }
    //         writeln!(f, "stack {i}:\t{}", stack[i])?;
    //     }

    //     Ok(())
    // }

    fn inner_run(
        &mut self,
        mut ip: isize,
        mut fp: usize,
        mut stack: Vec<StackValue>,
        mut arity: usize,
        mut local_count: usize,
        trace: bool,
    ) -> Result<RefType<Value>> {
        #[cfg(feature = "tracy-client")]
        let _frame = non_continuous_frame!("inner_run");
        loop {
            if ip as usize >= self.instrs.len() {
                return Err(BubbaError::IPOutOfBounds { ip: ip as usize }.into());
            }

            if trace {
                print_stack(&stack, fp);
                for iip in 0.max(ip - 3)..(self.instrs.len() as isize).min(ip + 3isize) {
                    let instr = &self.instrs[iip as usize];

                    let src = if let Some(source) = self.program.get_source() {
                        let span = self.source_map[ip as usize].clone();
                        &source[span]
                    } else {
                        ""
                    };

                    if ip == iip {
                        println!(
                            "<{:08x}:\t{instr}\t\t<- {}\t{}",
                            iip,
                            Colour::Purple.bold().paint("ip"),
                            Colour::White.dimmed().paint(src)
                        );
                    } else {
                        println!(
                            "<{:08x}:\t{instr}\t\t\t{}",
                            iip,
                            Colour::White.dimmed().paint(src)
                        );
                    }
                }
                println!();
            }

            let instr = &self.instrs[ip as usize];
            let ip_offset: isize = {
                match instr {
                    Instruction::Add => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        if trace {
                            println!("\t\t{}\t{},\t{}", Colour::Green.paint("add:"), a, b,);
                        }
                        let c = a.into_value() + b.into_value();
                        stack.push(c.into());

                        1
                    }
                    Instruction::And => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        if trace {
                            println!("\t\t{}\t{},\t{}", Colour::Green.paint("and:"), a, b,);
                        }
                        let c = Value::Boolean(
                            a.into_value().try_into()? && b.into_value().try_into()?,
                        );
                        stack.push(c.into());

                        1
                    }
                    #[cfg(feature = "async")]
                    Instruction::AsyncCall(func_arity) => {
                        #[cfg(feature = "tracy-client")]
                        let _span = span!("AsyncCall");
                        let callee = &stack[stack.len() - func_arity - 2].clone();
                        if trace {
                            println!("\t\t{}:\t{callee}", Colour::Green.paint("func:"));
                        }

                        let stack_local_count = &stack[stack.len() - func_arity - 1].clone();
                        if trace {
                            println!("\t\t{}:\t{stack_local_count}", Colour::Green.paint("func:"));
                        }

                        let (callee, frame_size, local_card, stack_count): (
                            isize,
                            Value,
                            usize,
                            usize,
                        ) = match stack_local_count.clone().into_value() {
                            Value::Integer(arity) => {
                                let callee: isize = callee.clone().into_value().try_into()?;
                                let local_count = arity as usize;
                                (
                                    callee,
                                    <usize as Into<Value>>::into(func_arity + local_count + 2),
                                    local_count,
                                    2,
                                )
                            }
                            Value::FubarPointer {
                                name,
                                frame_size,
                                captures,
                            } => {
                                self.captures = Some(captures);
                                let addr = self.func_map.get(&name).unwrap().0;
                                let local_count = frame_size;
                                (
                                    addr as isize,
                                    // This is plus one because the call frame does not include
                                    // the frame size.
                                    <usize as Into<Value>>::into(func_arity + local_count + 1),
                                    local_count,
                                    1,
                                )
                            }
                            _ => {
                                return Err(BubbaError::VmPanic {
                                    message: format!("Unexpected value: {stack_local_count:?}.",),
                                    location: location!(),
                                }
                                .into())
                            }
                        };

                        let mut new_stack = Vec::new();
                        for _ in 0..*func_arity + stack_count {
                            new_stack.push(stack.pop().unwrap());
                        }
                        new_stack.reverse();

                        let old_stack = &mut stack;
                        let mut stack = new_stack;

                        // The call stack has been setup, but we need to make room
                        // for locals.
                        (0..local_card).for_each(|_| {
                            stack.push(Value::Empty.into());
                        });

                        // Push the arity
                        stack.push(<usize as Into<Value>>::into(arity).into());

                        // Push the call frame size so that we can clean in out quickly
                        stack.push(frame_size.into());

                        // Push the sentinel IP
                        stack.push(Value::Empty.into());

                        let fp = stack.len();
                        // Push the sentinel frame pointer
                        stack.push(Value::Empty.into());

                        let mut vm = self.clone();
                        // This clone keeps the "escapes func body" ghoul away.
                        let func_arity = func_arity.clone();

                        let future = async move {
                            vm.inner_run(callee, fp, stack, func_arity, local_card, false)
                        };

                        let executor = match unsafe { EXECUTOR.get() } {
                            Some(executor) => executor,
                            None => {
                                let executor = Executor::new(self.thread_count);
                                unsafe {
                                    EXECUTOR.set(executor).unwrap();
                                    EXECUTOR.get().unwrap()
                                }
                            }
                        };
                        let worker = executor.root_worker();
                        let child_task = worker.create_task(future).unwrap();

                        // let task = executor
                        //     .new_worker()
                        //     .create_task(async move {
                        //         let result = child_task.await.unwrap();
                        //         worker.destroy();
                        //         Ok(result.into())
                        //     })
                        //     .unwrap();

                        let value = new_ref!(
                            Value,
                            Value::Task {
                                name: "invoke".to_owned(),
                                running: false,
                                task: Some(child_task)
                            }
                        );

                        old_stack.push(value.into());

                        // ip = callee;
                        // let result = self.inner_run(arity, local_count, trace)?;

                        // Move the frame pointer back
                        // fp = (&*s_read!(stack[fp])).try_into().unwrap();
                        // fp = old_fp;
                        // ip = old_ip;

                        // (0..arity + local_count + 3).for_each(|_| {
                        //     stack.pop();
                        // });

                        // stack.push(result);
                        1
                    }
                    #[cfg(feature = "async")]
                    Instruction::AsyncSpawn(func_arity) => {
                        #[cfg(feature = "tracy-client")]
                        let _span = span!("AsyncSpawn");
                        let callee = &stack[stack.len() - func_arity - 2].clone();
                        if trace {
                            println!("\t\t{}:\t{callee}", Colour::Green.paint("func:"));
                        }

                        let stack_local_count = &stack[stack.len() - func_arity - 1].clone();
                        if trace {
                            println!("\t\t{}:\t{stack_local_count}", Colour::Green.paint("func:"));
                        }

                        let (callee, frame_size, local_card, stack_count): (
                            isize,
                            Value,
                            usize,
                            usize,
                        ) = match stack_local_count.clone().into_value() {
                            Value::Integer(arity) => {
                                let callee: isize = callee.clone().into_value().try_into()?;
                                let local_count = arity as usize;
                                (
                                    callee,
                                    <usize as Into<Value>>::into(func_arity + local_count + 2),
                                    local_count,
                                    2,
                                )
                            }
                            Value::FubarPointer {
                                name,
                                frame_size,
                                captures,
                            } => {
                                self.captures = Some(captures);
                                let addr = self.func_map.get(&name).unwrap().0;
                                let local_count = frame_size;
                                (
                                    addr as isize,
                                    // This is plus one because the call frame does not include
                                    // the frame size.
                                    <usize as Into<Value>>::into(func_arity + local_count + 1),
                                    local_count,
                                    1,
                                )
                            }
                            _ => {
                                return Err(BubbaError::VmPanic {
                                    message: format!("Unexpected value: {stack_local_count:?}.",),
                                    location: location!(),
                                }
                                .into())
                            }
                        };

                        let mut new_stack = Vec::new();
                        for _ in 0..*func_arity + stack_count {
                            new_stack.push(stack.pop().unwrap());
                        }
                        new_stack.reverse();

                        let old_stack = &mut stack;
                        let mut stack = new_stack;

                        // The call stack has been setup, but we need to make room
                        // for locals.
                        (0..local_card).for_each(|_| {
                            stack.push(Value::Empty.into());
                        });

                        // Push the arity
                        stack.push(<usize as Into<Value>>::into(arity).into());

                        // Push the call frame size so that we can clean in out quickly
                        stack.push(frame_size.into());

                        // Push the sentinel IP
                        stack.push(Value::Empty.into());

                        let fp = stack.len();
                        // Push the sentinel frame pointer
                        stack.push(Value::Empty.into());

                        let mut vm = self.clone();
                        // This clone keeps the "escapes func body" ghoul away.
                        let func_arity = func_arity.clone();

                        let future = async move {
                            vm.inner_run(callee, fp, stack, func_arity, local_card, false)
                        };

                        let executor = match unsafe { EXECUTOR.get() } {
                            Some(executor) => executor,
                            None => {
                                let executor = Executor::new(self.thread_count);
                                unsafe {
                                    EXECUTOR.set(executor).unwrap();
                                    EXECUTOR.get().unwrap()
                                }
                            }
                        };
                        let worker = executor.new_worker();
                        let child_task = worker.spawn_task(future).unwrap();
                        executor.start_task(&child_task);

                        // let task = executor
                        //     .new_worker()
                        //     .create_task(async move {
                        //         let result = child_task.await.unwrap();
                        //         worker.destroy();
                        //         Ok(result.into())
                        //     })
                        //     .unwrap();

                        let value = new_ref!(
                            Value,
                            Value::Task {
                                name: "invoke".to_owned(),
                                running: true,
                                task: Some(child_task)
                            }
                        );

                        old_stack.push(value.into());

                        // ip = callee;
                        // let result = self.inner_run(arity, local_count, trace)?;

                        // Move the frame pointer back
                        // fp = (&*s_read!(stack[fp])).try_into().unwrap();
                        // fp = old_fp;
                        // ip = old_ip;

                        // (0..arity + local_count + 3).for_each(|_| {
                        //     stack.pop();
                        // });

                        // stack.push(result);
                        1
                    }
                    #[cfg(feature = "async")]
                    Instruction::Await => {
                        #[cfg(feature = "tracy-client")]
                        let _span = span!("Await");
                        let future = stack.pop().unwrap().into_pointer();
                        let mut expression = &mut *s_write!(future);

                        let executor = match unsafe { EXECUTOR.get() } {
                            Some(executor) => executor,
                            None => {
                                let executor = Executor::new(self.thread_count);
                                unsafe {
                                    EXECUTOR.set(executor).unwrap();
                                    EXECUTOR.get().unwrap()
                                }
                            }
                        };

                        let result = match &mut expression {
                            Value::Task {
                                name: _,
                                task,
                                running,
                            } => {
                                if let Some(task) = task.take() {
                                    if !*running {
                                        executor.start_task(&task);
                                    }
                                    log::trace!(target: "async", "block on");
                                    let f = future::block_on(task)?;
                                    log::trace!(target: "async", "block off");
                                    f
                                } else {
                                    panic!("Task is missing -- already awaited.");
                                }
                            }
                            // Value::Task {
                            //     worker: _,
                            //     parent: None,
                            // } => thing.into_pointer(),
                            // Value::Task {
                            //     worker: Some(worker),
                            //     parent,
                            // } => {
                            //     if let Some(parent) = parent.take() {
                            //         worker.start_task(&parent);
                            //         future::block_on(parent).map_err(|e| {
                            //             BubbaError::ValueError {
                            //                 source: Box::new(e),
                            //                 location: location!(),
                            //             }
                            //         })?
                            //     } else {
                            //         panic!("Parent is missing.");
                            //     }
                            // }
                            huh => {
                                dbg!(huh);
                                unimplemented!()
                            }
                        };

                        stack.push(result.into());

                        1
                    }
                    Instruction::Call(func_arity) => {
                        let callee = &stack[stack.len() - func_arity - 2];
                        if trace {
                            println!("\t\t{}:\t{callee}", Colour::Green.paint("func:"));
                        }

                        let stack_local_count = &stack[stack.len() - func_arity - 1];
                        if trace {
                            println!("\t\t{}:\t{stack_local_count}", Colour::Green.paint("func:"));
                        }

                        if let Value::Plugin((_, plugin)) = callee.clone().into_value() {
                            let method: String =
                                stack_local_count.clone().into_value().try_into()?;

                            match method.as_str() {
                                INVOKE_FUNC => {
                                    let mut plugin = s_write!(plugin);
                                    let args = stack.pop().clone().unwrap().into_value();
                                    let Value::Vector { inner, .. } = args else {
                                        panic!("Expected a vector of arguments.")
                                    };
                                    let args = s_read!(inner)
                                        .iter()
                                        .map(|v| {
                                            <Value as Into<FfiValue>>::into((*s_read!(v)).clone())
                                        })
                                        .collect::<Vec<FfiValue>>();
                                    let func = stack.pop().clone().unwrap().into_value();
                                    let func = func.to_inner_string();
                                    let ty = stack.pop().clone().unwrap().into_value();
                                    let ty = ty.to_inner_string();
                                    let module = stack.pop().clone().unwrap().into_value();
                                    let module = module.to_inner_string();

                                    match plugin.invoke_func(
                                        module.as_str().into(),
                                        ty.as_str().into(),
                                        func.as_str().into(),
                                        args.into(),
                                    ) {
                                        ROk(value) => {
                                            let result = self.program.get_symbol(RESULT).expect(
                                                "The RESULT symbol is missing from the program.",
                                            );
                                            stack.push(
                                                <(FfiValue, &Value) as Into<Value>>::into((
                                                    value, result,
                                                ))
                                                .into(),
                                            );
                                        }
                                        RErr(e) => {
                                            return Err(BubbaError::VmPanic {
                                                message: format!("Plugin error: {:?}", e),
                                                location: location!(),
                                            }
                                            .into())
                                        }
                                    }
                                }
                                _ => {
                                    return Err(BubbaError::VmPanic {
                                        message: format!("Unknown method: {method}.",),
                                        location: location!(),
                                    }
                                    .into())
                                }
                            }

                            1
                        } else {
                            let (callee, frame_size): (isize, Value) = match stack_local_count
                                .clone()
                                .into_value()
                            {
                                Value::Integer(arity) => {
                                    let callee: isize = callee.clone().into_value().try_into()?;

                                    local_count = arity as usize;
                                    (
                                        callee,
                                        <usize as Into<Value>>::into(func_arity + local_count + 2),
                                    )
                                }
                                Value::FubarPointer {
                                    name,
                                    frame_size,
                                    captures,
                                } => {
                                    self.captures = Some(captures);
                                    let addr = self.func_map.get(&name).unwrap().0;
                                    local_count = frame_size;
                                    (
                                        addr as isize,
                                        // This is plus one because the call frame does not include
                                        // the frame size.
                                        <usize as Into<Value>>::into(func_arity + local_count + 1),
                                    )
                                }
                                // Value::Plugin(plugin) => {}
                                _ => {
                                    return Err(BubbaError::VmPanic {
                                        message: format!(
                                            "Unexpected value: {stack_local_count:?}.",
                                        ),
                                        location: location!(),
                                    }
                                    .into())
                                }
                            };

                            let old_fp = fp;
                            let old_ip = ip;

                            // The call stack has been setup, but we need to make room
                            // for locals.
                            for _ in 0..local_count {
                                stack.push(Value::Empty.into());
                            }

                            // Push the arity
                            stack.push(<usize as Into<Value>>::into(arity).into());
                            arity = *func_arity;

                            // Push the call frame size so that we can clean in out quickly
                            stack.push(frame_size.into());

                            // Push the old IP
                            stack.push(<isize as Into<Value>>::into(old_ip).into());

                            fp = stack.len();
                            stack.push(<usize as Into<Value>>::into(old_fp).into());

                            // ip = callee;
                            // let result = self.inner_run(arity, local_count, trace)?;

                            // Move the frame pointer back
                            // fp = (&*s_read!(stack[fp])).try_into().unwrap();
                            // fp = old_fp;
                            // ip = old_ip;

                            // (0..arity + local_count + 3).for_each(|_| {
                            //     stack.pop();
                            // });

                            // stack.push(result);
                            callee - ip
                        }
                    }
                    Instruction::CaptureLocal(from, to) => {
                        let Some(captures) = self.captures.as_ref() else {
                            panic!("Attempt to capture a local outside of a lambda call.")
                        };
                        let value = captures[*from].clone();
                        stack[fp - arity - local_count - 3 + to] = value.into();
                        if trace {
                            println!(
                                "\t\t{}\t{},\t{}",
                                Colour::Green.paint("capture_local:"),
                                from,
                                to
                            );
                        }
                        1
                    }
                    Instruction::Comment(_) => 1,
                    // Instruction::DeconstructStructExpression => {
                    //     fn decode_expression(
                    //         value: RefType<Value>,
                    //     ) -> Result<(RefType<Value>, Option<RefType<Value>>)>
                    //     {
                    //         let read = s_read!(value);
                    //         match &*read {
                    //             Value::Enumeration(value) => match value {
                    //                 // 🚧 I can't tell if this is gross, or a sweet hack.
                    //                 // I think I'm referring to using the name as the scrutinee?
                    //                 EnumVariant::Unit(_, ty, value) => Ok((
                    //                     new_ref!(Value, Value::String(ty.to_owned())),
                    //                     Some(new_ref!(Value, Value::String(value.to_owned()))),
                    //                 )),
                    //                 // EnumFieldVariant::Struct(value) => (
                    //                 //     *value.type_name().to_owned(),
                    //                 //     Some(*value.get_value()),
                    //                 // ),
                    //                 EnumVariant::Tuple((ty, path), value) => {
                    //                     let path = path.split(PATH_SEP).collect::<Vec<&str>>();
                    //                     let mut path = VecDeque::from(path);
                    //                     let name = path.pop_front().unwrap().to_owned();
                    //                     if name.is_empty() {
                    //                         Ok((
                    //                             new_ref!(
                    //                                 Value,
                    //                                 Value::String(
                    //                                     s_read!(value).variant().to_owned()
                    //                                 )
                    //                             ),
                    //                             Some(s_read!(value).value().clone()),
                    //                         ))
                    //                     } else {
                    //                         Ok((
                    //                             new_ref!(Value, Value::String(name)),
                    //                             Some(new_ref!(
                    //                                 Value,
                    //                                 Value::Enumeration(EnumVariant::Tuple(
                    //                                     (
                    //                                         ty.clone(),
                    //                                         path.into_iter()
                    //                                             .collect::<Vec<&str>>()
                    //                                             .join(PATH_SEP)
                    //                                     ),
                    //                                     value.clone(),
                    //                                 ))
                    //                             )),
                    //                         ))
                    //                     }
                    //                 }
                    //                 _ => unimplemented!(),
                    //             },
                    //             _ => Ok((value.clone(), None)),
                    //         }
                    //     }

                    //     let mut variant = stack.pop().unwrap();
                    //     while let Ok((name, value)) = decode_expression(variant.into_pointer()) {
                    //         dbg!(&name, &value);
                    //         stack.push(name.into());
                    //         if let Some(value) = value {
                    //             variant = value.into();
                    //         } else {
                    //             break;
                    //         }
                    //     }

                    //     1
                    // }
                    Instruction::Divide => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        if trace {
                            println!("\t\t{}\t{},\t{}", Colour::Green.paint("div:"), a, b,);
                        }
                        let c = a.into_value() / b.into_value();

                        stack.push(c.into());

                        1
                    }
                    Instruction::Dup => {
                        let value = stack.pop().unwrap();
                        stack.push(value.clone());
                        stack.push(value);

                        1
                    }
                    Instruction::ExtractEnumValue => {
                        let user_enum = stack.pop().unwrap();
                        let Value::Enumeration(user_enum) = user_enum.clone().into_value() else {
                            return Err(BubbaError::VmPanic {
                                message: format!("Expeced enum, found: {user_enum:?}."),
                                location: location!(),
                            }
                            .into());
                        };

                        match user_enum {
                            EnumVariant::Unit(_, _, value) => {
                                stack.push(Value::String(value.to_owned()).into());
                            }
                            EnumVariant::Tuple(_, value) => {
                                stack.push(s_read!(value).value().clone().into());
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
                        // dbg!(
                        // &arity,
                        // &local_count,
                        // &index,
                        // fp - arity - local_count + index
                        // );
                        let value = stack[fp - arity - local_count - 3 + index].clone();
                        stack.push(value);

                        1
                    }
                    Instruction::FieldRead => {
                        let field = stack.pop().unwrap();
                        let ty_ = stack.pop().unwrap();
                        match ty_.into_value() {
                            Value::Struct(ty_) => {
                                match s_read!(ty_)
                                    .get_field_value(field.clone().into_value().to_inner_string())
                                {
                                    Some(value) => {
                                        if trace {
                                            println!(
                                                "\t\t{}\t{}",
                                                Colour::Green.paint("field_read:"),
                                                s_read!(value)
                                            );
                                        }
                                        stack.push(value.clone().into());
                                    }
                                    None => {
                                        return Err::<RefType<Value>, Error>(
                                            BubbaError::NoSuchField {
                                                field: field.to_string(),
                                                ty: s_read!(ty_).to_string(),
                                            }
                                            .into(),
                                        );
                                    }
                                }
                            }
                            value => {
                                print_stack(&stack, fp);
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::VmPanic {
                                        message: format!("FieldRead unexpected value: {value}."),
                                        location: location!(),
                                    }
                                    .into(),
                                );
                            }
                        }

                        1
                    }
                    Instruction::FieldWrite => {
                        let field = stack.pop().unwrap();
                        let ty_ = stack.pop().unwrap();
                        let value = stack.pop().unwrap();
                        match ty_.into_value() {
                            Value::Struct(ty_) => {
                                match s_write!(ty_).set_field_value(
                                    field.clone().into_value().to_inner_string(),
                                    value.clone().into_pointer().clone(),
                                ) {
                                    Some(_) => {
                                        if trace {
                                            println!(
                                                "\t\t{}\t{}",
                                                Colour::Green.paint("field_write:"),
                                                value
                                            );
                                        }
                                    }
                                    None => {
                                        return Err::<RefType<Value>, Error>(
                                            BubbaError::NoSuchField {
                                                field: field.to_string(),
                                                ty: s_read!(ty_).to_string(),
                                            }
                                            .into(),
                                        )
                                    }
                                }
                            }
                            value => {
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::VmPanic {
                                        message: format!("Unexpected value type: {value}."),
                                        location: location!(),
                                    }
                                    .into(),
                                )
                            }
                        }

                        1
                    }
                    Instruction::Goto(label) => {
                        let label = &*s_read!(label);
                        if let Some(new_ip) = self.labels.get(label) {
                            if trace {
                                println!(
                                    "\t\t{} {}",
                                    Colour::Red.bold().paint("goto"),
                                    Colour::Yellow.bold().paint(format!("0x{:08x}", new_ip + 1))
                                );
                            }
                            *new_ip as isize - ip
                        } else {
                            return Err(BubbaError::VmPanic {
                                location: location!(),
                                message: format!("Unknown label: {label}."),
                            }
                            .into());
                        }
                    }
                    Instruction::HaltAndCatchFire => {
                        let span = stack.pop().unwrap();
                        let span: std::ops::Range<usize> = span.into_value().try_into()?;

                        let file = stack.pop().unwrap();
                        let file: String = file.into_value().try_into()?;

                        return Err(BubbaError::HaltAndCatchFire { file, span }.into());
                    }
                    Instruction::Jump(offset) => {
                        if trace {
                            println!(
                                "\t\t{} {}",
                                Colour::Red.bold().paint("jmp"),
                                Colour::Yellow
                                    .bold()
                                    .paint(format!("0x{:08x}", ip + offset + 1))
                            );
                        }
                        offset + 1
                    }
                    Instruction::JumpIfFalse(offset) => {
                        let condition = stack.pop().unwrap();
                        let condition: bool = condition.into_value().try_into()?;

                        if !condition {
                            if trace {
                                println!(
                                    "\t\t{} {}",
                                    Colour::Red.bold().paint("jiff"),
                                    Colour::Yellow
                                        .bold()
                                        .paint(format!("0x{:08x}", ip + offset + 1))
                                );
                            }
                            offset + 1
                        } else {
                            1
                        }
                    }
                    Instruction::JumpIfTrue(offset) => {
                        let condition = stack.pop().unwrap();
                        let condition: bool = condition.into_value().try_into()?;
                        if condition {
                            if trace {
                                println!(
                                    "\t\t{} {}",
                                    Colour::Red.bold().paint("jift"),
                                    Colour::Yellow
                                        .bold()
                                        .paint(format!("0x{:08x}", ip + offset + 1))
                                );
                            }
                            offset + 1
                        } else {
                            1
                        }
                    }
                    Instruction::Label(_) => 1,
                    Instruction::ListIndex => {
                        let index = stack.pop().unwrap().into_value();
                        let list = stack.pop().unwrap();
                        let list = list.into_pointer();
                        let list = s_read!(list);
                        match index {
                            Value::Integer(index) => {
                                let index = index as usize;
                                if let Value::Vector { ty: _, inner: vec } = &list.clone() {
                                    let vec = s_read!(vec);
                                    if index < vec.len() {
                                        stack.push(vec[index].clone().into());
                                    } else {
                                        eprintln!("{self:?}");
                                        return Err(BubbaError::IndexOutOfBounds {
                                            index,
                                            len: vec.len(),
                                            span: self.get_span(ip),
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
                                        stack.push(
                                            Value::String(str[index..index + 1].join("")).into(),
                                        )
                                    } else {
                                        eprintln!("{self:?}");
                                        return Err(BubbaError::IndexOutOfBounds {
                                            index,
                                            len: str.len(),
                                            span: self.get_span(ip),
                                        }
                                        .into());
                                    }
                                } else {
                                    return Err(BubbaError::NotIndexable {
                                        span: self.get_span(ip),
                                        location: location!(),
                                    }
                                    .into());
                                }
                            }
                            // Value::Range(_) => {
                            //     let range: Range<usize> = index.try_into()?;
                            //     let list = eval_expression(list.clone(), context, vm)?;
                            //     let list = *list;
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
                            //                 &*index_expr.r11_x_value(&*lu_dog)[0];
                            //             let span = &*value.r63_span(&*lu_dog)[0];
                            //             let read = *span;
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
                            //             Ok(StackValue::Value(Value::String(str[range].join(""),)))
                            //         } else {
                            //             let value =
                            //                 &*index_expr.r11_x_value(&*lu_dog)[0];
                            //             let span = &*value.r63_span(&*lu_dog)[0];
                            //             let read = *span;
                            //             let span = read.start as usize..read.end as usize;

                            //             Err(ChaChaError::IndexOutOfBounds {
                            //                 index: range.end,
                            //                 len: str.len(),
                            //                 span,
                            //                 location: location!(),
                            //             })
                            //         }
                            // } else {
                            //     let value = &*list.r11_x_value(&*lu_dog)[0];
                            //     let span = &*value.r63_span(&*lu_dog)[0];
                            //     let read = *span;
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
                    Instruction::ListLength => {
                        let list = stack.pop().unwrap();
                        let list = list.into_pointer();
                        let list = s_read!(list);
                        match &*list {
                            Value::Vector { inner, .. } => {
                                let inner = s_read!(inner);
                                stack.push(Value::Integer(inner.len() as DwarfInteger).into());
                            }
                            Value::String(str) => {
                                stack.push(Value::Integer(str.len() as DwarfInteger).into());
                            }
                            _ => {
                                return Err(BubbaError::NotIndexable {
                                    span: self.get_span(ip),
                                    location: location!(),
                                }
                                .into());
                            }
                        }

                        1
                    }
                    Instruction::ListPush => {
                        let element = stack.pop().unwrap();
                        let list = stack.pop().unwrap();
                        let list = list.into_pointer();
                        match &*s_read!(list) {
                            Value::Vector { inner, .. } => {
                                let mut inner = s_write!(inner);
                                inner.push(element.into_pointer());
                            }
                            _ => {
                                return Err(BubbaError::NotIndexable {
                                    span: self.get_span(ip),
                                    location: location!(),
                                }
                                .into());
                            }
                        }

                        1
                    }
                    Instruction::MakeLambdaPointer(name, frame_size) => {
                        let captures = stack[fp - arity - local_count - 3..fp - 3]
                            .iter()
                            .cloned()
                            .map(|v| v.into_pointer())
                            .collect();
                        let name = s_read!(name).to_owned();
                        let value = Value::FubarPointer {
                            name,
                            frame_size: *frame_size,
                            captures,
                        };
                        stack.push(value.into());

                        1
                    }
                    Instruction::MethodLookup(name) => {
                        let ty = stack.pop().unwrap();

                        if let Value::Plugin(_) = ty.clone().into_value() {
                            stack.push(ty);
                            stack.push(
                                <String as Into<Value>>::into((*s_read!(name)).clone()).into(),
                            );
                        } else {
                            let ty = match ty.into_value() {
                                Value::Enumeration(variant) => match variant {
                                    EnumVariant::Struct(ty) => {
                                        let ty = s_read!(ty);
                                        let name = ty.type_name();
                                        name.to_owned()
                                    }
                                    EnumVariant::Tuple((_, ty), _) => ty.to_owned(),
                                    EnumVariant::Unit(_, ty, _) => ty.to_owned(),
                                },
                                Value::Struct(ty) => {
                                    let ty = s_read!(ty);
                                    let name = ty.type_name();
                                    name.to_owned()
                                }
                                // Value::Vector { ty, .. } => {
                                //     let ty = s_read!(ty);
                                //     let name = ty.type_name();
                                //     name.to_owned()
                                // }
                                oopsie => unreachable!("{oopsie:?}"),
                            };

                            let func = format!("{}::{}", ty, (*s_read!(name)).clone());

                            if let Some((ip, frame_size)) = self.func_map.get(&func) {
                                stack.push(Value::Integer(*ip as DwarfInteger).into());
                                stack.push(Value::Integer(*frame_size as DwarfInteger).into());
                            } else {
                                return Err(BubbaError::VmPanic {
                                    message: format!("Missing function definition: {func}"),
                                    location: location!(),
                                }
                                .into());
                            }
                        }

                        1
                    }
                    Instruction::Multiply => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        if trace {
                            println!("\t\t{}\t{},\t{}", Colour::Green.paint("mul:"), a, b,);
                        }
                        let c = a.into_value() * b.into_value();
                        stack.push(c.into());

                        1
                    }
                    Instruction::NewList(n) => {
                        if trace {
                            println!("\t\t{}\t{}", Colour::Green.paint("nl:"), n);
                        }

                        let ty = stack.pop().unwrap();
                        let ty: ValueType = ty.into_value().try_into()?;

                        if trace {
                            println!("\t\t\t\t{:?}", ty);
                        }

                        let ty = new_ref!(ValueType, ty);

                        let mut values = Vec::with_capacity(*n);

                        for _i in 0..*n {
                            let value = stack.pop().unwrap();
                            values.push(value.clone().into_pointer());
                            if trace {
                                println!("\t\t\t\t{}", value);
                            }
                        }

                        values.reverse();
                        let values = new_ref!(Vec<RefType<Value>>, values);

                        stack.push(Value::Vector { ty, inner: values }.into());

                        1
                    }
                    Instruction::NewTupleEnum(n) => {
                        if trace {
                            println!("\t\t{}\t{n}", Colour::Green.paint("nte:"));
                        }

                        let variant = stack.pop().unwrap();
                        let variant: String = variant.into_value().try_into()?;

                        if trace {
                            println!("\t\t\t\t{}", variant);
                        }

                        let path = stack.pop().unwrap();
                        let path: String = path.into_value().try_into()?;

                        if trace {
                            println!("\t\t\t\t{}", path);
                        }

                        let ty = stack.pop().unwrap();
                        let ty: ValueType = ty.into_value().try_into()?;

                        if trace {
                            println!("\t\t\t\t{:?}", ty);
                        }

                        let ty = new_ref!(ValueType, ty);

                        let mut values: Vec<RefType<Value>> = Vec::with_capacity(*n);

                        for _i in 0..*n as i32 {
                            let value = stack.pop().unwrap();
                            values.push(value.clone().into_pointer());
                            if trace {
                                println!("\t\t\t\t{}", value);
                            }
                        }

                        if n > &0usize {
                            // 🚧 This is temporary until Tuples are sorted.
                            let user_enum = TupleEnum::new(variant, values[0].to_owned());
                            let user_enum = new_ref!(TupleEnum<Value>, user_enum);
                            stack.push(
                                Value::Enumeration(EnumVariant::Tuple((ty, path), user_enum))
                                    .into(),
                            );
                        } else {
                            let user_enum = EnumVariant::Unit(ty, path, variant);
                            stack.push(Value::Enumeration(user_enum).into());
                        }

                        1
                    }
                    Instruction::NewUserType(n) => {
                        if trace {
                            println!("\t\t{}\t{n} {{", Colour::Green.paint("nut:"));
                        }

                        let name = stack.pop().unwrap();
                        let name: String = name.into_value().try_into()?;

                        if trace {
                            println!("\t\t\t\t{}", name);
                        }

                        let ty = stack.pop().unwrap();
                        let ty: ValueType = ty.into_value().try_into()?;

                        if trace {
                            println!("\t\t\t\t{:?}", ty);
                        }

                        let ty = new_ref!(ValueType, ty);

                        let mut instance = UserStruct::new(name, &ty);

                        for _i in 0..*n as i32 {
                            let name = stack.pop().unwrap();
                            let value = stack.pop().unwrap();

                            instance.define_field(
                                name.clone().into_value().to_inner_string(),
                                value.clone().into_pointer(),
                            );
                            if trace {
                                println!("\t\t\t\t{}: {}", name, value);
                            }
                        }

                        stack.push(Value::Struct(new_ref!(UserStruct<Value>, instance)).into());

                        if trace {
                            println!("\t\t\t\t}}");
                        }

                        1
                    }
                    Instruction::Not => {
                        let value = stack.pop().unwrap();
                        let value: bool = value.into_value().try_into()?;

                        stack.push(Value::Boolean(!value).into());

                        1
                    }
                    Instruction::Or => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        let a: bool = a.into_value().try_into()?;
                        let b: bool = b.into_value().try_into()?;

                        if trace {
                            println!("\t\t{}\t{},\t{}", Colour::Green.paint("or:"), a, b,);
                        }
                        let c = Value::Boolean(a || b);
                        stack.push(c.into());

                        1
                    }
                    Instruction::Out(stream) => {
                        let value = stack.pop().unwrap();
                        let value = value.into_value().to_inner_string();
                        let value = value.replace("\\n", "\n");

                        match stream {
                            0 => {
                                print!("{}", Colour::Green.paint(format!("{value}")));
                                std::io::Write::flush(&mut std::io::stdout()).unwrap();
                            }
                            1 => {
                                eprint!("{value}");
                                std::io::Write::flush(&mut std::io::stdout()).unwrap();
                            }
                            _ => {
                                return Err::<RefType<Value>, Error>(
                                    BubbaError::VmPanic {
                                        message: format!("Unknown stream: {stream}."),
                                        location: location!(),
                                    }
                                    .into(),
                                )
                            }
                        };

                        1
                    }
                    Instruction::PluginNew(arg_count) => {
                        let plugin_root = stack.pop().unwrap();
                        let plugin_root: String = plugin_root.into_value().try_into()?;

                        let mut args = Vec::with_capacity(*arg_count);
                        for _ in 0..*arg_count {
                            let arg = stack.pop().unwrap();
                            args.push(arg.into_value().into());
                        }

                        let library_path = RawLibrary::path_in_directory(
                            Path::new(&format!(
                                "{}/extensions/{}/lib",
                                self.home.display(),
                                plugin_root,
                            )),
                            plugin_root.as_str(),
                            LibrarySuffix::NoSuffix,
                        );
                        let root_module = (|| {
                            let header = lib_header_from_path(&library_path)?;
                            header.init_root_module::<PluginModRef>()
                        })()
                        .map_err(|e| {
                            eprintln!("{e}");
                            BubbaError::VmPanic {
                                message: "Plug-in error".to_owned(),
                                location: location!(),
                            }
                        })?;

                        let ctor = root_module.new();
                        let plugin = ctor(args.into()).unwrap();
                        let name = plugin.name().to_string();
                        let plugin = new_ref!(PluginType, plugin);
                        let value = Value::Plugin((name, plugin));
                        stack.push(value.into());

                        1
                    }
                    Instruction::Pop => {
                        stack.pop();

                        1
                    }
                    Instruction::Push(value) => {
                        stack.push(value.clone().into());

                        1
                    }
                    Instruction::PushArgs => {
                        let value = self.args.clone();
                        stack.push(value.into());

                        1
                    }
                    Instruction::Return => {
                        let result = stack.pop().unwrap();

                        self.captures = None;

                        // Clear the stack up to the frame pointer.
                        while stack.len() > fp + 1 {
                            stack.pop();
                        }

                        // reset the frame pointer
                        fp = match stack.pop().unwrap().into_value() {
                            Value::Integer(fp) => fp as usize,
                            Value::Empty => {
                                return Ok(result.into_pointer());
                            }
                            _ => {
                                return Err(BubbaError::VmPanic {
                                    message: format!(
                                        "Expected an integer, but got: {:?}.",
                                        stack.pop().unwrap()
                                    ),
                                    location: location!(),
                                }
                                .into());
                            }
                        };

                        let new_ip = stack.pop().unwrap();
                        let new_ip: isize = new_ip.into_value().try_into()?;

                        let frame_size = stack.pop().unwrap();
                        let frame_size: usize = frame_size.into_value().try_into()?;

                        let local_arity = stack.pop().unwrap();
                        arity = local_arity.into_value().try_into()?;

                        for _ in 0..frame_size {
                            stack.pop();
                        }

                        let frame_size = &stack[fp - 2];
                        let frame_size: usize = frame_size.clone().into_value().try_into()?;

                        // Fetch the local count from the stack, under the func addr.
                        let stack_local_count = &stack[fp - frame_size - 3 + 1];

                        // This feels pretty gross. It's trying to get the local count
                        // from the stack. If the type conversion fails, then it looks
                        // to see if there is a FubarPointer at the top of the frame
                        // and pulls the value from there.
                        //
                        // Why would there be a pointer named Fubar at the top of
                        // the frame? Well, that's what is put there when we are
                        // invoking a function on a plugin.
                        local_count = match stack_local_count.clone().into_value().try_into() {
                            Ok(local_count) => local_count,
                            Err(e) => {
                                let pointer = &stack[fp - frame_size - 3];
                                let pointer = pointer.clone().into_value();
                                if let Value::FubarPointer {
                                    name: _,
                                    frame_size,
                                    captures: _,
                                } = pointer
                                {
                                    frame_size
                                } else {
                                    return Err(e.into());
                                }
                            }
                        };

                        stack.push(result);

                        new_ip - ip + 1
                    }
                    // The fp is pointing someplace near the end of the vec.
                    // Nominally at one past the Thonk name at the bottom of the stack.
                    // Any locals will cause the fp to be moved up, with the
                    // locals existing between the Thonk name and the fp.
                    Instruction::StoreLocal(index) => {
                        let value = stack.pop().unwrap();
                        // We gotta index into the stack in reverse order from the index.
                        stack[fp - arity - local_count - 3 + index] = value;

                        1
                    }
                    Instruction::Subtract => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        let c = a.clone().into_value() - b.clone().into_value();
                        if trace {
                            println!("\t\t{}\t{},\t{}", Colour::Green.paint("sub:"), a, b,);
                        }

                        stack.push(c.into());

                        1
                    }
                    Instruction::TestEqual => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        let a = a.into_value();
                        let b = b.into_value();
                        stack.push(Value::Boolean(a == b).into());

                        1
                    }
                    Instruction::TestGreaterThan => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        stack.push(Value::Boolean(a.into_value().gt(&b.into_value())).into());

                        1
                    }
                    Instruction::TestLessThan => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        stack.push(Value::Boolean(a.into_value().lt(&b.into_value())).into());

                        1
                    }
                    Instruction::TestLessThanOrEqual => {
                        let b = stack.pop().unwrap();
                        let a = stack.pop().unwrap();
                        stack.push(Value::Boolean(a.into_value().lte(&b.into_value())).into());

                        1
                    }
                    Instruction::ToString => {
                        let value = stack.pop().unwrap();
                        let value = value.into_value().to_inner_string();
                        stack.push(Value::String(value).into());

                        1
                    }
                    Instruction::Typecast(as_ty) => {
                        let Value::ValueType(as_ty) = &*s_read!(as_ty) else {
                            return Err(BubbaError::VmPanic {
                                message: format!(
                                    "Expected a ValueType, but got: {as_ty:?}.",
                                    as_ty = *as_ty
                                ),
                                location: location!(),
                            }
                            .into());
                        };

                        let lhs = stack.pop().unwrap();
                        let lhs = lhs.into_pointer();
                        let lhs = s_read!(lhs);

                        let value = match &as_ty.subtype {
                            ValueTypeEnum::Ty(ref ty) => {
                                let ty = self.sarzak.exhume_ty(ty).unwrap();
                                let x = match &*ty.read().unwrap() {
                                    Ty::Boolean(_) => {
                                        let value: bool = (&*lhs).try_into()?;
                                        StackValue::Value(value.into())
                                    }
                                    Ty::Float(_) => {
                                        let value: f64 = (&*lhs).try_into()?;
                                        StackValue::Value(value.into())
                                    }
                                    Ty::Integer(_) => {
                                        let value: i64 = (&*lhs).try_into()?;
                                        StackValue::Value(value.into())
                                    }
                                    Ty::ZString(_) => {
                                        let value: String = (&*lhs).try_into()?;
                                        StackValue::Value(value.into())
                                    }
                                    Ty::ZUuid(_) => {
                                        let value: uuid::Uuid = (&*lhs).try_into()?;
                                        StackValue::Value(value.into())
                                    }
                                    ref alpha => {
                                        return Err(BubbaError::VmPanic {
                                            message: format!("Unexpected type: {alpha:?}.",),
                                            location: location!(),
                                        }
                                        .into())
                                    }
                                };
                                x
                            }
                            ty => {
                                return Err(BubbaError::VmPanic {
                                    message: format!("Unexpected type: {ty:?}.",),
                                    location: location!(),
                                }
                                .into())
                            }
                        };

                        stack.push(value);

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

            ip += ip_offset;
        }
    }
}

impl From<(FfiValue, &Value)> for Value {
    fn from((ffi_value, ty): (FfiValue, &Value)) -> Self {
        match ffi_value {
            FfiValue::Boolean(bool_) => Self::Boolean(bool_),
            FfiValue::Empty => Self::Empty,
            // FfiValue::Error(e) => Self::Error(e.into()),
            FfiValue::Float(num) => Self::Float(num),
            FfiValue::Integer(num) => Self::Integer(num),
            FfiValue::Option(option) => match option {
                ROption::RNone => Self::Empty,
                ROption::RSome(value) => {
                    <(FfiValue, &Value) as Into<Value>>::into((RBox::into_inner(value), ty))
                }
            },
            // FfiValue::ProxyType(plugin) => Self::ProxyType {
            //     module: plugin.module.into(),
            //     obj_ty: plugin.ty.into(),
            //     id: plugin.id.into(),
            //     plugin: new_ref!(PluginType, plugin.plugin),
            // },
            FfiValue::Range(range) => Self::Range(range.start..range.end),
            FfiValue::Result(result) => {
                let tuple = match result {
                    RResult::RErr(err) => TupleEnum {
                        variant: "Err".to_owned(),
                        value: new_ref!(
                            Value,
                            <(FfiValue, &Value) as Into<Value>>::into((RBox::into_inner(err), ty,))
                        ),
                    },
                    RResult::ROk(ok) => TupleEnum {
                        variant: "Ok".to_owned(),
                        value: new_ref!(
                            Value,
                            <(FfiValue, &Value) as Into<Value>>::into((RBox::into_inner(ok), ty))
                        ),
                    },
                };

                let Value::ValueType(ty) = ty else {
                    unreachable!()
                };

                Value::Enumeration(EnumVariant::Tuple(
                    (new_ref!(ValueType, ty.to_owned()), "Result".to_owned()),
                    new_ref!(TupleEnum<Value>, tuple),
                ))
            }
            FfiValue::String(str_) => Self::String(str_.into()),
            // FfiValue::UserType(uuid) => Self::UserType(new_ref!(UserType, uuid.into())),
            FfiValue::Uuid(uuid) => Self::Uuid(uuid.into()),
            // FfiValue::Vector(vec) => {
            //     Self::Vector(vec.into_iter().map(|v| new_ref!(Value, v.into())).collect())
            // }
            _ => panic!("Unexpected FfiValue: {ffi_value:?}."),
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
        lu_dog::ObjectStore as LuDogStore,
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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(as_int, 42);

        // let mut frame = vm.frames.pop();
        // assert_eq!(frame.ip, 2);
    }

    #[test]
    fn test_instr_add() {
        let mut thonk = Thonk::new("test".to_string());

        thonk.add_instruction(Instruction::Push(new_ref!(Value, 69.into())), None);
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 42.into())), None);
        thonk.add_instruction(Instruction::Add, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);

        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_int: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(as_int, 111);

        // let mut frame = vm.frames.pop();
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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let as_bool: bool = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert!(as_bool);

        // let mut frame = vm.frames.pop();
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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());
        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let result: String = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, "you rock!");

        // let mut frame = vm.frames.pop();
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
        program.add_symbol(
            "STRING".to_owned(),
            Value::ValueType(
                (*s_read!(ValueType::new_empty(true, &mut LuDogStore::new()))).clone(),
            ),
        );

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());

        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

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
            let struct_ty = ValueType::new_woog_struct(true, &foo, &mut lu_dog);
            let ty = Ty::new_integer(&sarzak);
            let ty = ValueType::new_ty(true, &ty, &mut lu_dog);
            let _ = Field::new("bar".to_owned(), &foo, &ty, &mut lu_dog);
            let ty = Ty::new_float(&sarzak);
            let ty = ValueType::new_ty(true, &ty, &mut lu_dog);
            let _ = Field::new("baz".to_owned(), &foo, &ty, &mut lu_dog);
            struct_ty
        };

        let ty = Ty::new_z_string(&sarzak);
        let ty = ValueType::new_ty(true, &ty, &mut s_write!(ctx.lu_dog));
        let ty = Value::ValueType((*s_read!(ty)).clone());

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
                Value::Struct(new_ref!(UserStruct<Value>, foo_inst))
            )),
            None,
        );
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "baz".into())), None);
        thonk.add_instruction(Instruction::FieldRead, None);
        thonk.add_instruction(Instruction::Return, None);
        println!("{}", thonk);
        let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
        program.add_thonk(thonk);

        program.add_symbol("STRING".to_owned(), ty);

        #[cfg(feature = "async")]
        let mut vm = VM::new(&program, &[], &PathBuf::new(), 1);
        #[cfg(not(feature = "async"))]
        let mut vm = VM::new(&program, &[], &PathBuf::new());

        let result = vm.invoke("test", &[]);
        println!("{:?}", result);
        println!("{:?}", vm);

        // assert!(vm.stack.is_empty());

        assert!(result.is_ok());

        let result: DwarfFloat = (&*s_read!(result.unwrap())).try_into().unwrap();
        assert_eq!(result, std::f64::consts::PI);
    }
}

fn print_stack(stack: &[StackValue], fp: usize) {
    for (i, entry) in stack.iter().enumerate() {
        if i == fp {
            eprint!("\t{} ->\t", Colour::Green.bold().paint("fp"));
        } else {
            eprint!("\t     \t");
        }
        eprintln!("stack {i}:\t{}", entry);
    }
}
