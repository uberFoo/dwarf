use std::{ops::Range, path::PathBuf};

#[cfg(feature = "async")]
use smol::future;

use ansi_term::Colour;
use circular_queue::CircularQueue;
use crossbeam::channel::unbounded;
use lazy_static::lazy_static;
use log::{self, log_enabled, Level::Debug};
use parking_lot::{Condvar, Mutex};
use snafu::{prelude::*, Location};
#[cfg(feature = "tracy")]
use tracy_client::{span, Client};
use uuid::Uuid;

#[cfg(feature = "async")]
use puteketeke::Executor;

use crate::{
    bubba::{Instruction, Program, Thonk, VM},
    chacha::{
        error::{Error, Result, UnimplementedSnafu},
        memory::{Memory, MemoryUpdateMessage},
        value::UserStruct,
    },
    lu_dog::{
        Block, Expression, ExpressionEnum, LocalVariable, ObjectStore as LuDogStore, Span,
        Statement, StatementEnum, ValueType, ValueTypeEnum, Variable, XValue,
    },
    new_ref, s_read, s_write, ChaChaError, Context as ExtruderContext, Dirty, DwarfInteger,
    ModelStore, NewRef, RefType, Value,
};

mod banner;
mod context;
mod expression;
mod func_call;
mod lambda;
mod pvt;
mod repl;
mod statement;
mod tui;

pub use banner::banner2;
pub(crate) use pvt::PrintableValueType;

#[cfg(feature = "repl")]
pub use repl::start_repl;

#[cfg(all(
    feature = "tui",
    not(any(feature = "single", feature = "single-vec", feature = "multi-nd-vec"))
))]
pub use tui::start_tui_repl;

use context::Context;
use expression::{
    block, call, debugger, field, for_loop, if_expr, index, list, literal, match_expr, operator,
    print, range, ret, struct_expr, typecast, variable,
};
use func_call::eval_function_call;
use lambda::eval_lambda_expression;

macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        name.strip_suffix("::f").unwrap()
    }};
}
pub(crate) use function;

macro_rules! trace {
    ($($arg:tt)*) => {
        log::trace!(
            target: "chacha",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
pub(crate) use trace;

macro_rules! debug {
    ($($arg:tt)*) => {
        log::debug!(
            target: "chacha",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Cyan.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
pub(crate) use debug;

macro_rules! error {
    ($($arg:tt)*) => {
        log::error!(
            target: "chacha",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Red.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
pub(crate) use error;

const TIMING_COUNT: usize = 1_000;
const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

lazy_static! {
    pub(super) static ref RUNNING: Mutex<bool> = Mutex::new(true);
    pub(super) static ref CVAR: Condvar = Condvar::new();
    pub(crate) static ref STEPPING: Mutex<bool> = Mutex::new(false);
    pub(super) static ref EXEC_MUTEX: Mutex<()> = Mutex::new(());
}

pub fn shutdown_interpreter() {
    let mut running = RUNNING.lock();
    *running = false;
    CVAR.notify_all();
}

/// Initialize the interpreter
///
pub fn initialize_interpreter(
    thread_count: usize,
    dwarf_home: PathBuf,
    e_context: ExtruderContext,
) -> Result<Context, Error> {
    debug!("initialize_interpreter with {thread_count} threads");

    let mut lu_dog = s_write!(e_context.lu_dog);
    let sarzak = s_read!(e_context.sarzak);

    // Initialize the stack with stuff from the compiled source.
    let block = Block::new(false, Uuid::new_v4(), None, None, &mut lu_dog);
    let (mut stack, receiver) = Memory::new();

    // We don't really care about the dirty flag because we are just stuffing
    // everything in below.
    let dirty = vec![];

    // Insert the functions in the root frame.
    let funcs = lu_dog.iter_function().collect::<Vec<_>>();
    for func in funcs {
        inter_func(func, &block, &mut stack, &mut lu_dog);
    }

    // Insert static methods for each struct. They go into the meta table.
    for user_type in lu_dog.iter_woog_struct() {
        inter_struct(user_type, &mut stack, &lu_dog);
    }

    // Insert static methods for each store. They go into the meta table.
    for store in lu_dog.iter_z_object_store() {
        inter_store(store, &mut stack, &lu_dog);
    }

    let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
    let ty = crate::sarzak::Ty::new_z_string(&sarzak);
    let ty = ValueType::new_ty(true, &ty, &mut lu_dog);
    let ty = Value::ValueType((*s_read!(ty)).clone());
    program.add_symbol("STRING".to_owned(), ty);

    if let Some(_id) = lu_dog.exhume_woog_struct_id_by_name("Complex") {
        // Hack to try to get mandelbrot running faster...
        let mut thonk = Thonk::new("norm_squared".to_string());

        // This is the function we are coding.
        // fn norm_squared(self: Complex) -> float {
        //     // This would be a direct translation on the assembly
        //     // below. But it's not what I want.
        //     asm!(
        //         "fetch 0",
        //         "push \"re\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "fetch 0",
        //         "push \"im\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "add",
        //         "ret"
        //     );
        //
        //     let result:Complex;
        //
        //     asm!(
        //         "fetch {self}",
        //         "push \"re\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "fetch {self}",
        //         "push \"im\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "add",
        //         "pop {result}"
        //     );
        //
        //     result
        //
        //     //self.re * self.re + self.im * self.im
        // }

        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())), None);
        //
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // dup
        thonk.add_instruction(Instruction::Dup, None);
        // mul
        thonk.add_instruction(Instruction::Multiply, None);
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())), None);
        //
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // dup
        thonk.add_instruction(Instruction::Dup, None);
        // mul
        thonk.add_instruction(Instruction::Multiply, None);
        // add
        thonk.add_instruction(Instruction::Add, None);
        thonk.add_instruction(Instruction::Return, None);

        thonk.increment_frame_size();

        program.add_thonk(thonk);

        // Hack to try to get mandelbrot running faster...
        let mut thonk = Thonk::new("add".to_string());

        // This is the function we are coding.
        // fn add(self: Complex, other: Complex) -> Complex {
        //     Complex {
        //         re: self.re + other.re,
        //         im: self.im + other.im,
        //     }
        // }

        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())), None);
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // Get the second parameter off the stack
        thonk.add_instruction(Instruction::FetchLocal(1), None);
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())), None);
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // add
        thonk.add_instruction(Instruction::Add, None);
        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())), None);
        // Write field
        thonk.add_instruction(Instruction::FieldWrite, None);
        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())), None);
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // Get the second parameter off the stack
        thonk.add_instruction(Instruction::FetchLocal(1), None);
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())), None);
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // add
        thonk.add_instruction(Instruction::Add, None);
        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())), None);
        // Write field
        thonk.add_instruction(Instruction::FieldWrite, None);
        // // new
        // let ty = lu_dog.exhume_value_type(&id).unwrap();
        // thonk.add_instruction(Instruction::NewUserType("Complex".to_string(), ty, 2));
        thonk.add_instruction(Instruction::Return, None);

        thonk.increment_frame_size();
        thonk.increment_frame_size();

        program.add_thonk(thonk);

        // Hack to try to get mandelbrot running faster...
        let mut thonk = Thonk::new("square".to_string());

        // This is the function we are coding.
        // fn square(self: Complex) -> Complex {
        //     Complex {
        //         re: self.re * self.re - self.im * self.im,
        //         im: 2.0 * self.re * self.im,
        //     }
        // }

        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())), None);
        //
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // dup
        thonk.add_instruction(Instruction::Dup, None);
        // mul
        thonk.add_instruction(Instruction::Multiply, None);
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())), None);
        //
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // dup
        thonk.add_instruction(Instruction::Dup, None);
        // mul
        thonk.add_instruction(Instruction::Multiply, None);
        // sub
        thonk.add_instruction(Instruction::Subtract, None);
        // push {fp + 0}
        //  this is the one for write
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())), None);
        // 2.0 * self.re * self.im
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())), None);
        //
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())), None);
        //
        // field
        thonk.add_instruction(Instruction::FieldRead, None);
        // push 2.0
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 2.0.into())), None);
        // mul
        thonk.add_instruction(Instruction::Multiply, None);
        // mul
        thonk.add_instruction(Instruction::Multiply, None);
        // push {fp + 0}
        thonk.add_instruction(Instruction::FetchLocal(0), None);
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())), None);
        thonk.add_instruction(Instruction::FieldWrite, None);
        thonk.add_instruction(Instruction::FieldWrite, None);
        // // new
        // let ty = lu_dog.exhume_value_type(&id).unwrap();
        // thonk.add_instruction(Instruction::NewUserType("Complex".to_string(), ty, 2));

        thonk.add_instruction(Instruction::Return, None);

        program.add_thonk(thonk);
    }

    let (std_out_send, std_out_recv) = unbounded();

    #[cfg(feature = "tracy")]
    Client::start();

    #[cfg(feature = "async")]
    {
        let executor = Executor::new(thread_count);

        Ok(Context::new(
            format!("{} ", Colour::Blue.normal().paint("道:>")),
            block,
            stack,
            e_context.lu_dog.clone(),
            e_context.sarzak.clone(),
            new_ref!(ModelStore, e_context.models),
            receiver,
            std_out_send,
            std_out_recv,
            None,
            CircularQueue::with_capacity(TIMING_COUNT),
            0,
            0,
            None,
            dwarf_home,
            dirty,
            e_context.source.to_owned(),
            program,
            executor,
            e_context.scopes,
            e_context.imports,
        ))
    }
    #[cfg(not(feature = "async"))]
    Ok(Context::new(
        format!("{} ", Colour::Blue.normal().paint("道:>")),
        block,
        stack,
        e_context.lu_dog.clone(),
        e_context.sarzak.clone(),
        new_ref!(ModelStore, e_context.models),
        receiver,
        std_out_send,
        std_out_recv,
        None,
        CircularQueue::with_capacity(TIMING_COUNT),
        0,
        0,
        None,
        dwarf_home,
        dirty,
        e_context.source.to_owned(),
        program,
        e_context.scopes,
        e_context.imports,
    ))
}

#[allow(unused_variables)]
fn chacha_print<S: AsRef<str>>(result: S, context: &mut Context) -> Result<()> {
    let result_style = Colour::Green.bold();
    cfg_if::cfg_if! {
        if #[cfg(feature = "print-std-out")] {
            print!("{}", result_style.paint(result.as_ref()));
            std::io::Write::flush(&mut std::io::stdout()).unwrap();
        } else {
            context
                .std_out_send()
                .send(format!("{}", result_style.paint(result.as_ref())))
                .context(crate::chacha::error::InternalCompilerChannelSnafu {
                    message: "error writing to std out queue".to_owned(),
                })?;
        }
    }
    Ok(())
}

fn eval_expression(
    expression: RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    // context.tracy.span(span_location!("eval_expression"), 0);
    // context
    //     .tracy
    //     .non_continuous_frame(frame_name!("eval_expression"));
    #[cfg(feature = "tracy")]
    span!("eval_expression");

    {
        let mut running = RUNNING.lock();
        if !*running {
            if let Some(sender) = &context.debug_status_writer() {
                let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                debug!("value {value:#?}");

                let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                let read = s_read!(span);
                let span = read.start as usize..read.end as usize;
                sender.send(DebuggerStatus::Paused(span)).unwrap();
            }
            debug!("waiting");
            CVAR.wait(&mut running);
            debug!("notified");
        }

        if *STEPPING.lock() {
            debug!("stepping");
            *running = false;
        }

        debug!("running: {expression:#?}");
        trace!("stack: {:#?}", context.memory());
    }

    // Timing goodness
    context.increment_expression_count(1);

    // This is nifty. With the `exec` target you get to see the expression
    // being evaluated.
    if log_enabled!(target: "exec", Debug) {
        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
        let span = s_read!(span);
        let span = span.start as usize..span.end as usize;
        let source = context.source();
        log::debug!(target: "exec", "`{}`", source[span].to_owned());
    }

    match s_read!(expression).subtype {
        #[cfg(feature = "async")]
        ExpressionEnum::AWait(ref expression) => {
            debug!("evaluating await");
            let expr = s_read!(lu_dog).exhume_a_wait(expression).unwrap();
            let expr = s_read!(expr).r98_expression(&s_read!(lu_dog))[0].clone();

            let mut child = context.clone();
            let value = eval_expression(expr, &mut child, vm)?;

            let executor = context.executor();

            let mut write_value = s_write!(value);
            match &mut *write_value {
                Value::Future {
                    name: _,
                    task,
                    executor,
                } => {
                    if let Some(task) = task.take() {
                        executor.start_task(&task);
                        future::block_on(task)
                    } else {
                        panic!("Who took my task!?");
                    }
                }
                Value::Task {
                    worker: _,
                    parent: None,
                } => Ok(value.clone()),
                Value::Task {
                    worker: None,
                    parent: Some(parent),
                } => {
                    executor.start_task(parent);
                    future::block_on(parent)
                }
                Value::Task {
                    worker: Some(worker),
                    // worker: _,
                    parent: Some(parent),
                } => {
                    worker.start_task(parent);
                    future::block_on(parent)
                }
                huh => {
                    unreachable!("{huh}")
                }
            }
            // Ok(new_ref!(Value, Value::Empty))
        }
        ExpressionEnum::Block(ref block) => block::eval(block, context, vm),
        ExpressionEnum::Call(ref call) => call::eval(call, &expression, context, vm),
        ExpressionEnum::XDebugger(_) => debugger::eval(context),
        ExpressionEnum::EmptyExpression(_) => Ok(new_ref!(Value, Value::Empty)),
        // ExpressionEnum::EnumField(ref enum_field) => enumeration::eval(enum_field, context, vm),
        ExpressionEnum::FieldAccess(ref field) => field::field_access::eval(field, context, vm),
        ExpressionEnum::FieldExpression(ref field_expr) => {
            field::field_expression::eval(field_expr, context, vm)
        }
        ExpressionEnum::ForLoop(ref for_loop) => for_loop::eval(for_loop, context, vm),
        ExpressionEnum::Index(ref index) => index::eval_index(index, context, vm),
        //
        // Lambda
        //
        // I know it's called eval_expression, but we don't actually want to
        // evaluate the lambda, we just want to return it.
        // It's eventually evaluated by ExpressionEnum::Call.
        //
        ExpressionEnum::Lambda(ref lambda) => {
            let lambda = s_read!(lu_dog).exhume_lambda(lambda).unwrap();
            Ok(new_ref!(Value, Value::Lambda(lambda)))
        }
        ExpressionEnum::ListElement(ref element) => list::eval_list_element(element, context, vm),
        ExpressionEnum::ListExpression(ref list) => list::eval_list_expression(list, context, vm),
        ExpressionEnum::Literal(ref literal) => literal::eval(literal, context),
        ExpressionEnum::Operator(ref operator) => {
            operator::eval_operator(operator, &expression, context, vm)
        }
        ExpressionEnum::XPrint(ref print) => print::eval(print, context, vm),
        ExpressionEnum::RangeExpression(ref range) => range::eval_range(range, context, vm),
        ExpressionEnum::StructExpression(ref expr) => struct_expr::eval(expr, context, vm),
        ExpressionEnum::TypeCast(ref expr) => typecast::eval(expr, context, vm),
        ExpressionEnum::VariableExpression(ref expr) => variable::eval(expr, &expression, context),
        ExpressionEnum::XIf(ref expr) => if_expr::eval_if_expression(expr, context, vm),
        ExpressionEnum::XMatch(ref expr) => match_expr::eval(expr, context, vm),
        ExpressionEnum::XReturn(ref expr) => ret::eval(expr, context, vm),
        ref alpha => {
            ensure!(
                false,
                UnimplementedSnafu {
                    message: format!("Hey! Implement expression: {:?}!", alpha),
                }
            );

            Ok(new_ref!(Value, Value::Empty))
        }
    }
}

pub fn eval_statement(
    statement: RefType<Statement>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    debug!("eval_statement statement {statement:?}");
    trace!("eval_statement stack {:?}", context.memory());

    #[cfg(feature = "tracy")]
    span!("eval_statement");

    // This is the entrypoint from the REPL, which is where the dirty thing comes
    // into play.
    for dirty in context.dirty() {
        match dirty {
            Dirty::Func(f) => inter_func(
                f.clone(),
                &context.block().clone(),
                context.memory(),
                &mut s_write!(lu_dog),
            ),
            Dirty::Store(ref s_id) => {
                let store = s_read!(lu_dog).exhume_z_object_store(s_id).unwrap();
                inter_store(store, context.memory(), &s_read!(lu_dog));
            }
            Dirty::Struct(s) => inter_struct(s.clone(), context.memory(), &s_read!(lu_dog)),
            foobar => {
                dbg!(foobar);
            }
        }
    }

    context.clear_dirty();

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&s_read!(lu_dog))[0].clone();
            let _value = eval_expression(expr, context, vm)?;

            Ok(new_ref!(Value, Value::Empty))
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_let_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            debug!("StatementEnum::LetStatement: stmt {stmt:?}");

            let expr = stmt.r20_expression(&s_read!(lu_dog))[0].clone();
            debug!("expr {expr:?}");

            let value = eval_expression(expr, context, vm)?;
            debug!("value {value:?}");

            let var = s_read!(stmt.r21_local_variable(&s_read!(lu_dog))[0]).clone();
            let var = s_read!(var.r12_variable(&s_read!(lu_dog))[0]).clone();

            debug!("allocating space for  `{} = {}`", var.name, s_read!(value));
            context.memory().insert(var.name, value);

            Ok(new_ref!(Value, Value::Empty))
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            debug!("StatementEnum::ResultStatement: stmt {stmt:?}");

            let expr = stmt.r41_expression(&s_read!(lu_dog))[0].clone();
            debug!("StatementEnum::ResultStatement expr {expr:?}");

            let value = eval_expression(expr, context, vm)?;
            debug!("StatementEnum::ResultStatement value {value:?}");

            Ok(value)
        }
        StatementEnum::ItemStatement(_) => Ok(new_ref!(Value, Value::Empty)),
    }
}

#[derive(Debug)]
pub enum DebuggerStatus {
    Error(String),
    Paused(Range<usize>),
    Running,
    StdOut(String),
    Stopped(RefType<Value>, RefType<ValueType>),
}

pub enum DebuggerControl {
    ExecuteInput(String),
    Run,
    SetBreakpoint(usize),
    StepInto,
    StepOver,
    Stop,
}

/// This runs the main function, assuming it exists.
pub fn start_func(
    name: &str,
    stopped: bool,
    context: &mut Context,
) -> Result<RefType<Value>, Error> {
    {
        let mut running = RUNNING.lock();
        *running = !stopped;
    }

    let dwarf_home = context.get_home().clone();
    let mut program = context.get_program().clone();
    let ty = crate::sarzak::Ty::new_z_string(&s_read!(context.sarzak_heel()));
    let ty = ValueType::new_ty(true, &ty, &mut s_write!(context.lu_dog_heel()));
    let ty = Value::ValueType((*s_read!(ty)).clone());
    program.add_symbol("STRING".to_owned(), ty);

    let stack = context.memory();
    let mut vm = VM::new(&program, &[], &dwarf_home);

    if let Some(main) = stack.get(name) {
        // This should fail if it's not a function. Actually, I think that it _has_
        // to be a function. Unless there's another named item that I'm not thinking
        // of. I mean, maybe `use main;`  would trigger this to return OK(()), and
        // not do anything?
        if let Value::Function(ref main) = *s_read!(main) {
            let main = s_read!(context.lu_dog_heel())
                .exhume_function(&s_read!(main).id)
                .unwrap();

            let value_ty = &s_read!(main).r1_value_type(&s_read!(context.lu_dog_heel()))[0];
            let span = &s_read!(value_ty).r62_span(&s_read!(context.lu_dog_heel()))[0];

            let result = eval_function_call(main, &[], None, true, span, context, &mut vm)?;

            #[allow(clippy::redundant_clone)]
            //              ^^^^^^^^^^^^^^^ : It's not redundant.
            // The macro is just hiding the fact that it isn't.
            // This is redundant: the above is not redundant.
            Ok(result)
        } else {
            Err(Error(ChaChaError::MainIsNotAFunction))
        }
    } else {
        Err(Error(ChaChaError::NoMainFunction))
    }
}

pub fn start_vm(n: DwarfInteger) -> Result<DwarfInteger, Error> {
    let mut thonk = Thonk::new("fib".to_string());

    let fib = new_ref!(String, "fib".into());

    // Get the parameter off the stack
    // push {fp + 0}
    thonk.add_instruction(Instruction::FetchLocal(0), None);
    // push 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())), None);
    // Check if it's <= 1
    // lte
    thonk.add_instruction(Instruction::TestLessThanOrEqual, None);
    // jne
    thonk.add_instruction(Instruction::JumpIfFalse(2), None);
    // If false return 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())), None);
    thonk.add_instruction(Instruction::Return, None);
    // return fib(n-1) + fib(n-2)
    // Load fib
    thonk.add_instruction(Instruction::CallDestination(fib.clone()), None);
    thonk.add_instruction(Instruction::LocalCardinality(fib.clone()), None);
    // load n
    thonk.add_instruction(Instruction::FetchLocal(0), None);
    // load 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())), None);
    // subtract
    thonk.add_instruction(Instruction::Subtract, None);
    // Call fib(n-1)
    thonk.add_instruction(Instruction::Call(1), None);
    // load fib
    thonk.add_instruction(Instruction::CallDestination(fib.clone()), None);
    thonk.add_instruction(Instruction::LocalCardinality(fib.clone()), None);
    // load n
    thonk.add_instruction(Instruction::FetchLocal(0), None);
    // load 2
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 2.into())), None);
    // subtract
    thonk.add_instruction(Instruction::Subtract, None);
    // Call fib(n-1)
    thonk.add_instruction(Instruction::Call(1), None);
    // add
    thonk.add_instruction(Instruction::Add, None);
    thonk.add_instruction(Instruction::Return, None);

    thonk.increment_frame_size();

    let mut program = crate::bubba::Program::new("".to_owned(), "".to_owned());
    program.add_thonk(thonk);

    use crate::sarzak::{ObjectStore as SarzakStore, Ty, MODEL as SARZAK_MODEL};
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let mut lu_dog = LuDogStore::new();

    // We need to stuff all of the sarzak types into the store.
    ValueType::new_ty(true, &Ty::new_boolean(&sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_float(&sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_integer(&sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_z_string(&sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_z_uuid(&sarzak), &mut lu_dog);

    let ty = Ty::new_z_string(&sarzak);
    let ty = ValueType::new_ty(true, &ty, &mut lu_dog);
    let ty = Value::ValueType((*s_read!(ty)).clone());
    program.add_symbol("STRING".to_owned(), ty);

    let mut vm = VM::new(&program, &[], &PathBuf::new());
    let result = vm.invoke("fib", &[new_ref!(Value, n.into())]);

    // vm.pop_stack();
    // vm.pop_stack();

    let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();

    Ok(result)
}

fn typecheck(
    lhs: &RefType<ValueType>,
    rhs: &RefType<ValueType>,
    span: &RefType<Span>,
    location: Location,
    context: &Context,
) -> Result<()> {
    cfg_if::cfg_if! {
        if #[cfg(any(feature = "single", feature = "single-vec", feature = "single-vec-tracy", feature = "debug"))] {
            if std::rc::Rc::as_ptr(lhs) == std::rc::Rc::as_ptr(rhs) {
                return Ok(());
            }
        } else {
            if std::sync::Arc::as_ptr(lhs) == std::sync::Arc::as_ptr(rhs) {
                return Ok(());
            }
        }
    }

    let (lhs_t, rhs_t) = (&s_read!(lhs).subtype, &s_read!(rhs).subtype);

    // If it's a lambda we test the function signature: return type, and parameters.
    // 🚧 looks like I'm only testing the return type.
    if let ValueTypeEnum::Lambda(l) = lhs_t {
        if let ValueTypeEnum::Lambda(r) = rhs_t {
            let l = s_read!(context.lu_dog_heel()).exhume_lambda(l).unwrap();
            let r = s_read!(context.lu_dog_heel()).exhume_lambda(r).unwrap();
            let lrt = s_read!(l).return_type;
            let rrt = s_read!(r).return_type;
            let l = s_read!(context.lu_dog_heel())
                .exhume_value_type(&lrt)
                .unwrap();
            let r = s_read!(context.lu_dog_heel())
                .exhume_value_type(&rrt)
                .unwrap();
            let l = &s_read!(l).subtype;
            let r = &s_read!(r).subtype;
            if l == r {
                return Ok(());
            }
        }
    }

    if let ValueTypeEnum::Enumeration(a) = lhs_t {
        if let ValueTypeEnum::Enumeration(b) = rhs_t {
            let lu_dog = context.lu_dog_heel();
            let lu_dog = s_read!(lu_dog);

            let a = lu_dog.exhume_enumeration(a).unwrap();
            let b = lu_dog.exhume_enumeration(b).unwrap();
            let a = s_read!(a);
            let b = s_read!(b);

            let a_name = if let Some(next) = a.name.split('<').next() {
                next
            } else {
                &a.name
            };
            let b_name = if let Some(next) = b.name.split('<').next() {
                next
            } else {
                &b.name
            };

            if a_name == b_name {
                return Ok(());
            }
        }
    }

    // Checking for proxy type/woog struct equivalence.
    if let ValueTypeEnum::WoogStruct(rhs_id) = rhs_t {
        if let ValueTypeEnum::Ty(lhs_id) = lhs_t {
            let woog_struct = s_read!(context.lu_dog_heel())
                .exhume_woog_struct(rhs_id)
                .unwrap();
            if s_read!(woog_struct).object == Some(*lhs_id) {
                return Ok(());
            }
        }
    }

    if matches!(
        lhs_t,
        ValueTypeEnum::FuncGeneric(_)
            | ValueTypeEnum::EnumGeneric(_)
            | ValueTypeEnum::StructGeneric(_)
    ) {
        return Ok(());
    }

    if lhs_t == rhs_t {
        Ok(())
    } else {
        let lhs = PrintableValueType(true, lhs.to_owned(), context.models());
        let rhs = PrintableValueType(true, rhs.to_owned(), context.models());
        Err(ChaChaError::TypeMismatch {
            expected: lhs.to_string(),
            found: rhs.to_string(),
            span: s_read!(span).start as usize..s_read!(span).end as usize,
            location,
        })
    }
}

fn inter_func(
    func: RefType<crate::lu_dog::Function>,
    block: &RefType<Block>,
    stack: &mut Memory,
    lu_dog: &mut LuDogStore,
) {
    let imp = s_read!(func).r9_implementation_block(lu_dog);
    if imp.is_empty() {
        let name = s_read!(func).name.clone();
        let value = Value::Function(func.clone());
        let ty = s_read!(func).r1_value_type(lu_dog)[0].clone();

        // Build the local in the AST.
        let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
        let var = Variable::new_local_variable(name.clone(), &local, lu_dog);
        let _value = XValue::new_variable(block, &ty, &var, lu_dog);

        trace!("inserting local function {}", name);
        stack.insert(name, new_ref!(Value, value));
    }
}

fn inter_struct(
    woog_struct: RefType<crate::lu_dog::WoogStruct>,
    stack: &mut Memory,
    lu_dog: &LuDogStore,
) {
    let woog_struct = s_read!(woog_struct);
    // Create a meta table for each struct.
    debug!("inserting struct in meta table {}", woog_struct.name);
    stack.insert_meta_table(woog_struct.name.to_owned());
    let impl_ = woog_struct.r8c_implementation_block(lu_dog);
    if !impl_.is_empty() {
        // For each function in the impl, insert the function. I should probably
        // check and only insert the static functions.
        for func in s_read!(impl_[0]).r9_function(lu_dog) {
            let insert = if let Some(param) = s_read!(func).r82_parameter(lu_dog).first() {
                let var = &s_read!(param).r12_variable(lu_dog)[0];
                let var = s_read!(var);
                var.name != "self"
            } else {
                true
            };

            if insert {
                debug!("inserting static function {}", s_read!(func).name);
                stack.insert_meta(
                    &woog_struct.name,
                    s_read!(func).name.to_owned(),
                    new_ref!(Value, Value::Function(func.clone(),)),
                )
            }
        }
    }
}

fn inter_store(
    store: RefType<crate::lu_dog::ZObjectStore>,
    stack: &mut Memory,
    lu_dog: &LuDogStore,
) {
    let store = s_read!(store);
    // Create a meta table for each struct.
    debug!("inserting store in meta table {}", store.domain);
    stack.insert_meta_table(store.name.to_owned());
    let impl_ = store.r83c_implementation_block(lu_dog);
    if !impl_.is_empty() {
        // For each function in the impl, insert the function. I should probably
        // check and only insert the static functions.
        // 🚧 Only insert the static functions
        for func in s_read!(impl_[0]).r9_function(lu_dog) {
            let insert = if let Some(param) = s_read!(func).r82_parameter(lu_dog).first() {
                let var = &s_read!(param).r12_variable(lu_dog)[0];
                let var = s_read!(var);
                var.name != "self"
            } else {
                true
            };

            if insert {
                debug!("inserting static function {}", s_read!(func).name);
                stack.insert_meta(
                    &store.name,
                    s_read!(func).name.to_owned(),
                    new_ref!(Value, Value::Function(func.clone(),)),
                )
            }
        }
    }
}
