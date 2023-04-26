use std::{
    fmt, io,
    mem::transmute,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use clap::Args;
use fxhash::FxHashMap as HashMap;
use heck::ToUpperCamelCase;
use lazy_static::lazy_static;
use log;
use sarzak::{
    dwarf::{inter_statement, parse_line},
    lu_dog::{
        Argument, Block, CallEnum, Empty, Expression, Function, Literal, LocalVariable,
        ObjectStore as LuDogStore, Statement, StatementEnum, Value as LuDogValue, ValueType,
        Variable, WoogOptionEnum, ZObjectStore,
    },
    merlin::types::{Inflection, Point},
    sarzak::{store::ObjectStore as SarzakStore, types::Ty, SUuid},
};
use serde::{Deserialize, Serialize};
use snafu::{prelude::*, Location};
use uuid::{uuid, Uuid};

#[cfg(feature = "repl")]
use rustyline::{error::ReadlineError, DefaultEditor};

lazy_static! {
    pub(crate) static ref MODEL: Arc<RwLock<SarzakStore>> =
        Arc::new(RwLock::new(SarzakStore::new()));
}

const ERR_CLR: Colour = Colour::Red;
const OK_CLR: Colour = Colour::Green;
const POP_CLR: Colour = Colour::Yellow;
const OTH_CLR: Colour = Colour::Cyan;

#[derive(Debug, Snafu)]
pub struct Error(InnerError);

#[derive(Debug, Snafu)]
enum InnerError {
    #[snafu(display("\n{}: {message}\n  --> {}:{}:{}", ERR_CLR.paint("error"), location.file, location.line, location.column))]
    Unimplemented {
        message: String,
        location: Location,
    },
    #[snafu(display("\n{}: could not find static method `{}::{}`.", ERR_CLR.paint("error"), OTH_CLR.paint(ty), OTH_CLR.paint(method)))]
    NoSuchStaticMethod {
        method: String,
        ty: String,
    },
    #[snafu(display("\n{}: wrong number of arguments. Expected `{}`, found `{}`.", ERR_CLR.paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.paint(got.to_string())))]
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
    },
    RustyLine {
        source: ReadlineError,
    },
    Store {
        source: io::Error,
    },
}

type Result<T, E = InnerError> = std::result::Result<T, E>;

macro_rules! error {
    ($arg:expr) => {
        log::error!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::error!(
            "{} --> {:?}\n  --> {}:{}:{}",
            Colour::Red.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

macro_rules! debug {
    ($arg:expr) => {
        log::debug!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::debug!(
            "{} --> {:?}\n  --> {}:{}:{}",
            Colour::Yellow.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

macro_rules! no_debug {
    ($arg:expr) => {
        log::debug!("{}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::debug!(
            "{} --> {}\n  --> {}:{}:{}",
            Colour::Yellow.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

macro_rules! trace {
    ($arg:expr) => {
        log::trace!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::trace!(
            "{} --> {:?}\n  --> {}:{}:{}",
            Colour::Yellow.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

#[derive(Args, Clone, Debug, Deserialize, Serialize)]
pub struct ChaChaOptions {
    /// Lu-Dog Source Store
    ///
    /// Path to the store.
    source: PathBuf,
    /// Model File
    ///
    /// Path to the model, corresponding to the source file, to build the
    /// Lu-Dog domain.
    model: PathBuf,
    /// Meta-Model File
    ///
    /// Path to the meta-model, sarzak.
    sarzak: PathBuf,
}

fn main() -> Result<()> {
    pretty_env_logger::init();

    let style = Colour::Purple;

    let banner = r#"
   ________          ________             _       __                            __
  / ____/ /_  ____ _/ ____/ /_  ____ _   (_)___  / /____  _________  ________  / /____  _____
 / /   / __ \/ __ `/ /   / __ \/ __ `/  / / __ \/ __/ _ \/ ___/ __ \/ ___/ _ \/ __/ _ \/ ___/
/ /___/ / / / /_/ / /___/ / / / /_/ /  / / / / / /_/  __/ /  / /_/ / /  /  __/ /_/  __/ /
\____/_/ /_/\__,_/\____/_/ /_/\__,_/  /_/_/ /_/\__/\___/_/  / .___/_/   \___/\__/\___/_/
                        __              __                 /_/____
   ____ ___  ____  ____/ /__  _    ____/ /      ______ ______/ __/
  / __ `__ \/ __ \/ __  / _ \(_)  / __  / | /| / / __ `/ ___/ /_
 / / / / / / /_/ / /_/ /  __/    / /_/ /| |/ |/ / /_/ / /  / __/
/_/ /_/ /_/\____/\__,_/\___(_)   \__,_/ |__/|__/\__,_/_/  /_/
                                                                                             "#;
    println!("{}", style.paint(banner));

    let sarzak = SarzakStore::load("../sarzak/models/sarzak.v2.json")
        .map_err(|e| InnerError::Store { source: e })?;

    // This will always be a lu-dog, but it's basically a compiled dwarf file.
    // let mut lu_dog = LuDogStore::load("../sarzak/target/sarzak/lu_dog")
    let mut lu_dog = LuDogStore::load("../sarzak/target/sarzak/merlin")
        .map_err(|e| InnerError::Store { source: e })?;

    // This won't always be Lu-Dog, clearly. So we'll need to be sure to also
    // generate some code that imports the types from the model.
    // let model = SarzakStore::load("../sarzak/models/lu_dog.v2.json")
    let model = SarzakStore::load("../sarzak/models/merlin.v2.json")
        .map_err(|e| InnerError::Store { source: e })?;

    // This needs to be in a block so that global is dropped.
    {
        let mut global = MODEL.write().unwrap();
        *global = model;
    }

    #[cfg(feature = "repl")]
    do_repl(&mut lu_dog, &sarzak).map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })
}

fn eval_function_call(
    func: Arc<RwLock<Function>>,
    args: &[Arc<RwLock<Argument>>],
    stack: &mut Stack,
    lu_dog: &LuDogStore,
    sarzak: &SarzakStore,
) -> Result<(Value, Arc<RwLock<ValueType>>)> {
    debug!("eval_function_call func ", func);
    trace!("eval_function_call stack", stack);

    let func = func.read().unwrap().to_owned();
    let block = lu_dog
        .exhume_block(&func.block)
        .unwrap()
        .read()
        .unwrap()
        .clone();
    let stmts = block.r18_statement(&lu_dog);

    if stmts.len() > 0 {
        stack.push();

        // We need to evaluate the arguments, and then push them onto the stack. We
        // also need to typecheck the arguments against the function parameters.
        // We need to look the params up anyway to set the local variables.
        let params = func.r13_parameter(&lu_dog);
        // Damn, this really needs to return a Result.
        ensure!(
            params.len() == args.len(),
            WrongNumberOfArgumentsSnafu {
                expected: params.len(),
                got: args.len()
            }
        );

        let params = if params.len() > 0 {
            let mut params = Vec::with_capacity(params.len());
            let mut next = func
                // .clone()
                .r13_parameter(&lu_dog)
                .iter()
                .find(|p| p.read().unwrap().r14c_parameter(&lu_dog).len() == 0)
                .unwrap()
                .clone();

            loop {
                let var = next.read().unwrap().r12_variable(&lu_dog)[0]
                    .read()
                    .unwrap()
                    .clone();
                let value = var.r11_value(lu_dog)[0].read().unwrap().clone();
                let ty = value.r24_value_type(lu_dog)[0].clone();
                params.push((var.name.clone(), ty.clone()));

                let next_id = { next.read().unwrap().next };
                if let Some(ref id) = next_id {
                    next = lu_dog.exhume_parameter(id).unwrap();
                } else {
                    break;
                }
            }

            params
        } else {
            Vec::new()
        };

        let arg_values = if args.len() > 0 {
            let mut arg_values = Vec::with_capacity(args.len());
            let mut next = args
                .iter()
                .find(|a| a.read().unwrap().r27c_argument(lu_dog).len() == 0)
                .unwrap()
                .clone();

            loop {
                let expr = lu_dog
                    .exhume_expression(&next.read().unwrap().expression)
                    .unwrap();
                let (value, ty) = eval_expression(expr, stack, lu_dog, sarzak)?;
                arg_values.push((value, ty));

                let next_id = { next.read().unwrap().next };
                if let Some(ref id) = next_id {
                    next = lu_dog.exhume_argument(id).unwrap();
                } else {
                    break;
                }
            }

            arg_values
        } else {
            Vec::new()
        };

        params
            .into_iter()
            .zip(arg_values)
            .for_each(|((name, param_ty), (value, arg_ty))| {
                debug!("eval_function_call type check", value);
                if param_ty.read().unwrap().to_owned() != arg_ty.read().unwrap().to_owned() {
                    let expected = PrintableValueType(param_ty, lu_dog, sarzak);
                    let found = PrintableValueType(arg_ty, lu_dog, sarzak);
                    panic!(
                        "argument {}, type mismatch: expected {}, found {}",
                        name, expected, found
                    );
                }
                stack.insert(name, value);
            });

        let mut value;
        let mut ty;
        // This is a pain.
        // Find the first statement, by looking for the one with no previous statement.
        let mut next = stmts
            .iter()
            .find(|s| s.read().unwrap().r17c_statement(lu_dog).len() == 0)
            .unwrap()
            .clone();

        loop {
            (value, ty) = eval_statement(next.clone(), stack, lu_dog, sarzak)?;
            if let Some(ref id) = next.clone().read().unwrap().next {
                next = lu_dog.exhume_statement(id).unwrap();
            } else {
                break;
            }
        }

        // Clean up
        stack.pop();

        Ok((value, ty))
    } else {
        Ok((Value::Empty, ValueType::new_empty()))
    }
}

fn eval_expression(
    expression: Arc<RwLock<Expression>>,
    stack: &mut Stack,
    lu_dog: &LuDogStore,
    sarzak: &SarzakStore,
) -> Result<(Value, Arc<RwLock<ValueType>>)> {
    debug!("eval_expression: expression", expression);
    trace!("eval_expression: stack", stack);

    let result_style = Colour::Green.bold();

    match expression.read().unwrap().to_owned() {
        Expression::Call(ref call) => {
            let call = lu_dog.exhume_call(call).unwrap().read().unwrap().clone();
            debug!("call", call);
            let args = call.r28_argument(lu_dog);
            debug!("args", args);

            // This optional expression is the LHS of the call.
            let (value, ty) = if let Some(ref expr) = call.expression {
                let expr = lu_dog.exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the function.
                let (value, ty) = eval_expression(expr, stack, lu_dog, sarzak)?;
                no_debug!("Expression::Call LHS value", value);
                debug!("Expression::Call LHS ty", ty);
                // So now value is pointing a a legit Function. We need to jump
                // through all sorts of hoops now. We need to setup a new stack
                // frame, and push the old one on to a stack that doesn't exist
                // yet. Then we need to eval all the arguments and put them in
                // the frame. And then we need to eval the statements in the
                // function body.

                // Or we can just call the function we already wrote!
                match &value {
                    Value::Function(ref func) => {
                        let func = lu_dog.exhume_function(&func.read().unwrap().id).unwrap();
                        debug!("Expression::Call func", func);
                        let (value, ty) = eval_function_call(func, &args, stack, lu_dog, sarzak)?;
                        debug!("value", value);
                        debug!("ty", ty);
                        (value, ty)
                    }
                    Value::UserType(ut) => match ut {
                        UserType::Inflection(inflection) => {
                            debug!("Expression::Call inflection", inflection);
                            let model = MODEL.read().unwrap();
                            let ty = model.exhume_object_id_by_name("Inflection").unwrap();
                            dbg!(&ty);
                            let ty = lu_dog.exhume_value_type(ty).unwrap();
                            dbg!(&ty);
                            dbg!(&inflection);

                            (value, ty)
                        }
                        UserType::MerlinStore(_store) => {
                            // let store = ZObjectStore::new("MerlinStore".to_owned(), lu_dog);
                            (value, ValueType::new_empty())
                        }
                        foo => panic!("holy fucking hell batman! {:?}", foo),
                    },
                    value => {
                        panic!("dereferenced function expression and found this: {}", value);
                    }
                }
            } else {
                (Value::Empty, ValueType::new_empty())
            };

            // So we need to figure out the type this is being called on.
            match (&call.subtype, value, ty) {
                (CallEnum::FunctionCall(_), value, ty) => Ok((value, ty)),
                (CallEnum::MethodCall(meth), mut value, ty) => {
                    let meth = lu_dog.exhume_method_call(meth).unwrap();
                    let meth = &meth.read().unwrap().name;
                    debug!("method call method", meth);
                    debug!("method call value", value);
                    debug!("method call type", ty);

                    match &mut value {
                        Value::UserType(ut) => match ut {
                            UserType::Inflection(inflection) => {
                                let args: Vec<Value> = Vec::new();
                                Ok(inflection.call(meth, &args))
                            }
                            foo => panic!("need to deal with UserType {:?}", foo),
                        },
                        bar => panic!("need to deal with Value {:?}", bar),
                    }
                }
                (CallEnum::StaticMethodCall(meth), _, _) => {
                    let meth = lu_dog.exhume_static_method_call(meth).unwrap();
                    let call = meth.read().unwrap().r30_call(lu_dog)[0].clone();
                    let args = call.read().unwrap().r28_argument(lu_dog);

                    let ty = &meth.read().unwrap().ty;
                    let func = &meth.read().unwrap().func;
                    debug!("ty", ty);
                    debug!("func", func);

                    // This is dirty. Down and dirty...
                    if ty == "Uuid" && func == "new" {
                        let value = Value::Uuid(Uuid::new_v4());
                        let ty = Ty::new_s_uuid();
                        let ty = lu_dog.exhume_value_type(&ty.id()).unwrap();

                        Ok((value, ty.clone()))
                    } else {
                        if let Some(value) = stack.get_meta(ty, func) {
                            match &value {
                                Value::Function(ref func) => {
                                    let func =
                                        lu_dog.exhume_function(&func.read().unwrap().id).unwrap();
                                    debug!("func", func);
                                    let (value, ty) =
                                        eval_function_call(func, &args, stack, lu_dog, sarzak)?;
                                    debug!("value", value);
                                    debug!("ty", ty);
                                    Ok((value, ty))
                                }
                                value => {
                                    error!("deal with call expression", value);
                                    Ok((Value::Empty, ValueType::new_empty()))
                                }
                            }
                        } else if let Some(mut value) = stack.get_mut(ty) {
                            match &mut value {
                                Value::Function(ref func) => {
                                    let func =
                                        lu_dog.exhume_function(&func.read().unwrap().id).unwrap();
                                    debug!("func", func);
                                    let (value, ty) =
                                        eval_function_call(func, &args, stack, lu_dog, sarzak)?;
                                    debug!("value", value);
                                    debug!("ty", ty);
                                    Ok((value, ty))
                                }
                                Value::StoreType(ref mut store_type) => {
                                    // We should actually know what's behind the curtain, since
                                    // we requested it with `stack.get(ty)`, above.
                                    match store_type {
                                        StoreType::Inflection(ref mut inf) => {
                                            let args: Vec<Value> = Vec::new();
                                            Ok(inf.call(func, &args))
                                        }
                                        _ => Ok((
                                            Value::Error("make point work".to_owned()),
                                            ValueType::new_empty(),
                                        )),
                                    }
                                }
                                value => {
                                    error!("deal with call expression", value);
                                    Ok((Value::Empty, ValueType::new_empty()))
                                }
                            }
                        } else {
                            ensure!(
                                false,
                                NoSuchStaticMethodSnafu {
                                    ty: ty.to_owned(),
                                    method: func.to_owned(),
                                }
                            );

                            // We never will get here.
                            Ok((Value::Empty, ValueType::new_empty()))
                        }
                    }
                }
            }
        }
        Expression::ErrorExpression(ref error) => {
            let error = lu_dog.exhume_error_expression(error).unwrap();

            print!("\t{}", error.read().unwrap().span);

            Ok((Value::Empty, ValueType::new_empty()))
        }
        Expression::Literal(ref literal) => {
            let literal = lu_dog.exhume_literal(literal).unwrap();
            let z = match literal.read().unwrap().to_owned() {
                Literal::IntegerLiteral(ref literal) => {
                    let literal = lu_dog.exhume_integer_literal(literal).unwrap();
                    let value = literal.read().unwrap().value;
                    let value = Value::Integer(value);
                    let ty = Ty::new_integer();
                    let ty = lu_dog.exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty.clone()))
                }
                Literal::StringLiteral(ref literal) => {
                    let literal = lu_dog.exhume_string_literal(literal).unwrap();
                    // 🚧 It'd be great if this were an Rc...
                    let value =
                        Value::String(Arc::new(RwLock::new(literal.read().unwrap().value.clone())));
                    let ty = Ty::new_s_string();
                    let ty = lu_dog.exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty.clone()))
                }
                z => {
                    ensure!(
                        false,
                        UnimplementedSnafu {
                            message: format!("deal with literal expression: {:?}", z),
                        }
                    );

                    Ok((Value::Empty, ValueType::new_empty()))
                }
            };
            z
        }
        Expression::Print(ref print) => {
            let print = lu_dog.exhume_print(print).unwrap();
            debug!("Expression::Print print", print);
            let expr = print.read().unwrap().r32_expression(&lu_dog)[0].clone();
            let (value, _) = eval_expression(expr, stack, lu_dog, sarzak)?;
            let result = format!("{}", value);
            let result = result.replace("\\n", "\n");
            print!("\t{}", result_style.paint(result));

            Ok((value, ValueType::new_empty()))
        }
        Expression::StructExpression(ref expr) => {
            let expr = lu_dog.exhume_struct_expression(expr).unwrap();
            let field_exprs = expr.read().unwrap().r26_field_expression(lu_dog);
            let field_exprs = field_exprs
                .iter()
                .map(|f| {
                    let expr = lu_dog
                        .exhume_expression(&f.read().unwrap().expression)
                        .unwrap();
                    //
                    // 🐝🐝🐝
                    let (value, ty) = eval_expression(expr, stack, lu_dog, sarzak).unwrap();
                    // let (value, ty) = match eval_expression(expr, stack, lu_dog, sarzak) {
                    //     Ok(x) => x,
                    //     Err(e) => return Err(e),
                    // };
                    (f.read().unwrap().name.clone(), ty, value)
                })
                .collect::<Vec<_>>();

            let woog_struct = lu_dog
                .exhume_woog_struct(&expr.read().unwrap().woog_struct)
                .unwrap();
            let fields = woog_struct.read().unwrap().r7_field(lu_dog);

            // Type checking fields here
            for (name, ty, _value) in field_exprs {
                if let Some(field) = fields.iter().find(|f| f.read().unwrap().name == name) {
                    let struct_ty = lu_dog.exhume_value_type(&field.read().unwrap().ty).unwrap();
                    if struct_ty.read().unwrap().to_owned() != ty.read().unwrap().to_owned() {
                        let expected = PrintableValueType(struct_ty, lu_dog, sarzak);
                        let found = PrintableValueType(ty, lu_dog, sarzak);
                        panic!(
                            "field {}, type mismatch: expected {}, found {}",
                            name, expected, found
                        );
                    }
                } else {
                    panic!("no such field");
                }
            }

            let woog_struct = expr.read().unwrap().r39_woog_struct(lu_dog)[0].clone();
            let ty = lu_dog
                .exhume_value_type(&woog_struct.read().unwrap().id)
                .unwrap();

            // let value = Value

            Ok((Value::Empty, ty.clone()))
        }
        Expression::VariableExpression(ref expr) => {
            let expr = lu_dog.exhume_variable_expression(expr).unwrap();
            debug!("expr", expr);
            let value = stack.get(&expr.read().unwrap().name);
            if let Some(value) = value {
                no_debug!("value", value);

                // We can grab the type from the value
                let ty = match &value {
                    Value::Empty => ValueType::new_empty(),
                    Value::Function(ref func) => {
                        let func = lu_dog.exhume_function(&func.read().unwrap().id).unwrap();
                        debug!("VariableExpression get type func", func);
                        let z = func.read().unwrap().r1_value_type(lu_dog)[0].clone();
                        z
                    }
                    Value::Integer(ref int) => {
                        debug!("VariableExpression get type for int", int);
                        let ty = Ty::new_integer();
                        lu_dog.exhume_value_type(&ty.id()).unwrap().clone()
                    }
                    Value::StoreType(ref store) => {
                        debug!("VariableExpression get type for store", store);
                        store.get_type()
                    }
                    Value::String(ref str) => {
                        debug!("VariableExpression get type for string", str);
                        let ty = Ty::new_s_string();
                        lu_dog.exhume_value_type(&ty.id()).unwrap().clone()
                    }
                    Value::UserType(ref ut) => {
                        no_debug!("VariableExpression get type for user type", ut);
                        match ut {
                            UserType::MerlinStore(_) => {
                                // 🚧 this is clearly the wrong thing.
                                // let store = ZObjectStore::new("merlin".to_owned(), lu_dog);
                                ValueType::new_empty()
                            }
                            UserType::Point(_) => panic!("shit-fuck"),
                            // 🚧 is it a reference or an owned value?
                            UserType::Inflection(_) => {
                                let model = MODEL.read().unwrap();
                                let ty = model.exhume_object_id_by_name("Inflection").unwrap();
                                dbg!(&ty);
                                lu_dog.exhume_value_type(ty).unwrap()
                            }
                        }
                    }
                    Value::Uuid(ref uuid) => {
                        debug!("VariableExpression get type for uuid", uuid);
                        let ty = Ty::new_s_uuid();
                        lu_dog.exhume_value_type(&ty.id()).unwrap().clone()
                    }
                    value => {
                        error!("deal with variable expression", value);
                        ValueType::new_empty()
                    }
                };

                // Cloning the value isn't going to cut it I don't think. There are
                // three cases to consider. One is when the value is used read-only.
                // Cloning is find here. If the value is mutated however, we would
                // either need to return a reference, or write the value when it's
                // modified. The third thing is when the value is a reference. By
                // that I mean the type (above) is a reference. Not even sure what
                // to think about that atm.
                Ok((value.clone(), ty))
            } else {
                println!(
                    "\t{} not found.",
                    Colour::Red.paint(&expr.read().unwrap().name)
                );
                Ok((Value::Empty, ValueType::new_empty()))
            }
        }
        ref alpha => {
            ensure!(
                false,
                UnimplementedSnafu {
                    message: format!("deal with expression: {:?}", alpha),
                }
            );

            Ok((Value::Empty, ValueType::new_empty()))
        }
    }
}

fn eval_statement(
    statement: Arc<RwLock<Statement>>,
    stack: &mut Stack,
    lu_dog: &LuDogStore,
    sarzak: &SarzakStore,
) -> Result<(Value, Arc<RwLock<ValueType>>)> {
    debug!("eval_statement statement", statement);
    trace!("eval_statement stack", stack);

    match statement.read().unwrap().subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog
                .exhume_expression_statement(stmt)
                .unwrap()
                .read()
                .unwrap()
                .clone();
            let expr = stmt.r31_expression(&lu_dog)[0].clone();
            let (value, ty) = eval_expression(expr, stack, lu_dog, sarzak)?;
            no_debug!("StatementEnum::ExpressionStatement: value", value);
            debug!("StatementEnum::ExpressionStatement: ty", ty);

            Ok((Value::Empty, ty))
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = lu_dog
                .exhume_let_statement(stmt)
                .unwrap()
                .read()
                .unwrap()
                .clone();
            debug!("StatementEnum::LetStatement: stmt", stmt);

            let expr = stmt.r20_expression(&lu_dog)[0].clone();
            debug!("expr", expr);

            let (value, ty) = eval_expression(expr, stack, lu_dog, sarzak)?;
            debug!("value", value);
            debug!("ty", ty);

            let var = stmt.r21_local_variable(lu_dog)[0].read().unwrap().clone();
            let var = var.r12_variable(lu_dog)[0].read().unwrap().clone();
            debug!("var", var);

            log::debug!("inserting {} = {}", var.name, value);
            stack.insert(var.name.clone(), value);

            Ok((Value::Empty, ty))
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = lu_dog
                .exhume_result_statement(stmt)
                .unwrap()
                .read()
                .unwrap()
                .clone();
            debug!("StatementEnum::ResultStatement: stmt", stmt);

            let expr = stmt.r41_expression(&lu_dog)[0].clone();
            debug!("expr", expr);

            let (value, ty) = eval_expression(expr, stack, lu_dog, sarzak)?;
            debug!("value", value);
            debug!("ty", ty);

            Ok((value, ty))
        }
        ref beta => {
            error!("deal with statement", beta);
            Ok((Value::Empty, ValueType::new_empty()))
        }
    }
}

#[cfg(feature = "repl")]
fn do_repl(lu_dog: &mut LuDogStore, sarzak: &SarzakStore) -> Result<()> {
    let model = &MODEL;

    let block = Block::new(Uuid::new_v4(), lu_dog);

    let mut stack = Stack::new();

    // Insert the functions in the root frame.
    let funcs = lu_dog
        .iter_function()
        .map(|z| z.to_owned())
        .collect::<Vec<_>>();
    for func in funcs {
        let func_read = func.read().unwrap();
        let imp = func_read.r9_implementation(lu_dog);
        if imp.len() == 0 {
            let name = func_read.name.clone();
            let value = Value::Function(func.clone());

            // Build the local in the AST.
            let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
            let var = Variable::new_local_variable(name.clone(), local, lu_dog);
            let _value =
                LuDogValue::new_variable(block.clone(), ValueType::new_empty(), var, lu_dog);

            log::trace!("inserting local function {}", name);
            stack.insert(name, value);
        }
    }

    // Insert static methods for each struct.
    for user_type in lu_dog.iter_woog_struct() {
        let user_type = user_type.read().unwrap();
        // Create a meta table for each struct.
        stack.insert_meta_table(user_type.name.to_owned());
        let impl_ = user_type.r8c_implementation(lu_dog);
        if impl_.len() > 0 {
            // For each function in the impl, insert the function. I should probably
            // check and only insert the static functions.
            // 🚧 Only insert the static functions
            for func in impl_[0].read().unwrap().r9_function(lu_dog) {
                stack.insert_meta(
                    &user_type.name,
                    func.read().unwrap().name.to_owned(),
                    // It's here that I'd really like to be able to do a cheap
                    // clone of an Rc.
                    Value::Function(func.clone()),
                )
            }
        }
    }

    // Playing with integrating the store.
    stack.insert_global(
        "MERLIN_STORE".to_owned(),
        Value::UserType(UserType::MerlinStore((*model).clone())),
    );
    stack.insert_global(
        "INFLECTION".to_owned(),
        Value::StoreType(StoreType::Inflection(InflectionStoreType::default())),
    );

    // Build the AST
    let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
    let var = Variable::new_local_variable("MERLIN_STORE".to_owned(), local, lu_dog);
    let store = ZObjectStore::new("merlin".to_owned(), lu_dog);
    let _value = LuDogValue::new_variable(
        block.clone(),
        ValueType::new_z_object_store(store, lu_dog),
        var,
        lu_dog,
    );

    let prompt_style = Colour::Blue.normal();
    let result_style = Colour::Yellow.italic().dimmed();
    let type_style = Colour::Blue.italic().dimmed();
    let error_style = Colour::Red.bold();

    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new().map_err(|e| InnerError::RustyLine { source: e })?;

    // #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(&format!("{} ", prompt_style.paint("道:>")));
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())
                    .map_err(|e| InnerError::RustyLine { source: e })?;
                if let Some(stmt) = parse_line(&line) {
                    debug!("stmt from readline", stmt);

                    let (stmt, _) = inter_statement(
                        Arc::new(RwLock::new(stmt)),
                        block.clone(),
                        lu_dog,
                        &model.read().unwrap(),
                        sarzak,
                    );
                    match eval_statement(stmt, &mut stack, lu_dog, sarzak) {
                        Ok((value, ty)) => {
                            let value = format!("{}", value);
                            println!("{}", result_style.paint(value));

                            let ty = PrintableValueType(ty, &lu_dog, sarzak);
                            let ty = format!("{}", ty);
                            println!("\t  ──➤  {}", type_style.paint(ty));
                        }
                        Err(e) => {
                            println!("{}", e);
                        }
                    }
                } else {
                    println!("{}", error_style.paint("\tWTF?"));
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("👋 Bye bye!");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("👋 Bye bye!");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    // #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt")
        .map_err(|e| InnerError::RustyLine { source: e })?;

    Ok(())
}

struct PrintableValueType<'b, 'c>(Arc<RwLock<ValueType>>, &'b LuDogStore, &'c SarzakStore);

impl<'b, 'c> fmt::Display for PrintableValueType<'b, 'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.0.read().unwrap().to_owned();
        let lu_dog = self.1;
        let sarzak = self.2;

        match value {
            ValueType::Empty(_) => write!(f, "()"),
            ValueType::Error(_) => write!(f, "<error>"),
            ValueType::Function(_) => write!(f, "<function>"),
            ValueType::Import(ref import) => {
                let import = lu_dog
                    .exhume_import(import)
                    .unwrap()
                    .read()
                    .unwrap()
                    .clone();
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueType::List(ref list) => {
                let list = lu_dog.exhume_list(list).unwrap().read().unwrap().clone();
                let ty = list.r36_value_type(lu_dog)[0].clone();
                write!(f, "[{}]", PrintableValueType(ty, lu_dog, sarzak))
            }
            ValueType::Reference(ref reference) => {
                let reference = lu_dog
                    .exhume_reference(reference)
                    .unwrap()
                    .read()
                    .unwrap()
                    .clone();
                let ty = reference.r35_value_type(lu_dog)[0].clone();
                write!(f, "&{}", PrintableValueType(ty, lu_dog, sarzak))
            }
            ValueType::Ty(ref ty) => {
                let ty = sarzak.exhume_ty(ty).unwrap();
                match ty {
                    Ty::Object(ref object) => {
                        if let Some(object) = sarzak.exhume_object(object) {
                            write!(f, "{}", object.name)
                        } else {
                            write!(f, "<unknown object>")
                        }
                    }
                    Ty::SString(_) => write!(f, "String"),
                    Ty::SUuid(_) => write!(f, "Uuid"),
                    gamma => {
                        error!("deal with sarzak type", gamma);
                        write!(f, "todo")
                    }
                }
            }
            ValueType::Unknown(_) => write!(f, "<unknown>"),
            ValueType::WoogOption(ref option) => {
                let option = lu_dog.exhume_woog_option(option).unwrap();
                let baz = match option.read().unwrap().subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = lu_dog.exhume_z_some(some).unwrap().read().unwrap().clone();
                        let value = some.r23_value(lu_dog)[0].read().unwrap().clone();
                        let ty = value.r24_value_type(lu_dog)[0].clone();
                        write!(f, "Some({})", PrintableValueType(ty, lu_dog, sarzak))
                    }
                };
                baz
            }
            ValueType::WoogStruct(ref woog_struct) => {
                debug!("woog_struct", woog_struct);
                let woog_struct = lu_dog
                    .exhume_woog_struct(woog_struct)
                    .unwrap()
                    .read()
                    .unwrap()
                    .clone();
                write!(f, "{}", woog_struct.name)
            }
            ValueType::ZObjectStore(ref id) => {
                let zobject_store = lu_dog
                    .exhume_z_object_store(id)
                    .unwrap()
                    .read()
                    .unwrap()
                    .clone();
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}

#[derive(Debug)]
struct Stack {
    pub meta: HashMap<String, HashMap<String, Value>>,
    pub global: HashMap<String, Value>,
    pub frames: Vec<HashMap<String, Value>>,
}

impl Stack {
    fn new() -> Self {
        Stack {
            meta: HashMap::default(),
            global: HashMap::default(),
            frames: vec![HashMap::default()],
        }
    }

    fn push(&mut self) {
        self.frames.push(HashMap::default());
    }

    fn pop(&mut self) {
        self.frames.pop();
    }

    fn insert_meta_table(&mut self, table: String) {
        self.meta.insert(table, HashMap::default());
    }

    fn insert_meta(&mut self, table: &str, name: String, value: Value) {
        let table = self.meta.get_mut(table).unwrap();
        table.insert(name, value);
    }

    fn get_meta(&self, table: &str, name: &str) -> Option<&Value> {
        if let Some(table) = self.meta.get(table) {
            table.get(name)
        } else {
            None
        }
    }

    fn insert_global(&mut self, name: String, value: Value) {
        if name.contains("::") {
            let mut split = name.split("::");
            let table = split.nth(0).unwrap();
            let name = split
                .nth(0)
                .expect("name contained `::`, but no second element");

            if let Some(Value::Table(ref mut table)) = self.global.get_mut(table) {
                table.insert(name.to_owned(), value);
            } else {
                self.global
                    .insert(table.to_owned(), Value::Table(HashMap::default()));
                if let Some(Value::Table(ref mut table)) = self.global.get_mut(table) {
                    table.insert(name.to_owned(), value);
                } else {
                    unreachable!()
                }
            }
        } else {
            self.global.insert(name, value);
        }
    }

    fn insert(&mut self, name: String, value: Value) {
        let frame = self.frames.last_mut().unwrap();
        frame.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<&Value> {
        if name.contains("::") {
            let mut split = name.split("::");

            let name = split.next().unwrap();
            let table = split.next().unwrap();

            if let Some(Value::Table(ref table)) = self.get_simple(table) {
                table.get(name)
            } else {
                None
            }
        } else {
            self.get_simple(name)
        }
    }

    fn get_simple(&self, name: &str) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        self.global.get(name)
    }

    fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        if name.contains("::") {
            let mut split = name.split("::");

            let name = split.next().unwrap();
            let table = split.next().unwrap();

            if let Some(Value::Table(ref mut table)) = self.get_simple_mut(table) {
                table.get_mut(name)
            } else {
                None
            }
        } else {
            self.get_simple_mut(name)
        }
    }

    fn get_simple_mut(&mut self, name: &str) -> Option<&mut Value> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(value) = frame.get_mut(name) {
                return Some(value);
            }
        }
        self.global.get_mut(name)
    }
}

/// This is an actual Value
///
/// This is the type used by the interpreter to represent values.
#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Empty,
    Error(String),
    Float(f64),
    // 🚧 I need to rething the necessity of this locking.
    Function(Arc<RwLock<Function>>),
    Integer(i64),
    Option(Option<Box<Self>>),
    /// WTF was I thinking?
    ///
    /// That means Self. Or, maybe self?
    Reflexive,
    StoreType(StoreType),
    // 🚧 I need to rething the necessity of this locking.
    String(Arc<RwLock<String>>),
    Table(HashMap<String, Value>),
    /// User Defined Type
    ///
    ///  Feels like we'll need to generate some code to make this work.
    UserType(UserType),
    Uuid(uuid::Uuid),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(bool_) => write!(f, "{}", bool_),
            Self::Empty => write!(f, "()"),
            Self::Error(e) => write!(f, "{}: {}", Colour::Red.bold().paint("error"), e),
            Self::Float(num) => write!(f, "{}", num),
            Self::Function(_) => write!(f, "<function>"),
            Self::Integer(num) => write!(f, "{}", num),
            Self::Option(option) => match option {
                Some(value) => write!(f, "Some({})", value),
                None => write!(f, "None"),
            },
            Self::Reflexive => write!(f, "self"),
            Self::StoreType(store) => write!(f, "{:?}", store),
            Self::String(str_) => write!(f, "{}", str_.read().unwrap()),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::Table(table) => write!(f, "{:?}", table),
            Self::UserType(ut) => write!(f, "{}", ut),
            Self::Uuid(uuid) => write!(f, "{}", uuid),
        }
    }
}

///🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
/// The following will need to be generated
///
#[derive(Clone, Debug)]
pub enum StoreType {
    Inflection(InflectionStoreType),
    Point(PointStoreType),
}

impl StoreType {
    pub fn get_type(&self) -> Arc<RwLock<ValueType>> {
        match self {
            Self::Inflection(inflection) => Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                "e982c815-1947-4cd2-b34f-be7b73c8ae1e"
            )))),
            Self::Point(point) => Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                "d96244dd-9e32-4cb0-85a9-ea0627830439"
            )))),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InflectionStoreType {
    pub self_: Option<Inflection>,
}

impl InflectionStoreType {
    pub fn call(&mut self, method: &str, args: &Vec<Value>) -> (Value, Arc<RwLock<ValueType>>) {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => (
                    Value::Uuid(self_.id()),
                    Arc::new(RwLock::new(ValueType::Ty(SUuid::new().id()))),
                ),
                // 🚧 This needs to be sorted out with the other error stuff.
                道 => (
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                ),
            }
        } else {
            match method {
                "new" => {
                    let inflection = Inflection::new();
                    self.self_ = Some(inflection);
                    (
                        Value::UserType(UserType::Inflection(self.clone())),
                        // Clearly this will be generated...
                        // This is the id of the Inflection object
                        Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                            "e982c815-1947-4cd2-b34f-be7b73c8ae1e"
                        )))),
                    )
                }
                道 => (
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                ),
            }
        }
    }
}

impl Default for InflectionStoreType {
    fn default() -> Self {
        Self { self_: None }
    }
}

#[derive(Clone, Debug)]
pub struct PointStoreType {
    pub self_: Option<Inflection>,
}

impl PointStoreType {
    pub fn call(&mut self, method: &str, args: &Vec<Value>) -> (Value, Arc<RwLock<ValueType>>) {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => (
                    Value::Uuid(self_.id()),
                    Arc::new(RwLock::new(ValueType::Ty(SUuid::new().id()))),
                ),
                // 🚧 This needs to be sorted out with the other error stuff.
                道 => (
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                ),
            }
        } else {
            match method {
                "new" => {
                    let inflection = Inflection::new();
                    self.self_ = Some(inflection);
                    (
                        Value::UserType(UserType::Point(self.clone())),
                        // Clearly this will be generated...
                        // This is the id of the Ioint object
                        Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                            "d96244dd-9e32-4cb0-85a9-ea0627830439"
                        )))),
                    )
                }
                道 => (
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                ),
            }
        }
    }
}

impl Default for PointStoreType {
    fn default() -> Self {
        Self { self_: None }
    }
}

#[derive(Clone, Debug)]
pub enum UserType {
    MerlinStore(Arc<RwLock<SarzakStore>>),
    Inflection(InflectionStoreType),
    Point(PointStoreType),
}

impl fmt::Display for UserType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MerlinStore(_) => write!(f, "MerlinStore"),
            // I think that we are guaranteed to have a self_ here.
            Self::Inflection(inflection) => write!(
                f,
                "Inflection({})\n",
                inflection.self_.as_ref().unwrap().id()
            ),
            Self::Point(point) => write!(f, "{:?}", point),
        }
    }
}
