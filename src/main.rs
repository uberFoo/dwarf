use std::{fmt, path::PathBuf};

use ansi_term::Colour;
use clap::Args;
use fxhash::FxHashMap as HashMap;
use heck::ToUpperCamelCase;
use log;
use sarzak::{
    dwarf::{inter_statement, parse_line},
    lu_dog::{FieldExpression, Parameter},
    v2::{
        lu_dog::{
            store::ObjectStore as LuDogStore,
            types::{
                Argument, Block, CallEnum, Expression, Function, Literal, LocalVariable, Statement,
                StatementEnum, Value as LuDogValue, ValueType, Variable, WoogOptionEnum,
            },
        },
        sarzak::{store::ObjectStore as SarzakStore, types::Ty},
    },
};
use serde::{Deserialize, Serialize};
use snafu::{prelude::*, Whatever};
use uuid::Uuid;

#[cfg(feature = "repl")]
use rustyline::error::ReadlineError;
#[cfg(feature = "repl")]
use rustyline::{DefaultEditor, Result as ReplResult};

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

macro_rules! make_vec_from_reflexive {
    ($bag:ident, $first_fun:expr, $next_fun:expr, $eval:expr) => {
        if $bag.len() > 0 {
            let mut vec = Vec::with_capacity($bag.len());
            let mut next = $first_fun();

            loop {
                let value = $eval(next);
                vec.push(value);

                if let Some(ref id) = next.next {
                    next = $next_fun(id);
                } else {
                    break;
                }
            }

            vec
        } else {
            Vec::new()
        }
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

fn main() -> Result<(), Whatever> {
    pretty_env_logger::init();

    let style = Colour::Purple;

    println!("");
    let banner = r#"   ________          ________             _       __                            __
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
        .with_whatever_context(|_| "failed to load sarzak")?;

    // This will always be a lu-dog, but it's basically a compiled dwarf file.
    // let mut lu_dog = LuDogStore::load("../sarzak/target/sarzak/lu_dog")
    let mut lu_dog = LuDogStore::load("../sarzak/target/sarzak/merlin")
        .with_whatever_context(|_| "failed to load lu_dog")?;

    // This won't always be Lu-Dog, clearly. So we'll need to be sure to also
    // generate some code that imports the types from the model.
    // let model = SarzakStore::load("../sarzak/models/lu_dog.v2.json")
    let model = SarzakStore::load("../sarzak/models/merlin.v2.json")
        .with_whatever_context(|_| "failed to load model")?;

    #[cfg(feature = "repl")]
    do_repl(&mut lu_dog, &sarzak, &model).with_whatever_context(|_| "repl error")?;

    Ok(())
}

fn eval_function_call(
    func: &Function,
    args: &[&Argument],
    stack: &mut Stack,
    lu_dog: &LuDogStore,
) -> ValueType {
    debug!("eval_function_call func ", func);
    trace!("eval_function_call stack", stack);

    let block = lu_dog.exhume_block(&func.block).unwrap();
    let stmts = block.r18_statement(&lu_dog);

    if stmts.len() > 0 {
        stack.push();

        // We need to evaluate the arguments, and then push them onto the stack. We
        // also need to typecheck the arguments against the function parameters.
        // We need to look the params up anyway to set the local variables.
        let params = func.r13_parameter(&lu_dog);
        let params = make_vec_from_reflexive!(
            params,
            || *func
                .r13_parameter(&lu_dog)
                .iter()
                .find(|p| p.r14c_parameter(&lu_dog).len() == 0)
                .unwrap(),
            |id| lu_dog.exhume_parameter(id).unwrap(),
            |next: &Parameter| {
                let var = next.r12_variable(&lu_dog)[0];
                let value = var.r11_value(lu_dog)[0];
                let ty = value.r24_value_type(lu_dog)[0];
                (var.name.clone(), ty.clone())
            }
        );
        // let params = if params.len() > 0 {
        //     let mut params = Vec::new();
        //     let mut next = *func
        //         .r13_parameter(&lu_dog)
        //         .iter()
        //         .find(|p| p.r14c_parameter(&lu_dog).len() == 0)
        //         .unwrap();

        //     loop {
        //         let var = next.r12_variable(&lu_dog)[0];
        //         let value = var.r11_value(lu_dog)[0];
        //         let ty = value.r24_value_type(lu_dog)[0];
        //         debug!("eval_function_call param", var.name);
        //         debug!("eval_function_call param", ty);
        //         params.push((var.name.clone(), ty));
        //         if let Some(ref id) = next.next {
        //             next = lu_dog.exhume_parameter(id).unwrap();
        //         } else {
        //             break;
        //         }
        //     }
        //     params
        // } else {
        //     Vec::new()
        // };

        let arg_values = if args.len() > 0 {
            let mut arg_values = Vec::new();
            let mut next = *args
                .iter()
                .find(|a| a.r27c_argument(lu_dog).len() == 0)
                .unwrap();

            loop {
                let expr = lu_dog.exhume_expression(&next.expression).unwrap();
                let (value, ty) = eval_expression(expr, stack, lu_dog);
                arg_values.push((value, ty));
                if let Some(ref id) = next.next {
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
                if param_ty != arg_ty {
                    panic!("Type mismatch: {:?} != {:?}", param_ty, arg_ty);
                }
                stack.insert(name, value);
            });

        let mut value;
        // This is a pain.
        // Find the first statement, by looking for the one with no previous statement.
        let mut next = *stmts
            .iter()
            .find(|s| s.r17c_statement(lu_dog).len() == 0)
            .unwrap();

        loop {
            value = eval_statement(next, stack, lu_dog);
            if let Some(ref id) = next.next {
                next = lu_dog.exhume_statement(id).unwrap();
            } else {
                break;
            }
        }

        // Clean up
        stack.pop();

        value
    } else {
        ValueType::new_empty()
    }
}

fn eval_expression(
    expression: &Expression,
    stack: &mut Stack,
    lu_dog: &LuDogStore,
) -> (Value, ValueType) {
    debug!("eval_expression: expression", expression);
    trace!("eval_expression: stack", stack);

    let result_style = Colour::Green.bold();

    match &expression {
        Expression::Call(ref call) => {
            let call = lu_dog.exhume_call(call).unwrap();
            debug!("call", call);
            let args = call.r28_argument(lu_dog);
            debug!("args", args);

            // This optional expression is the LHS of the call, as an expression.
            if let Some(ref expr) = call.expression {
                let expr = lu_dog.exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the function.
                let (value, ty) = eval_expression(&expr, stack, lu_dog);
                debug!("value", value);
                debug!("ty", ty);
                // So now value is pointing a a legit Function. We need to jump
                // through all sorts of hoops now. We need to setup a new stack
                // frame, and push the old one on to a stack that doesn't exist
                // yet. Then we need to eval all the arguments and put them in
                // the frame. And then we need to eval the statements in the
                // function body.

                // Or we can just call the function we already wrote!
                match &value {
                    Value::Function(ref func) => {
                        let func = lu_dog.exhume_function(&func.id).unwrap();
                        debug!("func", func);
                        let ty = eval_function_call(func, &args, stack, lu_dog);
                        debug!("ty", ty);
                    }
                    value => {
                        panic!(
                            "dereferenced function expression and found this: {:?}",
                            value
                        );
                    }
                }
            } else {
                // So we need to figure out the type this is being called on.
                match &call.subtype {
                    CallEnum::FunctionCall(_) => {
                        panic!("shouldn't have a function call without an expression as the LHS");
                    }
                    CallEnum::MethodCall(meth) => {
                        let meth = lu_dog.exhume_method_call(meth).unwrap();
                        error!("deal with method call", meth);
                    }
                    CallEnum::StaticMethodCall(meth) => {
                        let meth = lu_dog.exhume_static_method_call(meth).unwrap();
                        let call = meth.r30_call(lu_dog)[0];
                        let args = call.r28_argument(lu_dog);

                        let ty = &meth.ty;
                        let func = &meth.func;

                        // This is dirty. Down and dirty...
                        if ty == "Uuid" && func == "new_v4" {
                            let value = Value::Uuid(Uuid::new_v4());
                            let ty = Ty::new_s_uuid();
                            let ty = lu_dog.exhume_value_type(&ty.id()).unwrap();

                            return (value, ty.clone());
                        } else {
                            if let Some(value) = stack.get_meta(ty, func) {
                                match &value {
                                    Value::Function(ref func) => {
                                        let func = lu_dog.exhume_function(&func.id).unwrap();
                                        debug!("func", func);
                                        let ty = eval_function_call(func, &args, stack, lu_dog);
                                        debug!("ty", ty);
                                    }
                                    value => {
                                        error!("deal with call expression", value);
                                    }
                                }
                            }
                        }

                        // error!("deal with static method call", meth);
                    }
                }
            }

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // Oh, come on!!!
            (Value::Empty, ValueType::new_empty())
        }
        Expression::ErrorExpression(ref error) => {
            let error = lu_dog.exhume_error_expression(error).unwrap();

            print!("\t{}", error.span);

            (Value::Empty, ValueType::new_empty())
        }
        Expression::Literal(ref literal) => {
            let literal = lu_dog.exhume_literal(literal).unwrap();
            match literal {
                Literal::StringLiteral(ref literal) => {
                    let literal = lu_dog.exhume_string_literal(literal).unwrap();
                    let value = literal.value.clone();
                    let value = Value::String(value);
                    let ty = Ty::new_s_string();
                    let ty = lu_dog.exhume_value_type(&ty.id()).unwrap();

                    (value, ty.clone())
                }
                z => {
                    error!("deal with literal expression", z);
                    (Value::Empty, ValueType::new_empty())
                }
            }
        }
        Expression::Print(ref print) => {
            let print = lu_dog.exhume_print(print).unwrap();
            debug!("Expression::Print print", print);
            let expr = print.r32_expression(&lu_dog)[0];
            let (value, _) = eval_expression(&expr, stack, lu_dog);
            let result = format!("{}", value);
            let result = result.replace("\\n", "\n");
            print!("\t{}", result_style.paint(result));

            (value, ValueType::new_empty())
        }
        Expression::StructExpression(ref expr) => {
            let expr = lu_dog.exhume_struct_expression(expr).unwrap();
            let fields = expr.r26_field_expression(lu_dog);
            let fields = make_vec_from_reflexive!(
                fields,
                || *fields
                    .iter()
                    .find(|f| f.r25c_field_expression(lu_dog).len() == 0)
                    .unwrap(),
                |id| lu_dog.exhume_field_expression(id).unwrap(),
                |next: &FieldExpression| {
                    let next = lu_dog.exhume_field_expression(&next.id).unwrap();
                    let expr = lu_dog.exhume_expression(&next.expression).unwrap();
                    let (value, ty) = eval_expression(&expr, stack, lu_dog);

                    (next.name.clone(), ty, value)
                }
            );

            // 🚧 Type checking here
            dbg!(&fields);

            let woog_struct = expr.r39_woog_struct(lu_dog)[0];
            let ty = lu_dog.exhume_value_type(&woog_struct.id).unwrap();

            // let value = Value

            (Value::Empty, ty.clone())
        }
        Expression::VariableExpression(ref expr) => {
            let expr = lu_dog.exhume_variable_expression(expr).unwrap();
            debug!("expr", expr);
            let value = stack.get(&expr.name);
            if let Some(value) = value {
                debug!("value", value);

                // We can grab the type from the value
                let ty = match &value {
                    Value::Empty => ValueType::new_empty(),
                    Value::Function(ref func) => {
                        let func = lu_dog.exhume_function(&func.id).unwrap();
                        debug!("VariableExpression get type func", func);
                        func.r1_value_type(lu_dog)[0].clone()
                    }
                    Value::String(ref str) => {
                        debug!("VariableExpression get type for string", str);
                        let ty = Ty::new_s_string();
                        lu_dog.exhume_value_type(&ty.id()).unwrap().clone()
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
                // let ty = {
                //     let expr = expr.r15_expression(lu_dog)[0];
                //     debug!("expr", expr);
                //     let value = expr.r11_value(lu_dog)[0];
                //     debug!("value", value);
                //     let ty = value.r24_value_type(lu_dog)[0];
                //     debug!("ty", ty);
                //     ty
                // };

                (value.clone(), ty)
            } else {
                println!("\t{} not found.", Colour::Red.paint(&expr.name));
                (Value::Empty, ValueType::new_empty())
            }
        }
        ref alpha => {
            error!("deal with expression", alpha);
            (Value::Empty, ValueType::new_empty())
        }
    }
}

fn eval_statement(statement: &Statement, stack: &mut Stack, lu_dog: &LuDogStore) -> ValueType {
    debug!("eval_statement statement", statement);
    trace!("eval_statement stack", stack);

    match statement.subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog.exhume_expression_statement(stmt).unwrap();
            let expr = stmt.r31_expression(&lu_dog)[0];
            let (value, ty) = eval_expression(&expr, stack, lu_dog);
            debug!("StatementEnum::ExpressionStatement: value", value);
            debug!("StatementEnum::ExpressionStatement: ty", ty);

            ty
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = lu_dog.exhume_let_statement(stmt).unwrap();
            debug!("StatementEnum::LetStatement: stmt", stmt);

            let expr = stmt.r20_expression(&lu_dog)[0];
            debug!("expr", expr);

            let (value, ty) = eval_expression(&expr, stack, lu_dog);
            debug!("value", value);
            debug!("ty", ty);

            let var = stmt.r21_local_variable(lu_dog)[0];
            let var = var.r12_variable(lu_dog)[0];
            debug!("var", var);

            log::debug!("inserting {} = {}", var.name, value);
            stack.insert(var.name.clone(), value);
            debug!("stack", stack);

            ty
        }
        ref beta => {
            error!("deal with statement", beta);
            ValueType::new_empty()
        }
    }
}

#[cfg(feature = "repl")]
fn do_repl(lu_dog: &mut LuDogStore, sarzak: &SarzakStore, model: &SarzakStore) -> ReplResult<()> {
    let block = Block::new(Uuid::new_v4(), lu_dog);

    let mut stack = Stack::new();

    // Insert the functions at the root level.
    let funcs = lu_dog.iter_function().cloned().collect::<Vec<_>>();
    for func in funcs {
        let imp = func.r9_implementation(lu_dog);
        if imp.len() == 0 {
            let name = func.name.clone();
            let value = Value::Function(func.clone());

            // Build the local in the AST.
            let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
            let var = Variable::new_local_variable(name.clone(), &local, lu_dog);
            let _value = LuDogValue::new_variable(&block, &ValueType::new_empty(), &var, lu_dog);

            log::trace!("inserting local function {}", name);
            stack.insert(name, value);
        }
    }

    // Insert static methods for each struct.
    for user_type in lu_dog.iter_woog_struct() {
        // Create a meta table for each struct.
        stack.insert_meta_table(user_type.name.to_owned());
        let impl_ = user_type.r8c_implementation(lu_dog);
        if impl_.len() > 0 {
            // For each function in the impl, insert the function. I should probably
            // check and only insert the static functions.
            // 🚧 Only insert the static functions
            for func in impl_[0].r9_function(lu_dog) {
                stack.insert_meta(
                    &user_type.name,
                    func.name.to_owned(),
                    Value::Function(func.clone()),
                )
            }
        }
    }

    // Playing with integrating the store.
    stack.insert_global("MERLIN_STORE".to_owned(), Value::Empty);

    let prompt_style = Colour::Blue.normal();
    let result_style = Colour::Yellow.italic().dimmed();
    let error_style = Colour::Red.bold();

    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    // #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(&format!("{} ", prompt_style.paint("道:>")));
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                if let Some(stmt) = parse_line(&line) {
                    debug!("stmt at beginning of readline", stmt);

                    // let mut lu_dog = lu_dog.clone();

                    let (stmt, _) = inter_statement(&stmt, &block, lu_dog, model, sarzak);
                    let value = eval_statement(&stmt, &mut stack, &lu_dog);
                    debug!("stack", stack);

                    let ty = PrintableValueType(&value, &lu_dog, sarzak);
                    let ty = format!("{}", ty);
                    println!("\t  ──➤  {}", result_style.paint(ty));
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
    rl.save_history("history.txt")?;
    Ok(())
}

struct PrintableValueType<'a, 'b, 'c>(&'a ValueType, &'b LuDogStore, &'c SarzakStore);

impl<'a, 'b, 'c> fmt::Display for PrintableValueType<'a, 'b, 'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.0;
        let lu_dog = self.1;
        let sarzak = self.2;

        match value {
            ValueType::Empty(_) => write!(f, "()"),
            ValueType::Error(_) => write!(f, "<error>"),
            ValueType::Function(_) => write!(f, "<function>"),
            ValueType::Import(ref import) => {
                let import = lu_dog.exhume_import(import).unwrap();
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueType::List(ref list) => {
                let list = lu_dog.exhume_list(list).unwrap();
                let ty = list.r36_value_type(lu_dog)[0];
                write!(f, "[{}]", PrintableValueType(&ty, lu_dog, sarzak))
            }
            ValueType::Reference(ref reference) => {
                let reference = lu_dog.exhume_reference(reference).unwrap();
                let ty = reference.r35_value_type(lu_dog)[0];
                write!(f, "&{}", PrintableValueType(&ty, lu_dog, sarzak))
            }
            ValueType::Ty(ref ty) => {
                let ty = sarzak.exhume_ty(ty).unwrap();
                match ty {
                    Ty::SString(_) => write!(f, "String"),
                    gamma => {
                        error!("deal with sarzak type", gamma);
                        write!(f, "todo")
                    }
                }
            }
            ValueType::Unknown(_) => write!(f, "<unknown>"),
            ValueType::WoogOption(ref option) => {
                let option = lu_dog.exhume_woog_option(option).unwrap();
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = lu_dog.exhume_z_some(some).unwrap();
                        let value = some.r23_value(lu_dog)[0];
                        let ty = value.r24_value_type(lu_dog)[0];
                        write!(f, "Some({})", PrintableValueType(&ty, lu_dog, sarzak))
                    }
                }
            }
            ValueType::WoogStruct(ref woog_struct) => {
                let woog_struct = lu_dog.exhume_woog_struct(woog_struct).unwrap();
                write!(f, "{}", woog_struct.name)
            }
            ValueType::ZObjectStore(ref id) => {
                let zobject_store = lu_dog.exhume_z_object_store(id).unwrap();
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}

#[derive(Debug)]
struct Stack {
    meta: HashMap<String, HashMap<String, Value>>,
    global: HashMap<String, Value>,
    frames: Vec<HashMap<String, Value>>,
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
        self.global.insert(name, value);
    }

    fn insert(&mut self, name: String, value: Value) {
        let frame = self.frames.last_mut().unwrap();
        frame.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        self.global.get(name)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Empty,
    Float(f64),
    Function(Function),
    Integer(i64),
    Option(Option<Box<Self>>),
    // That means Self. Or, maybe self?
    Reflexive,
    String(String),
    // Feels like we'll need to generate some code to make this work.
    UserType(UserType),
    Uuid(uuid::Uuid),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(bool_) => write!(f, "{}", bool_),
            Self::Empty => write!(f, "()"),
            Self::Float(num) => write!(f, "{}", num),
            Self::Function(_) => write!(f, "<function>"),
            Self::Integer(num) => write!(f, "{}", num),
            Self::Option(option) => match option {
                Some(value) => write!(f, "Some({})", value),
                None => write!(f, "None"),
            },
            Self::Reflexive => write!(f, "self"),
            Self::String(str_) => write!(f, "{}", str_),
            // Self::String(str_) => write!(f, "\"{}\"", str_),
            Self::UserType(ut) => write!(f, "{}", ut),
            Self::Uuid(uuid) => write!(f, "{}", uuid),
        }
    }
}

///🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
/// The following will need to be generated
#[derive(Clone, Debug)]
pub enum UserType {
    Literal(Literal),
    LocalVariable(LocalVariable),
}

impl fmt::Display for UserType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{:?}", literal),
            Self::LocalVariable(local_variable) => write!(f, "{:?}", local_variable),
        }
    }
}
