use std::{fmt, path::PathBuf};

use ansi_term::Colour;
use clap::Args;
use fxhash::FxHashMap as HashMap;
use log;
use sarzak::{
    dwarf::{inter_statement, parse_line, Value},
    v2::{
        lu_dog::{
            store::ObjectStore as LuDogStore,
            types::{
                Block, Expression, Function, Literal, LocalVariable, Statement, StatementEnum,
                Value as LuDogValue, ValueType, Variable, WoogOptionEnum,
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

macro_rules! red {
    ($arg:expr) => {
        log::debug!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::debug!(
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

    let style = Colour::Purple.blink();

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
    let mut lu_dog = LuDogStore::load("../sarzak/target/sarzak/lu_dog")
        .with_whatever_context(|_| "faled to load lu_dog")?;

    // let main = lu_dog.iter_function().find(|f| f.name == "main").unwrap();

    // eval_function_call(main, &lu_dog);

    #[cfg(feature = "repl")]
    do_repl(&mut lu_dog, &sarzak).with_whatever_context(|_| "repl error")?;

    Ok(())
}

fn eval_function_call(
    func: &Function,
    // frame: &mut HashMap<String, Value>,
    stack: &mut Vec<HashMap<String, Value>>,
    lu_dog: &LuDogStore,
) -> ValueType {
    debug!("eval_function_call func ", func);
    trace!("eval_function_call stack", stack);

    let block = lu_dog.exhume_block(&func.block).unwrap();
    let stmts = block.r18_statement(&lu_dog);

    if stmts.len() > 0 {
        stack.push(HashMap::default());
        let mut value = ValueType::new_empty();
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
    // frame: &mut HashMap<String, Value>,
    stack: &mut Vec<HashMap<String, Value>>,
    lu_dog: &LuDogStore,
) -> (Value, ValueType) {
    debug!("eval_expression: expression", expression);
    trace!("eval_expression: stack", stack);

    let result_style = Colour::Green.bold();

    match &expression {
        Expression::Call(ref call) => {
            let call = lu_dog.exhume_call(call).unwrap();
            debug!("call", call);
            if let Some(ref expr) = call.expression {
                let expr = lu_dog.exhume_expression(expr).unwrap();
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
                        let ty = eval_function_call(func, stack, lu_dog);
                        debug!("ty", ty);
                    }
                    value => {
                        red!("deal with call expression", value);
                    }
                }
            }
            // match &call.subtype {
            //     CallEnum::FunctionCall(ref func) => {
            //         // let func = lu_dog.exhume_function_call(func).unwrap();
            //         debug!("func", func);
            //     }
            //     call => {
            //         red!("deal with call", call);
            //     }
            // }

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
                    red!("deal with literal expression", z);
                    (Value::Empty, ValueType::new_empty())
                }
            }
        }
        Expression::Print(ref print) => {
            let print = lu_dog.exhume_print(print).unwrap();
            let expr = print.r32_expression(&lu_dog)[0];
            let (value, _) = eval_expression(&expr, stack, lu_dog);
            let result = format!("{}", value);
            let result = result.replace("\\n", "\n");
            print!("\t{}", result_style.paint(result));

            (value, ValueType::new_empty())
        }
        Expression::VariableExpression(ref expr) => {
            let expr = lu_dog.exhume_variable_expression(expr).unwrap();
            debug!("expr", expr);
            let frame = stack.last().unwrap();
            let value = frame.get(&expr.name);
            if let Some(value) = value {
                debug!("value", value);

                // We can grab the type from the value
                let ty = match &value {
                    // Value::String(_) => Ty::new_s_string(),
                    Value::Function(ref func) => {
                        let func = lu_dog.exhume_function(&func.id).unwrap();
                        debug!("VariableExpression get type func", func);
                        func.r1_value_type(lu_dog)[0].clone()
                    }
                    Value::String(ref str) => {
                        red!("VariableExpression get type for string {}", str);
                        let ty = Ty::new_s_string();
                        lu_dog.exhume_value_type(&ty.id()).unwrap().clone()
                    }
                    Value::Empty => ValueType::new_empty(),
                    value => {
                        red!("deal with variable expression", value);
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
            red!("deal with expression", alpha);
            (Value::Empty, ValueType::new_empty())
        }
    }
}

fn eval_statement(
    statement: &Statement,
    stack: &mut Vec<HashMap<String, Value>>,
    lu_dog: &LuDogStore,
) -> ValueType {
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
            let frame = stack.last_mut().unwrap();
            frame.insert(var.name.clone(), value);

            ty
        }
        ref beta => {
            red!("deal with statement", beta);
            ValueType::new_empty()
        }
    }
}

#[cfg(feature = "repl")]
fn do_repl(lu_dog: &mut LuDogStore, sarzak: &SarzakStore) -> ReplResult<()> {
    let block = Block::new(Uuid::new_v4(), lu_dog);
    let mut stack = vec![HashMap::default()];
    let mut frame = stack.last_mut().unwrap();

    let funcs = lu_dog.iter_function().cloned().collect::<Vec<_>>();

    for func in funcs {
        let name = func.name.clone();
        let value = Value::Function(func.clone());

        // Build the local in the AST.
        let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
        let var = Variable::new_local_variable(name.clone(), &local, lu_dog);
        // let (expr, ty) = inter_expression(expr, &block, lu_dog);
        let _value = LuDogValue::new_variable(&block, &ValueType::new_empty(), &var, lu_dog);

        log::debug!("inserting {} = {}", var.name, value);
        frame.insert(name, value);
    }

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

                    let (stmt, _) = inter_statement(&stmt, &block, lu_dog);
                    // 🚧 I should really link the statement into the previous in the block.

                    let value = eval_statement(&stmt, &mut stack, lu_dog);

                    let ty = PrintableValueType(&value, lu_dog, sarzak);
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
            ValueType::WoogOption(ref option) => {
                let option = lu_dog.exhume_woog_option(option).unwrap();
                match option.subtype {
                    WoogOptionEnum::None(_) => write!(f, "None"),
                    WoogOptionEnum::Some(ref some) => {
                        let some = lu_dog.exhume_some(some).unwrap();
                        let value = some.r23_value(lu_dog)[0];
                        let ty = value.r24_value_type(lu_dog)[0];
                        write!(f, "Some({})", PrintableValueType(&ty, lu_dog, sarzak))
                    }
                }
            }
            ValueType::Ty(ref ty) => {
                let ty = sarzak.exhume_ty(ty).unwrap();
                match ty {
                    Ty::SString(_) => write!(f, "String"),
                    gamma => {
                        red!("deal with sarzak type", gamma);
                        write!(f, "todo")
                    }
                }
            }
        }
    }
}
