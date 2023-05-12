use std::{
    collections::VecDeque,
    fmt,
    path::Path,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use fxhash::FxHashMap as HashMap;
use heck::ToUpperCamelCase;
use lazy_static::lazy_static;
use log;
use sarzak::{
    lu_dog::{
        Argument, Binary, Block, BooleanLiteral, CallEnum, Comparison, Expression, Function,
        Literal, LocalVariable, ObjectStore as LuDogStore, Operator, OperatorEnum, Statement,
        StatementEnum, Value as LuDogValue, ValueType, Variable, WoogOptionEnum,
    },
    sarzak::{store::ObjectStore as SarzakStore, types::Ty},
};
use snafu::{location, prelude::*, Location};
use uuid::Uuid;

#[cfg(feature = "repl")]
use rustyline::{error::ReadlineError, DefaultEditor};

use crate::{
    dwarf::{inter_statement, parse_line},
    svm::{CallFrame, Chunk, Instruction, VM},
    value::{StoreProxy, UserType},
    ChaChaError, DwarfInteger, Error, NoSuchFieldSnafu, NoSuchStaticMethodSnafu, Result,
    TypeMismatchSnafu, UnimplementedSnafu, Value, VariableNotFoundSnafu,
    WrongNumberOfArgumentsSnafu,
};

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

lazy_static! {
    pub(crate) static ref MODELS: Arc<RwLock<Vec<SarzakStore>>> = Arc::new(RwLock::new(Vec::new()));
    pub(crate) static ref LU_DOG: Arc<RwLock<LuDogStore>> =
        Arc::new(RwLock::new(LuDogStore::new()));
    pub(crate) static ref SARZAK: Arc<RwLock<SarzakStore>> =
        Arc::new(RwLock::new(SarzakStore::new()));
}

pub struct Context {
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl Context {
    pub fn register_model<P: AsRef<Path>>(&self, model_path: P) -> Result<()> {
        let model =
            SarzakStore::load(model_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

        MODELS.write().unwrap().push(model);

        Ok(())
    }

    pub fn lu_dog_heel(&self) -> Arc<RwLock<LuDogStore>> {
        self.lu_dog.clone()
    }
}

pub fn initialize_compiler<P: AsRef<Path>>(
    sarzak_path: P,
    lu_dog_path: P,
) -> Result<Context, Error> {
    let sarzak =
        SarzakStore::load(sarzak_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

    // This will always be a lu-dog, but it's basically a compiled dwarf file.
    // let mut lu_dog = LuDogStore::load("../sarzak/target/sarzak/lu_dog")
    let mut lu_dog =
        LuDogStore::load(lu_dog_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

    // Hide everything behind a the globals.
    *SARZAK.write().unwrap() = sarzak;
    *LU_DOG.write().unwrap() = lu_dog;

    Ok(Context {
        lu_dog: LU_DOG.clone(),
    })
}

fn compile_expression(
    expression: Arc<RwLock<Expression>>,
    chunk: &mut Chunk,
) -> Result<Arc<RwLock<ValueType>>> {
    let lu_dog = &LU_DOG;

    debug!("compile_expression: expression", expression);
    debug!("compile_expression: chunk", chunk);

    match *expression.read().unwrap() {
        //
        // Block
        //
        Expression::Block(ref block) => {
            let block = lu_dog.read().unwrap().exhume_block(block).unwrap();
            let block = block.read().unwrap();
            let stmts = block.r18_statement(&lu_dog.read().unwrap());

            if !stmts.is_empty() {
                let mut value;
                let mut ty;
                // This is a pain.
                // Find the first statement, by looking for the one with no previous statement.
                let mut next = stmts
                    .iter()
                    .find(|s| {
                        s.read()
                            .unwrap()
                            .r17c_statement(&lu_dog.read().unwrap())
                            .is_empty()
                    })
                    .unwrap()
                    .clone();

                loop {
                    let result = compile_statement(next.clone(), chunk).map_err(|e| e);

                    if let Err(ChaChaError::Return { value, ty }) = &result {
                        return Ok(ty.clone());
                    }

                    (value, ty) = result?;

                    if let Some(ref id) = next.clone().read().unwrap().next {
                        next = lu_dog.read().unwrap().exhume_statement(id).unwrap();
                    } else {
                        break;
                    }
                }

                Ok(ty)
            } else {
                Ok(ValueType::new_empty(&lu_dog.read().unwrap()))
            }
        }
        //
        // Call
        //
        Expression::Call(ref call) => {
            let call = lu_dog.read().unwrap().exhume_call(call).unwrap();
            let call = call.read().unwrap();
            debug!("call", call);
            let args = call.r28_argument(&lu_dog.read().unwrap());
            debug!("args", args);

            // This optional expression is the LHS of the call.
            let ty = if let Some(ref expr) = call.expression {
                let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the function.
                let ty = compile_expression(expr, chunk)?;
                debug!("Expression::Call LHS ty", ty);
            } else {
                ValueType::new_empty(&lu_dog.read().unwrap())
            };

            // So we need to figure out the type this is being called on.
            match (&call.subtype, ty) {
                //
                // FunctionCall
                //
                (CallEnum::FunctionCall(_), ty) => Ok(ty),
                //
                // MethodCall
                //
                (CallEnum::MethodCall(meth), ty) => {
                    let meth = lu_dog.read().unwrap().exhume_method_call(meth).unwrap();
                    let meth = &meth.read().unwrap().name;
                    debug!("MethodCall method", meth);
                    debug!("MethodCall type", ty);

                    match &mut value {
                        Value::ProxyType(pt) => {
                            let arg_values = if !args.is_empty() {
                                let mut arg_values = VecDeque::with_capacity(args.len());
                                let mut next = args
                                    .iter()
                                    .find(|a| {
                                        a.read()
                                            .unwrap()
                                            .r27c_argument(&lu_dog.read().unwrap())
                                            .is_empty()
                                    })
                                    .unwrap()
                                    .clone();

                                loop {
                                    let expr = lu_dog
                                        .read()
                                        .unwrap()
                                        .exhume_expression(&next.read().unwrap().expression)
                                        .unwrap();
                                    let (value, _ty) = compile_expression(expr, chunk)?;
                                    arg_values.push_back(value);

                                    let next_id = { next.read().unwrap().next };
                                    if let Some(ref id) = next_id {
                                        next = lu_dog.read().unwrap().exhume_argument(id).unwrap();
                                    } else {
                                        break;
                                    }
                                }

                                arg_values
                            } else {
                                VecDeque::new()
                            };

                            pt.write().unwrap().call(meth, arg_values)
                        }
                        // Value::UserType(ut) => {}
                        bar => panic!("need to deal with Value {:?}", bar),
                    }
                }
                //
                // StaticMethodCall
                //
                (CallEnum::StaticMethodCall(meth), _, _) => {
                    let meth = lu_dog
                        .read()
                        .unwrap()
                        .exhume_static_method_call(meth)
                        .unwrap();
                    let call = meth.read().unwrap().r30_call(&lu_dog.read().unwrap())[0].clone();
                    let args = call.read().unwrap().r28_argument(&lu_dog.read().unwrap());

                    // This is for method call on a store type, and we do it out here so that we don't have
                    // to borrow stack mutably more than once.
                    let arg_values = if !args.is_empty() {
                        let mut arg_values = VecDeque::with_capacity(args.len());
                        let mut next = args
                            .iter()
                            .find(|a| {
                                a.read()
                                    .unwrap()
                                    .r27c_argument(&lu_dog.read().unwrap())
                                    .is_empty()
                            })
                            .unwrap()
                            .clone();

                        loop {
                            let expr = lu_dog
                                .read()
                                .unwrap()
                                .exhume_expression(&next.read().unwrap().expression)
                                .unwrap();
                            let (value, _ty) = compile_expression(expr, chunk)?;
                            arg_values.push_back(value);

                            let next_id = { next.read().unwrap().next };
                            if let Some(ref id) = next_id {
                                next = lu_dog.read().unwrap().exhume_argument(id).unwrap();
                            } else {
                                break;
                            }
                        }

                        arg_values
                    } else {
                        VecDeque::new()
                    };

                    let ty = &meth.read().unwrap().ty;
                    let func = &meth.read().unwrap().func;
                    debug!("StaticMethodCall ty", ty);
                    debug!("StaticMethodCall func", func);

                    // This is dirty. Down and dirty...
                    if ty == "Uuid" && func == "new" {
                        let value = Value::Uuid(Uuid::new_v4());
                        let ty = Ty::new_s_uuid();
                        let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                        Ok((value, ty))
                    } else if let Some(value) = stack.get_meta(ty, func) {
                        debug!("StaticMethodCall meta value", value);
                        match &value {
                            Value::Function(ref func) => {
                                let func = lu_dog
                                    .read()
                                    .unwrap()
                                    .exhume_function(&func.read().unwrap().id)
                                    .unwrap();
                                debug!("StaticMethodCall meta func", func);
                                let (value, ty) = eval_function_call(func, &args, chunk)?;
                                debug!("StaticMethodCall meta value", value);
                                debug!("StaticMethodCall meta ty", ty);
                                Ok((value, ty))
                            }
                            value => {
                                error!("deal with call expression", value);
                                Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
                            }
                        }
                    } else if let Some(mut value) = stack.get_mut(ty) {
                        debug!("StaticMethodCall frame value", value);
                        match &mut value {
                            Value::Function(ref func) => {
                                let func = lu_dog
                                    .read()
                                    .unwrap()
                                    .exhume_function(&func.read().unwrap().id)
                                    .unwrap();
                                debug!("StaticMethodCall frame func", func);
                                let (value, ty) = eval_function_call(func, &args, chunk)?;
                                debug!("StaticMethodCall frame value", value);
                                debug!("StaticMethodCall frame ty", ty);
                                Ok((value, ty))
                            }
                            Value::ProxyType(ut) => {
                                debug!("StaticMethodCall proxy", ut);
                                ut.write().unwrap().call(func, arg_values)
                            }
                            // Value::StoreType(ref mut store_type) => {
                            //     // We should actually know what's behind the curtain, since
                            //     // we requested it with `stack.get(ty)`, above.
                            //     match store_type {
                            //         StoreType::Inflection(ref mut inf) => {
                            //             let args: Vec<Value> = Vec::new();
                            //             inf.call(func, &args)
                            //         }
                            //         _ => Ok((
                            //             Value::Error("make point work".to_owned()),
                            //             ValueType::new_empty(),
                            //         )),
                            //     }
                            // }
                            value => {
                                error!("deal with call expression", value);
                                Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
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
                        Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
                    }
                }
            }
        }
        //
        // Error Expression
        //
        // 🚧 This should be looked at as part of  The Great Error Overhaul
        //
        Expression::ErrorExpression(ref error) => {
            let error = lu_dog
                .read()
                .unwrap()
                .exhume_error_expression(error)
                .unwrap();

            print!("\t{}", error.read().unwrap().span);

            Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
        //
        // FieldAccess
        //
        Expression::FieldAccess(ref field) => {
            let field = lu_dog.read().unwrap().exhume_field_access(field).unwrap();

            debug!("FieldAccess field", field);

            let field_name = &field.read().unwrap().name;

            // What we're doing below is actualy dereferencing a pointer. I wonder
            // if there is a way to make this less confusing and error prone? A
            // macro wouldn't work because the pointer is stored under vairous
            // names. So it would be a function on the referrer. Like realationship
            // navigation, actually.
            //      `let expr = field.expression(lu_dog).unwrap()`
            // Something like that.
            // A macro could maybe do it, if we pass the name of the field storing
            // the pointer, actually.
            //
            let expr = &field.read().unwrap().expression;
            let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
            // dereference!(field, expression, lu_dog);

            let (value, _ty) = compile_expression(expr, chunk)?;

            match value {
                Value::ProxyType(value) => {
                    let value = value.read().unwrap();
                    let value = value.get_attr_value(field_name)?;
                    let ty = value.get_type(&lu_dog.read().unwrap());

                    Ok((value.to_owned(), ty))
                }
                Value::UserType(value) => {
                    let value = value.read().unwrap();
                    let value = value.get_attr_value(field_name).unwrap();
                    let ty = value.get_type(&lu_dog.read().unwrap());

                    Ok((value.to_owned(), ty))
                }
                _ => Err(ChaChaError::BadJuJu {
                    message: "Bad value in field access".to_owned(),
                    location: location!(),
                }),
            }
        }
        //
        // For Loop
        //
        Expression::ForLoop(ref for_loop) => {
            debug!("ForLoop", for_loop);

            // let for_loop = lu_dog.read().unwrap().exhume_for_loop(for_loop).unwrap();
            // let for_loop = for_loop.read().unwrap();
            // let ident = &for_loop.ident;

            Err(ChaChaError::BadJuJu {
                message: "This isn't implemented yet".to_owned(),
                location: location!(),
            })
        }
        //
        // Literal
        //
        Expression::Literal(ref literal) => {
            let literal = lu_dog.read().unwrap().exhume_literal(literal).unwrap();
            let z = match &*literal.read().unwrap() {
                //
                // BooleanLiteral
                //
                Literal::BooleanLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_boolean_literal(literal)
                        .unwrap();
                    let literal = literal.read().unwrap();
                    let ty = Ty::new_boolean();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    match *literal {
                        BooleanLiteral::FalseLiteral(_) => Ok((Value::Boolean(false), ty)),
                        BooleanLiteral::TrueLiteral(_) => Ok((Value::Boolean(true), ty)),
                    }
                }
                //
                // FloatLiteral
                //
                Literal::FloatLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_float_literal(literal)
                        .unwrap();
                    let value = literal.read().unwrap().value;
                    let value = Value::Float(value);
                    let ty = Ty::new_float();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty))
                }
                //
                // IntegerLiteral
                //
                Literal::IntegerLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_integer_literal(literal)
                        .unwrap();
                    let value = literal.read().unwrap().value;
                    let value = Value::Integer(value);
                    let ty = Ty::new_integer();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty))
                }
                //
                // StringLiteral
                //
                Literal::StringLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_string_literal(literal)
                        .unwrap();
                    // 🚧 It'd be great if this were an Rc...
                    let value = Value::String(literal.read().unwrap().value.clone());
                    let ty = Ty::new_s_string();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty))
                }
            };
            z
        }
        //
        // Operator
        //
        Expression::Operator(ref operator) => {
            let operator = lu_dog.read().unwrap().exhume_operator(operator).unwrap();
            let operator = operator.read().unwrap();
            let lhs = lu_dog
                .read()
                .unwrap()
                .exhume_expression(&operator.lhs)
                .unwrap();
            let (lhs, lhs_ty) = compile_expression(lhs, chunk)?;
            let rhs = if let Some(ref rhs) = operator.rhs {
                let rhs = lu_dog.read().unwrap().exhume_expression(rhs).unwrap();
                let (rhs, _rhs_ty) = compile_expression(rhs, chunk)?;
                Some(rhs)
            } else {
                None
            };

            match &operator.subtype {
                OperatorEnum::Binary(ref binary) => {
                    let binary = lu_dog.read().unwrap().exhume_binary(binary).unwrap();
                    let binary = binary.read().unwrap();
                    match &*binary {
                        Binary::Addition(_) => {
                            let value = lhs + rhs.unwrap();
                            Ok((value, lhs_ty))
                        }
                        Binary::Subtraction(_) => {
                            let value = lhs - rhs.unwrap();
                            Ok((value, lhs_ty))
                        }
                    }
                }
                OperatorEnum::Comparison(ref comp) => {
                    let comp = lu_dog.read().unwrap().exhume_comparison(comp).unwrap();
                    let comp = comp.read().unwrap();
                    match &*comp {
                        Comparison::LessThanOrEqual(_) => {
                            let value = lhs.lte(&rhs.unwrap());
                            let value = Value::Boolean(value);
                            let ty = Ty::new_boolean();
                            let ty = ValueType::new_ty(&ty, &mut lu_dog.write().unwrap());

                            Ok((value, ty))
                        }
                        _ => unimplemented!(),
                    }
                }
            }
        }
        //
        // Print
        //
        Expression::Print(ref print) => {
            let print = lu_dog.read().unwrap().exhume_print(print).unwrap();
            debug!("Expression::Print print", print);
            let expr = print
                .read()
                .unwrap()
                .r32_expression(&lu_dog.read().unwrap())[0]
                .clone();
            let (value, _) = compile_expression(expr, chunk)?;
            let result = format!("{}", value);
            let result = result.replace("\\n", "\n");
            print!("\t{}", result_style.paint(result));

            Ok((value, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
        //
        // StructExpression
        //
        Expression::StructExpression(ref expr) => {
            let expr = lu_dog
                .read()
                .unwrap()
                .exhume_struct_expression(expr)
                .unwrap();
            let field_exprs = expr
                .read()
                .unwrap()
                .r26_field_expression(&lu_dog.read().unwrap());

            // Get name, value and type for each field expression.
            let field_exprs = field_exprs
                .iter()
                .map(|f| {
                    let expr = lu_dog
                        .read()
                        .unwrap()
                        .exhume_expression(&f.read().unwrap().expression)
                        .unwrap();
                    let (value, ty) = compile_expression(expr, chunk)?;
                    debug!("StructExpression field value", value);
                    debug!("StructExpression field ty", ty);
                    Ok((f.read().unwrap().name.clone(), ty, value))
                })
                .collect::<Result<Vec<_>>>()?;

            let woog_struct = expr
                .read()
                .unwrap()
                .r39_woog_struct(&lu_dog.read().unwrap())[0]
                .clone();
            let ty = lu_dog
                .read()
                .unwrap()
                .exhume_value_type(&woog_struct.read().unwrap().id)
                .unwrap();
            let fields = woog_struct
                .read()
                .unwrap()
                .r7_field(&lu_dog.read().unwrap());

            // Type checking fields here
            let mut user_type = UserType::new(ty.clone());
            for (name, ty, value) in field_exprs {
                if let Some(field) = fields.iter().find(|f| f.read().unwrap().name == name) {
                    let struct_ty = lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&field.read().unwrap().ty)
                        .unwrap();
                    ensure!(*struct_ty.read().unwrap() == *ty.read().unwrap(), {
                        let expected = PrintableValueType(struct_ty).to_string();
                        let got = PrintableValueType(ty).to_string();
                        TypeMismatchSnafu { expected, got }
                    });

                    // This is where we add the attribute value to the user type.
                    user_type.add_attr(&name, value);
                } else {
                    ensure!(
                        false,
                        NoSuchFieldSnafu {
                            field: name.to_owned(),
                        }
                    );
                }
            }

            Ok((Value::UserType(Arc::new(RwLock::new(user_type))), ty))
        }
        //
        // VariableExpression
        //
        Expression::VariableExpression(ref expr) => {
            let expr = lu_dog
                .read()
                .unwrap()
                .exhume_variable_expression(expr)
                .unwrap();
            debug!("Expression::VariableExpression", expr);
            let value = stack.get(&expr.read().unwrap().name);

            ensure!(value.is_some(), {
                let var = expr.read().unwrap().name.clone();
                VariableNotFoundSnafu { var }
            });

            let value = value.unwrap();

            no_debug!("Expression::VariableExpression", value);

            let ty = value.get_type(&lu_dog.read().unwrap());

            // Cloning the value isn't going to cut it I don't think. There are
            // three cases to consider. One is when the value is used read-only.
            // Cloning is find here. If the value is mutated however, we would
            // either need to return a reference, or write the value when it's
            // modified. The third thing is when the value is a reference. By
            // that I mean the type (above) is a reference. Not even sure what
            // to think about that atm.
            Ok((value.clone(), ty))
        }
        //
        // XIf
        //
        Expression::XIf(ref expr) => {
            let expr = lu_dog.read().unwrap().exhume_x_if(expr).unwrap();
            let expr = expr.read().unwrap();
            debug!("Expression::XIf", expr);

            let cond_expr = lu_dog
                .read()
                .unwrap()
                .exhume_expression(&expr.test)
                .unwrap();

            let (cond, _ty) = compile_expression(cond_expr, chunk)?;
            debug!("Expression::XIf conditional", cond);

            let (value, ty) = if cond.try_into()? {
                // Evaluate the true block
                let block = lu_dog
                    .read()
                    .unwrap()
                    .exhume_expression(&expr.true_block)
                    .unwrap();
                compile_expression(block, chunk)?
            } else {
                debug!("Expression::XIf else");
                if let Some(expr) = &expr.false_block {
                    debug!("Expression::XIf false block");
                    // Evaluate the false block
                    let block = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
                    compile_expression(block, chunk)?
                } else {
                    (Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap()))
                }
            };

            Ok((value, ty))
        }
        //
        // XReturn
        //
        Expression::XReturn(ref expr) => {
            let expr = lu_dog.read().unwrap().exhume_x_return(expr).unwrap();
            debug!("Expression::XReturn", expr);

            let expr = &expr.read().unwrap().expression;
            let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();

            let (value, ty) = compile_expression(expr, chunk)?;
            Err(ChaChaError::Return {
                value: value,
                ty: ty,
            })
        }
        ref alpha => {
            ensure!(
                false,
                UnimplementedSnafu {
                    message: format!("deal with expression: {:?}", alpha),
                }
            );

            Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
    }
}

fn compile_statement(
    statement: Arc<RwLock<Statement>>,
    chunk: &mut Chunk,
) -> Result<(Value, Arc<RwLock<ValueType>>)> {
    let lu_dog = &LU_DOG;

    debug!("compile_statement statement", statement);
    trace!("compile_statement chunk", chunk);

    match statement.read().unwrap().subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog
                .read()
                .unwrap()
                .exhume_expression_statement(stmt)
                .unwrap();
            let stmt = stmt.read().unwrap();
            let expr = stmt.r31_expression(&lu_dog.read().unwrap())[0].clone();
            let (value, ty) = compile_expression(expr, chunk)?;
            no_debug!("StatementEnum::ExpressionStatement: value", value);
            debug!("StatementEnum::ExpressionStatement: ty", ty);

            Ok((value, ty))
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = lu_dog.read().unwrap().exhume_let_statement(stmt).unwrap();
            let stmt = stmt.read().unwrap();
            debug!("StatementEnum::LetStatement: stmt", stmt);

            let expr = stmt.r20_expression(&lu_dog.read().unwrap())[0].clone();
            debug!("expr", expr);

            let (value, ty) = compile_expression(expr, chunk)?;
            debug!("value", value);
            debug!("ty", ty);

            let var = stmt.r21_local_variable(&lu_dog.read().unwrap())[0]
                .read()
                .unwrap()
                .clone();
            let var = var.r12_variable(&lu_dog.read().unwrap())[0]
                .read()
                .unwrap()
                .clone();
            debug!("var", var);

            log::debug!("inserting {} = {}", var.name, value);

            Ok((Value::Empty, ty))
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = lu_dog
                .read()
                .unwrap()
                .exhume_result_statement(stmt)
                .unwrap();
            let stmt = stmt.read().unwrap();
            debug!("StatementEnum::ResultStatement: stmt", stmt);

            let expr = stmt.r41_expression(&lu_dog.read().unwrap())[0].clone();
            debug!("StatementEnum::ResultStatement expr", expr);

            let (value, ty) = compile_expression(expr, chunk)?;
            debug!("StatementEnum::ResultStatement value", value);
            debug!("StatementEnum::ResultStatement ty", ty);

            Ok((value, ty))
        }
        ref beta => {
            error!("deal with statement", beta);
            Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
    }
}
