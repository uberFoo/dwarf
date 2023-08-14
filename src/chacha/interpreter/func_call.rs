use std::{path::Path, time::Instant};

use abi_stable::library::{lib_header_from_path, LibrarySuffix, RawLibrary};
use ansi_term::Colour;
use snafu::{location, prelude::*, Location};
use tracy_client::span;

use crate::{
    chacha::{
        error::{Result, WrongNumberOfArgumentsSnafu},
        value::FfiValue,
        vm::VM,
    },
    interpreter::{
        debug, error, eval_expression, eval_statement, function, trace, typecheck, ChaChaError,
        Context,
    },
    lu_dog::{Argument, BodyEnum, Function, List, Span, ValueType},
    new_ref,
    plug_in::PluginModRef,
    plug_in::PluginType,
    s_read, s_write, NewRef, RefType, Value,
};

const OBJECT_STORE: &str = "ObjectStore";
const FUNCTION_NEW: &str = "new";

pub fn eval_function_call(
    func: RefType<Function>,
    args: &[RefType<Argument>],
    arg_check: bool,
    span: &RefType<Span>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();
    context.increment_call_count();

    debug!("eval_function_call func {func:?}");
    trace!("eval_function_call stack {:?}", context.memory());

    span!("eval_function_call");

    let body = s_read!(func).r19_body(&s_read!(lu_dog))[0].clone();

    let body = s_read!(body);
    match body.subtype {
        //
        // This is a function defined in a dwarf file.
        BodyEnum::Block(ref id) => {
            eval_built_in_function_call(func, id, args, arg_check, span, context, vm)
        }
        //
        // This is an externally defined function that was declared in a dwarf file.
        BodyEnum::ExternalImplementation(ref id) => {
            eval_external_function_call(id, args, arg_check, span, context, vm)
        }
    }
}

fn eval_external_function_call(
    block_id: &usize,
    args: &[RefType<Argument>],
    _arg_check: bool,
    _span: &RefType<Span>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let external = s_read!(lu_dog)
        .exhume_external_implementation(block_id)
        .unwrap();

    let arg_values = if !args.is_empty() {
        let mut arg_values = Vec::with_capacity(args.len());
        let mut next = args
            .iter()
            .find(|a| s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty())
            .unwrap()
            .clone();

        loop {
            let expr = s_read!(next).r37_expression(&s_read!(lu_dog))[0].clone();
            let (value, ty) = eval_expression(expr.clone(), context, vm)?;
            arg_values.push((expr, value, ty));

            let next_id = { s_read!(next).next };
            if let Some(ref id) = next_id {
                next = s_read!(lu_dog).exhume_argument(id).unwrap();
            } else {
                break;
            }
        }

        arg_values
    } else {
        Vec::new()
    };

    let model_name = s_read!(external).x_model.clone();
    let mut model = s_write!(context.models());
    let model = model.get_mut(&model_name).unwrap();
    let func_name = s_read!(external).function.clone();

    let object_name = &s_read!(external).object;
    let object_name = object_name.clone();
    if object_name == OBJECT_STORE {
        // Here we load the plug-in and create an instance of the object store.
        if s_read!(external).function == FUNCTION_NEW {
            let library_path = RawLibrary::path_in_directory(
                Path::new(&format!("./plug-ins/{model_name}/target/debug")),
                &model_name,
                LibrarySuffix::NoSuffix,
            );
            let root_module = (|| {
                let header = lib_header_from_path(&library_path)?;
                header.init_root_module::<PluginModRef>()
            })()
            .map_err(|e| {
                eprintln!("{e}");
                ChaChaError::BadJuJu {
                    message: "Plug-in error".to_owned(),
                    location: location!(),
                }
            })?;

            let ctor = root_module.new();
            // let plugin = new_ref!(PluginType, ctor(vec![].into()).unwrap());
            let plugin = new_ref!(
                PluginType,
                ctor(vec![Value::String("../sarzak/models/sarzak.v2.json".into()).into()].into())
                    .unwrap()
            );
            model.1.replace(plugin.clone());

            let value = new_ref!(Value, Value::PlugIn(plugin));
            // We know that we'll find one of these because we created it when we
            // extruded.
            let store = s_read!(lu_dog)
                .iter_z_object_store()
                .find(|store| {
                    let store = s_read!(store);
                    store.domain == model_name
                })
                .unwrap();

            // 🚧 I should look this up, rather than creating a new one.
            let ty = ValueType::new_z_object_store(&store, &mut s_write!(lu_dog));
            Ok((value, ty))
        } else {
            dbg!("other");
            unimplemented!();
        }
    }
    // 🚧 Should these be wrapped in a mutex-like?
    else if let Some(obj_id) = model.0.exhume_object_id_by_name(&object_name) {
        if let Some(plugin) = &model.1 {
            // 🚧 Don't unwrap -- error handling.
            let proxy_obj = s_write!(plugin)
                .invoke_func(
                    object_name.as_str().into(),
                    func_name.as_str().into(),
                    arg_values
                        .into_iter()
                        .map(|(_, value, _)| {
                            let value = s_read!(value).clone();
                            <Value as Into<FfiValue>>::into(value)
                        })
                        .collect::<Vec<_>>()
                        .into(),
                )
                .unwrap();
            match proxy_obj {
                FfiValue::ProxyType(proxy_obj) => {
                    let value = new_ref!(Value, Value::ProxyType((obj_id, proxy_obj.plugin)));

                    let woog_struct = s_read!(lu_dog)
                        .iter_woog_struct()
                        .find(|woog| {
                            let woog = s_read!(woog);
                            woog.name == object_name
                        })
                        .unwrap();
                    let ty = ValueType::new_woog_struct(&woog_struct, &mut s_write!(lu_dog));

                    Ok((value, ty))
                }
                FfiValue::Vector(vec) => {
                    let vec = vec
                        .into_iter()
                        .map(Value::from)
                        .map(|v| new_ref!(Value, v))
                        .collect::<Vec<_>>();
                    let value = new_ref!(Value, Value::Vector(vec));

                    let woog_struct = s_read!(lu_dog)
                        .iter_woog_struct()
                        .find(|woog| {
                            let woog = s_read!(woog);
                            woog.name == object_name
                        })
                        .unwrap();
                    let ty = ValueType::new_woog_struct(&woog_struct, &mut s_write!(lu_dog));
                    let list = List::new(&ty, &mut s_write!(lu_dog));
                    let ty = ValueType::new_list(&list, &mut s_write!(lu_dog));

                    Ok((value, ty))
                }
                all_manner_of_things => {
                    panic!("{all_manner_of_things:?} is not a proxy");
                }
            }
        } else {
            panic!("no plugin");
        }
    } else {
        error!("object not found");
        unimplemented!()
    }
}

fn eval_built_in_function_call(
    func: RefType<Function>,
    block_id: &usize,
    args: &[RefType<Argument>],
    arg_check: bool,
    span: &RefType<Span>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let func = s_read!(func);

    let block = s_read!(lu_dog).exhume_block(block_id).unwrap();
    let has_stmts = !s_read!(block).r18_statement(&s_read!(lu_dog)).is_empty();

    if has_stmts {
        // Collect timing info
        let now = Instant::now();
        let expr_count_start = context.get_expression_count();

        context.memory().push_frame();

        // We need to evaluate the arguments, and then push them onto the stack. We
        // also need to typecheck the arguments against the function parameters.
        // We need to look the params up anyway to set the local variables.
        let params = func.r13_parameter(&s_read!(lu_dog));

        // 🚧 I'd really like to see the source code printed out, with the function
        // call highlighted.
        // And can't we catch this is the compiler?
        ensure!(params.len() == args.len(), {
            let value_ty = &func.r1_value_type(&s_read!(lu_dog))[0];
            let defn_span = &s_read!(value_ty).r62_span(&s_read!(lu_dog))[0];
            let read = s_read!(defn_span);
            let defn_span = read.start as usize..read.end as usize;

            let read = s_read!(span);
            let invocation_span = read.start as usize..read.end as usize;

            WrongNumberOfArgumentsSnafu {
                expected: params.len(),
                got: args.len(),
                defn_span,
                invocation_span,
            }
        });

        let params = if !params.is_empty() {
            let mut params = Vec::with_capacity(params.len());
            let mut next = func
                .r13_parameter(&s_read!(lu_dog))
                .iter()
                .find(|p| s_read!(p).r14c_parameter(&s_read!(lu_dog)).is_empty())
                .unwrap()
                .clone();

            loop {
                // Apparently I'm being clever. I don't typecheck against an actual
                // type associated with the parameter. No, I am looking up the variable
                // associated with the parameter and using it's type. I guess that's cool,
                // but it's tricky if you aren't aware.
                let var = s_read!(s_read!(next).r12_variable(&s_read!(lu_dog))[0]).clone();
                let value = s_read!(var.r11_x_value(&s_read!(lu_dog))[0]).clone();
                let ty = value.r24_value_type(&s_read!(lu_dog))[0].clone();
                params.push((var.name.clone(), ty.clone()));

                let next_id = { s_read!(next).next };
                if let Some(ref id) = next_id {
                    next = s_read!(lu_dog).exhume_parameter(id).unwrap();
                } else {
                    break;
                }
            }

            params
        } else {
            Vec::new()
        };

        let arg_values = if !args.is_empty() {
            let mut arg_values = Vec::with_capacity(args.len());
            let mut next = args
                .iter()
                .find(|a| s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty())
                .unwrap()
                .clone();

            loop {
                let expr = s_read!(next).r37_expression(&s_read!(lu_dog))[0].clone();
                let (value, ty) = eval_expression(expr.clone(), context, vm)?;
                arg_values.push((expr, value, ty));

                let next_id = { s_read!(next).next };
                if let Some(ref id) = next_id {
                    next = s_read!(lu_dog).exhume_argument(id).unwrap();
                } else {
                    break;
                }
            }

            arg_values
        } else {
            Vec::new()
        };

        let zipped = params.into_iter().zip(arg_values);
        for ((name, param_ty), (expr, value, arg_ty)) in zipped {
            debug!("type check name {name:?}");
            debug!("type check param_ty {param_ty:?}");
            debug!("type check value {value:?}");
            debug!("type check arg_ty {arg_ty:?}");

            if arg_check {
                let x_value = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];
                let span = &s_read!(x_value).r63_span(&s_read!(lu_dog))[0];

                typecheck(&param_ty, &arg_ty, span, location!(), context)?;
            }

            context.memory().insert(name.clone(), value);
        }

        let mut value = new_ref!(Value, Value::Empty);
        let mut ty = Value::Empty.get_type(&s_read!(sarzak), &s_read!(lu_dog));
        if let Some(ref id) = s_read!(block).statement {
            let mut next = s_read!(lu_dog).exhume_statement(id).unwrap();

            loop {
                let result = eval_statement(next.clone(), context, vm).map_err(|e| {
                    context.memory().pop_frame();
                    e
                });

                if let Err(ChaChaError::Return { value, ty }) = &result {
                    return Ok((value.clone(), ty.clone()));
                }

                (value, ty) = result?;

                if let Some(ref id) = s_read!(next.clone()).next {
                    next = s_read!(lu_dog).exhume_statement(id).unwrap();
                } else {
                    break;
                }
            }
        }

        // Clean up
        context.memory().pop_frame();
        let elapsed = now.elapsed();
        // Counting 10k expressions per second
        let eps = (context.get_expression_count() - expr_count_start) as f64
            / elapsed.as_micros() as f64
            * 10.0;
        context.new_timing(eps);

        Ok((value, ty))
    } else {
        Ok((
            new_ref!(Value, Value::Empty),
            Value::Empty.get_type(&s_read!(sarzak), &s_read!(lu_dog)),
        ))
    }
}
