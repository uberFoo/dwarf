use std::{path::Path, time::Instant};

use abi_stable::{
    library::{lib_header_from_path, LibrarySuffix, RawLibrary},
    std_types::{RErr, ROk},
};
use ansi_term::Colour;
use heck::ToUpperCamelCase;
use snafu::{location, prelude::*, Location};
#[cfg(feature = "tracy")]
use tracy_client::span;

#[cfg(feature = "async")]
use tracing::{debug_span, Instrument};

use crate::{
    chacha::{
        error::{Result, WrongNumberOfArgumentsSnafu},
        ffi_value::FfiValue,
    },
    interpreter::{
        debug, error, eval_expression, eval_statement, function, trace, typecheck, ChaChaError,
        Context,
    },
    lu_dog::{
        Argument, BodyEnum, Expression, ExternalImplementation, Function,
        ObjectStore as LuDogStore, Span, ValueType,
    },
    new_ref,
    plug_in::PluginModRef,
    plug_in::PluginType,
    s_read, s_write,
    sarzak::ObjectStore,
    Desanitize, NewRef, RefType, SarzakStorePtr, Value, FUNCTION_LOAD, FUNCTION_NEW, MERLIN,
    OBJECT_STORE, SARZAK,
};

pub fn eval_function_call(
    func: RefType<Function>,
    args: &[RefType<Argument>],
    first_arg: Option<SarzakStorePtr>,
    arg_check: bool,
    span: &RefType<Span>,
    context: &mut Context,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    context.increment_call_count();

    debug!("eval_function_call func {func:?}");
    trace!("eval_function_call stack {:?}", context.memory());

    #[cfg(feature = "tracy")]
    span!("eval_function_call");

    let body = s_read!(func).r19_body(&s_read!(lu_dog))[0].clone();

    #[cfg(feature = "async")]
    let task_name: String = s_read!(func).name.to_owned();

    if s_read!(body).a_sink {
        #[cfg(not(feature = "async"))]
        {
            Ok(new_ref!(
                Value,
                Value::Error(Box::new(ChaChaError::AsyncNotSupported))
            ))
        }
        #[cfg(feature = "async")]
        {
            let args = args.to_owned();
            let span = span.to_owned();
            let mut cloned_context = context.clone();

            let t_span = debug_span!("async func_call", target = "async");

            let future = async move {
                inner_eval_function_call(
                    func,
                    &args,
                    first_arg,
                    arg_check,
                    &span,
                    &mut cloned_context,
                )
            }
            .instrument(t_span);

            let task = context.worker().unwrap().create_task(future).unwrap();

            let value = new_ref!(
                Value,
                Value::Future {
                    name: task_name,
                    executor: context.executor().clone(),
                    task: Some(task)
                }
            );

            Ok(value)
        }
    } else {
        inner_eval_function_call(func, args, first_arg, arg_check, span, context)
    }
}

fn inner_eval_function_call(
    func: RefType<Function>,
    args: &[RefType<Argument>],
    first_arg: Option<SarzakStorePtr>,
    arg_check: bool,
    span: &RefType<Span>,
    context: &mut Context,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    debug!("inner_eval_function_call func {func:?}");
    trace!("inner_eval_function_call stack {:?}", context.memory());

    #[cfg(feature = "tracy")]
    span!("inner_eval_function_call");

    let body = s_read!(func).r19_body(&s_read!(lu_dog))[0].clone();
    let body = s_read!(body);
    match body.subtype {
        //
        // This is a function defined in a dwarf file.
        BodyEnum::Block(ref id) => {
            eval_built_in_function_call(func, id, args, first_arg, arg_check, span, context)
        }
        //
        // This is an externally defined function that was declared in a dwarf file.
        BodyEnum::ExternalImplementation(ref id) => {
            eval_external_method(id, args, first_arg, arg_check, span, context)
        }
    }
}

/// Evaluate a static method call in a dynamic library.
///
/// Note that the func_name below comes from the annotation, as below (in this
/// example, the answer is "flubber"):
/// ```ignore
///#[proxy(store = "sarzak", object = "Boolean", func = "flubber")]
///```
fn eval_external_method(
    block_id: &SarzakStorePtr,
    args: &[RefType<Argument>],
    first_arg: Option<SarzakStorePtr>,
    _arg_check: bool,
    span: &RefType<Span>,
    context: &mut Context,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let external = s_read!(lu_dog)
        .exhume_external_implementation(block_id)
        .unwrap();

    let mut arg_values = if let Some(next) = first_arg {
        let mut arg_values = Vec::with_capacity(args.len());
        let mut next = s_read!(lu_dog).exhume_argument(&next).unwrap();

        loop {
            let expr = s_read!(next).r37_expression(&s_read!(lu_dog))[0].clone();
            let value = eval_expression(expr.clone(), context)?;
            arg_values.push((expr, value));

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
    // This is a hack since merlin and sarzak are in the same plugin, and it's
    // called sarzak.
    let model_name = if model_name == MERLIN {
        SARZAK.to_owned()
    } else {
        model_name
    };
    let mut models = s_write!(context.models().models());
    let model = models.get_mut(&model_name).unwrap();
    let func_name = s_read!(external).function.clone();

    let object_name = &s_read!(external).object;
    let object_name = object_name.to_upper_camel_case();

    if object_name == OBJECT_STORE {
        objectstore_static_methods(
            &model_name,
            model,
            &mut arg_values,
            &external,
            span,
            context,
            &lu_dog,
        )
    }
    // 🚧 Should these be wrapped in a mutex-like?
    else if let Some(obj_id) = model.0.exhume_object_id_by_name(&object_name.desanitize()) {
        if let Some(plugin) = &model.1 {
            match s_write!(plugin).invoke_func(
                model_name.as_str().into(),
                object_name.as_str().into(),
                func_name.as_str().into(),
                arg_values
                    .into_iter()
                    .map(|(_, value)| {
                        let value = s_read!(value).clone();
                        <Value as Into<FfiValue>>::into(value)
                    })
                    .collect::<Vec<_>>()
                    .into(),
            ) {
                ROk(proxy_obj) => match proxy_obj {
                    FfiValue::ProxyType(proxy_obj) => {
                        let value = new_ref!(
                            Value,
                            Value::ProxyType {
                                module: model_name,
                                obj_ty: obj_id,
                                id: proxy_obj.id.into(),
                                plugin: new_ref!(PluginType, proxy_obj.plugin)
                            }
                        );

                        Ok(value)
                    }
                    FfiValue::Vector(vec) => {
                        let vec = vec
                            .into_iter()
                            .map(|k| Value::from((k, &*s_read!(lu_dog))))
                            .map(|v| new_ref!(Value, v))
                            .collect::<Vec<_>>();
                        let ty = if vec.is_empty() {
                            ValueType::new_empty(true, &mut s_write!(lu_dog))
                        } else {
                            s_read!(vec[0])
                                .get_value_type(&s_read!(context.sarzak_heel()), &s_read!(lu_dog))
                        };
                        let value = new_ref!(
                            Value,
                            Value::Vector {
                                ty,
                                inner: new_ref!(Vec<RefType<Value>>, vec)
                            }
                        );

                        Ok(value)
                    }
                    all_manner_of_things => {
                        panic!("{all_manner_of_things:?} is not a proxy for model {model_name}.");
                    }
                },
                RErr(e) => {
                    dbg!(e);
                    Err(ChaChaError::NoSuchMethod {
                        method: func_name,
                        span: s_read!(span).start as usize..s_read!(span).end as usize,
                        location: location!(),
                    })
                }
            }
        } else {
            panic!("no plugin");
        }
    } else {
        error!("object not found: {object_name}");
        unimplemented!()
    }
}

#[allow(clippy::too_many_arguments)]
fn eval_built_in_function_call(
    func: RefType<Function>,
    block_id: &SarzakStorePtr,
    args: &[RefType<Argument>],
    first_arg: Option<SarzakStorePtr>,
    arg_check: bool,
    span: &RefType<Span>,
    context: &mut Context,
) -> Result<RefType<Value>> {
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
                location: location!(),
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

        let arg_values = if let Some(next) = first_arg {
            let mut arg_values = Vec::with_capacity(args.len());
            let mut next = s_read!(lu_dog).exhume_argument(&next).unwrap();

            loop {
                let expr = s_read!(next).r37_expression(&s_read!(lu_dog))[0].clone();
                let value = eval_expression(expr.clone(), context)?;
                arg_values.push((expr, value));

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
        for ((name, param_ty), (expr, value)) in zipped {
            debug!("type check name {name:?}");
            debug!("type check param_ty {param_ty:?}");
            debug!("type check value {value:?}");

            if arg_check {
                let arg_ty = s_read!(value).get_value_type(&s_read!(sarzak), &s_read!(lu_dog));
                let x_value = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];
                let span = &s_read!(x_value).r63_span(&s_read!(lu_dog))[0];

                debug!("type check arg_ty {arg_ty:?}");
                typecheck(&param_ty, &arg_ty, span, location!(), context)?;
            }

            context.memory().insert(name.clone(), value);
        }

        let mut value = new_ref!(Value, Value::Empty);
        if let Some(ref id) = s_read!(block).statement {
            let mut next = s_read!(lu_dog).exhume_statement(id).unwrap();

            loop {
                let result = eval_statement(next.clone(), context).map_err(|e| {
                    context.memory().pop_frame();
                    e
                });

                if let Err(ChaChaError::Return { value, ty: _ }) = &result {
                    return Ok(value.clone());
                }

                value = result?;

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

        Ok(value)
    } else {
        Ok(new_ref!(Value, Value::Empty))
    }
}

fn objectstore_static_methods(
    model_name: &String,
    model: &mut (ObjectStore, Option<RefType<PluginType>>),
    arg_values: &mut Vec<(RefType<Expression>, RefType<Value>)>,
    external: &RefType<ExternalImplementation>,
    span: &RefType<Span>,
    context: &Context,
    lu_dog: &RefType<LuDogStore>,
) -> Result<RefType<Value>> {
    // We know that we'll find one of these because we created it when we
    // extruded.
    let store = s_read!(lu_dog)
        .iter_z_object_store()
        .find(|store| {
            let store = s_read!(store);
            store.domain == *model_name
        })
        .unwrap();

    match s_read!(external).function.as_str() {
        FUNCTION_LOAD => {
            // This is where we actually instantiate the shared library.
            let library_path = RawLibrary::path_in_directory(
                Path::new(&format!(
                    "{}/extensions/{model_name}/lib",
                    context.get_home().display()
                )),
                model_name,
                LibrarySuffix::NoSuffix,
            );
            let root_module = (|| {
                let header = lib_header_from_path(&library_path)?;
                header.init_root_module::<PluginModRef>()
            })()
            .map_err(|e| {
                eprintln!("{e}");
                ChaChaError::BadnessHappened {
                    message: "Plug-in error".to_owned(),
                    location: location!(),
                }
            })?;

            let ctor = root_module.new();
            let (_, path) = arg_values.pop().unwrap();
            let path = s_read!(path).clone();
            let plugin = new_ref!(PluginType, ctor(vec![path.into()].into()).unwrap());
            model.1.replace(plugin.clone());

            let value = new_ref!(Value, Value::Store(store, plugin));

            Ok(value)
        }
        FUNCTION_NEW => {
            // This is where we actually instantiate the shared library.
            let library_path = RawLibrary::path_in_directory(
                Path::new(&format!(
                    "{}/extensions/{model_name}/lib",
                    context.get_home().display()
                )),
                model_name,
                LibrarySuffix::NoSuffix,
            );
            let root_module = (|| {
                let header = lib_header_from_path(&library_path)?;
                header.init_root_module::<PluginModRef>()
            })()
            .map_err(|e| {
                eprintln!("{e}");
                ChaChaError::BadnessHappened {
                    message: format!("Plug-in error: {e:?}"),
                    location: location!(),
                }
            })?;

            let ctor = root_module.new();
            let plugin = new_ref!(PluginType, ctor(vec![].into()).unwrap());
            model.1.replace(plugin.clone());

            let value = new_ref!(Value, Value::Store(store, plugin));

            Ok(value)
        }
        method => {
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            Err(ChaChaError::NoSuchMethod {
                method: method.to_owned(),
                span,
                location: location!(),
            })
        }
    }
}
