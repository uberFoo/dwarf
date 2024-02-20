use snafu::{location, Location};
use uuid::Uuid;

#[cfg(feature = "async")]
use crate::keywords::{LEN, PUSH, SPAWN};

use crate::{
    bubba::{
        compiler::{compile_expression, compile_statement, CThonk, Context, Result},
        instr::Instruction,
        value::Value,
        BOOL, STRING_ARRAY,
    },
    keywords::{ARGS, ASSERT, ASSERT_EQ, CHACHA, FORMAT, NEW, PLUGIN},
    lu_dog::{BodyEnum, Call, CallEnum, Expression, ValueType, ValueTypeEnum},
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Span, PATH_SEP, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    call: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let wrapped_call = lu_dog.exhume_call(call).unwrap();
    let call = s_read!(wrapped_call);

    tracing::debug!(target: "instr", "{}: {:?}\n  --> {}:{}:{}", POP_CLR.paint("compile_call"), call.subtype, file!(), line!(), column!());

    let first_arg = call.argument;
    let args = call.r28_argument(&lu_dog);

    let arg_exprs = if let Some(next) = first_arg {
        let mut exprs = Vec::with_capacity(args.len());
        let mut next = lu_dog.exhume_argument(&next).unwrap();

        loop {
            let expr = s_read!(next).r37_expression(&lu_dog)[0].clone();
            exprs.push(expr);

            let next_id = s_read!(next).next;
            if let Some(ref id) = next_id {
                next = lu_dog.exhume_argument(id).unwrap();
            } else {
                break;
            }
        }

        exprs
    } else {
        Vec::new()
    };

    match call.subtype {
        CallEnum::FunctionCall(ref call) => {
            let call = lu_dog.exhume_function_call(call).unwrap();
            let call = s_read!(call);
            compile_function_call(&call.name, wrapped_call.clone(), &arg_exprs, thonk, context)
        }
        CallEnum::MethodCall(ref meth) => {
            let meth = lu_dog.exhume_method_call(meth).unwrap();
            let meth = s_read!(meth);
            compile_method_call(
                meth.name.to_owned(),
                wrapped_call.clone(),
                &arg_exprs,
                thonk,
                context,
            )
        }
        CallEnum::StaticMethodCall(ref meth) => {
            let meth = lu_dog.exhume_static_method_call(meth).unwrap();
            let meth = s_read!(meth);
            compile_static_method_call(&meth.ty, &meth.func, &arg_exprs, thonk, context, span)
        }
        ref call => todo!("handle the other calls: {call:?}"),
    }
}

/// Compile a Lambda
///
/// We do compile the function here, and importantly, we push a pointer to the
/// function onto the stack.
///
/// Doing this in a single step is fine because the pointer will either be used
/// directly or stored in a variable.
#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile_lambda(
    λ: &SarzakStorePtr,
    outer_thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_lambda"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    context.push_symbol_table();
    let saved_captures = context.captures.take();
    context.captures = Some(Default::default());

    let wrapped_λ = lu_dog.exhume_lambda(λ).unwrap();
    let λ = s_read!(wrapped_λ);
    // let ret_ty = λ.r74_value_type(&lu_dog)[0].clone();
    let body = λ.r73_body(&lu_dog)[0].clone();
    let body = s_read!(body);
    let params = λ.r76_lambda_parameter(&lu_dog);
    // let mut name = "(".to_owned();

    let name = format!("{}", Uuid::new_v4());

    let mut thonk = CThonk::new(name.clone());

    if !params.is_empty() {
        let mut next = λ.r103_lambda_parameter(&lu_dog)[0].clone();
        loop {
            let param = next.clone();
            let param = s_read!(param);
            let var = s_read!(param.r12_variable(&lu_dog)[0]).clone();
            let ty = s_read!(param.r77_value_type(&lu_dog)[0]).clone();

            context.insert_symbol(var.name.clone(), ty);
            thonk.increment_frame_size();
            // name += var.name.as_str();
            // name += ",";

            let next_id = param.next;
            if let Some(ref id) = next_id {
                next = lu_dog.exhume_lambda_parameter(id).unwrap();
            } else {
                break;
            }
        }
    }

    // let ret_ty = PrintableValueType(&ret_ty, context.extruder_context, &lu_dog);
    // name += format!(")->{ret_ty:?}").as_str();
    match body.subtype {
        //
        // This is a function defined in a dwarf file.
        BodyEnum::Block(ref id) => {
            let block = lu_dog.exhume_block(id).unwrap();
            let has_stmts = !s_read!(block).r18_statement(&lu_dog).is_empty();

            if has_stmts {
                if let Some(ref id) = s_read!(block).statement {
                    let mut next = lu_dog.exhume_statement(id).unwrap();

                    loop {
                        compile_statement(&next, &mut thonk, context)?;

                        if let Some(ref id) = s_read!(next.clone()).next {
                            next = lu_dog.exhume_statement(id).unwrap();
                        // } else if thonk.returned {
                        //     break;
                        } else {
                            if !thonk.returned {
                                thonk.insert_instruction(
                                    Instruction::Push(new_ref!(Value, Value::Empty)),
                                    location!(),
                                );
                                thonk.insert_instruction(Instruction::Return, location!());
                                thonk.returned = true;
                            }
                            break;
                        }
                    }
                }
            } else {
                thonk.insert_instruction(
                    Instruction::Push(new_ref!(Value, Value::Empty)),
                    location!(),
                );
                thonk.insert_instruction(Instruction::Return, location!());
                thonk.returned = true;
            }
        }
        //
        // This is an externally defined function that was declared in a dwarf file.
        BodyEnum::ExternalImplementation(ref block_id) => {
            todo!("handle external implementation: {block_id:?}");
        }
    };

    context.pop_symbol_table();

    let frame_size = thonk.inner.frame_size();
    for (var, index) in context.captures.take().unwrap() {
        let Some(symbol) = context.get_symbol(&var) else {
            panic!("capture not found: {var}");
        };
        thonk.prefix_instruction(Instruction::CaptureLocal(symbol.number, index), location!());
    }

    context.captures = saved_captures;

    context.get_program().add_thonk(thonk.into());

    // outer_thonk.add_instruction(Instruction::Push(new_ref!(Value, pointer)), location!());
    outer_thonk.insert_instruction(
        Instruction::MakeLambdaPointer(new_ref!(String, name.clone()), frame_size),
        location!(),
    );

    Ok(Some((*s_read!(λ.r1_value_type(&lu_dog)[0])).clone()))
}

/// Compile a Function Call
///
/// Compile a function call expression, including it's arguments.
///
/// This ensures that the stack is setup for a function call, which we issue
/// at the tail of the function.
#[tracing::instrument]
fn compile_function_call(
    name: &str,
    call: RefType<Call>,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {name}\n -> {}:{}:{}", POP_CLR.paint("compile_function_call"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let a_sink = if let Some(func) = lu_dog.exhume_function_id_by_name(&name) {
        let func = lu_dog.exhume_function(&func).unwrap();
        let body = s_read!(func).r19_body(&lu_dog)[0].clone();
        let body = s_read!(body);
        body.a_sink
    } else {
        false
    };

    match name {
        FORMAT => todo!("handle FORMAT"),
        _ => {
            let call = s_read!(call);
            let result = if let Some(ref expr) = call.expression {
                let expr = lu_dog.exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the underlying value/instance.
                // I'm not sure that this is the correct type. OTOH, what other
                // type would I use?
                compile_expression(&expr, thonk, context)
            } else {
                panic!();
            };

            for expr in args {
                compile_expression(expr, thonk, context)?;
            }

            if a_sink {
                thonk.insert_instruction(Instruction::AsyncCall(args.len()), location!());
            } else {
                thonk.insert_instruction(Instruction::Call(args.len()), location!());
            }

            result
        }
    }
}

#[tracing::instrument]
fn compile_method_call(
    name: String,
    call: RefType<Call>,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {name}\n  -> {}:{}:{}", POP_CLR.paint("compile_method_call"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let sarzak = context.sarzak_heel();
    let sarzak = s_read!(sarzak);

    let a_sink = if let Some(func) = lu_dog.exhume_function_id_by_name(&name) {
        let func = lu_dog.exhume_function(&func).unwrap();
        let body = s_read!(func).r19_body(&lu_dog)[0].clone();
        let body = s_read!(body);
        body.a_sink
    } else {
        false
    };

    // First off we need to evaluate the expression associated with this call.
    let result = if let Some(ref expr) = s_read!(call).expression {
        let expr = lu_dog.exhume_expression(expr).unwrap();
        // Evaluate the LHS to get at the underlying value/instance.
        let result = compile_expression(&expr, thonk, context);

        match name.as_str() {
            PUSH => {
                match result.clone()?.unwrap().subtype {
                    ValueTypeEnum::List(_) => {
                        // skip self
                        for (_, expr) in args.iter().enumerate().skip(1) {
                            compile_expression(expr, thonk, context)?;
                        }

                        thonk.insert_instruction(Instruction::ListPush, location!());

                        return result;
                    }
                    _ => unreachable!(),
                }
            }
            LEN => match result.clone()?.unwrap().subtype {
                ValueTypeEnum::List(_) => {
                    thonk.insert_instruction(Instruction::ListLength, location!());
                    return result;
                }
                ValueTypeEnum::Ty(ref id) => {
                    let ty = sarzak.exhume_ty(id).unwrap();
                    let ty = ty.read().unwrap();

                    match &*ty {
                        Ty::ZString(_) => {
                            thonk.insert_instruction(Instruction::ListLength, location!());
                            return result;
                        }
                        darn => unreachable!("{darn:?}"),
                    }
                }
                whoa => unreachable!("{whoa:?}"),
            },
            _ => {}
        }

        thonk.insert_instruction(
            Instruction::MethodLookup(new_ref!(String, name)),
            location!(),
        );
        result
    } else {
        panic!();
    };

    // Don't forget that the first one of these is self. So it's effectively
    // re-evaluating the call.expression above. Somehow it's plumbed to do that.
    for expr in args {
        compile_expression(expr, thonk, context)?;
    }

    if a_sink {
        thonk.insert_instruction(Instruction::AsyncCall(args.len()), location!());
    } else {
        thonk.insert_instruction(Instruction::Call(args.len()), location!());
    }

    result
}

#[tracing::instrument]
fn compile_static_method_call(
    ty: &str,
    func: &str,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_static_method_call"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let boolean = context.get_type(BOOL).unwrap().clone();
    let string_array = context.get_type(STRING_ARRAY).unwrap().clone();

    match ty {
        CHACHA => match func {
            ARGS => {
                thonk.insert_instruction_with_span(
                    Instruction::PushArgs,
                    span.clone(),
                    location!(),
                );
                Ok(Some(string_array))
            }
            ASSERT => {
                let expr = &args[0];
                compile_expression(expr, thonk, context)?;
                thonk.insert_instruction_with_span(
                    Instruction::Push(new_ref!(Value, true.into())),
                    span.clone(),
                    location!(),
                );
                thonk.insert_instruction_with_span(
                    Instruction::TestEqual,
                    span.clone(),
                    location!(),
                );
                thonk.insert_instruction_with_span(
                    Instruction::JumpIfTrue(5),
                    span.clone(),
                    location!(),
                );
                thonk.insert_instruction_with_span(
                    Instruction::Push(new_ref!(
                        Value,
                        format!("assertion failed: {span:?}").into()
                    )),
                    span.clone(),
                    location!(),
                );
                thonk.insert_instruction_with_span(Instruction::Out(1), span.clone(), location!());

                // Bail on false
                thonk.insert_instruction(
                    Instruction::Push(new_ref!(
                        Value,
                        context.extruder_context.source_path.clone().into()
                    )),
                    location!(),
                );
                thonk.insert_instruction(
                    Instruction::Push(new_ref!(Value, span.clone().into())),
                    location!(),
                );
                thonk.insert_instruction(Instruction::HaltAndCatchFire, location!());

                Ok(Some(boolean))
            }
            ASSERT_EQ => {
                let lhs = &args[0];
                let rhs = &args[1];
                compile_expression(lhs, thonk, context)?;
                compile_expression(rhs, thonk, context)?;
                thonk.insert_instruction_with_span(
                    Instruction::TestEqual,
                    span.clone(),
                    location!(),
                );
                thonk.insert_instruction_with_span(
                    Instruction::JumpIfTrue(5),
                    span.clone(),
                    location!(),
                );
                thonk.insert_instruction_with_span(
                    Instruction::Push(new_ref!(
                        Value,
                        format!("assertion failed: {span:?}").into()
                    )),
                    span.clone(),
                    location!(),
                );
                thonk.insert_instruction_with_span(Instruction::Out(1), span.clone(), location!());

                // This is the bad path.
                thonk.insert_instruction(
                    Instruction::Push(new_ref!(
                        Value,
                        context.extruder_context.source_path.clone().into()
                    )),
                    location!(),
                );
                thonk.insert_instruction(
                    Instruction::Push(new_ref!(Value, span.clone().into())),
                    location!(),
                );
                thonk.insert_instruction_with_span(
                    Instruction::HaltAndCatchFire,
                    span,
                    location!(),
                );

                Ok(Some(boolean))
            }
            #[cfg(feature = "async")]
            SPAWN => {
                let inner = &args[0];

                let result = compile_expression(inner, thonk, context);

                thonk.insert_instruction(Instruction::AsyncSpawn(0), location!());

                result
            }
            meth => todo!("handle chacha method: {meth}"),
        },
        ty => {
            // This is where we load the shared library that is the extension.
            // Note that we are doing magic with the word "Plugin".
            if Some(Some(PLUGIN)) == ty.split('<').next().map(|s| s.split(PATH_SEP).last()) {
                match func {
                    NEW => {
                        let plugin = ty.split('<').nth(1).unwrap().strip_suffix('>').unwrap();
                        let plugin = lu_dog.exhume_x_plugin_id_by_name(plugin).unwrap();
                        let plugin = lu_dog.exhume_x_plugin(&plugin).unwrap();
                        let plugin = s_read!(plugin);
                        let path = &plugin.x_path;
                        let plugin_root = path.split(PATH_SEP).next().unwrap();

                        if let Some(path) = path.split(PATH_SEP).nth(1) {
                            thonk.insert_instruction(
                                Instruction::Push(new_ref!(Value, Value::String(path.to_owned()))),
                                location!(),
                            );
                        };

                        thonk.insert_instruction(
                            Instruction::Push(new_ref!(Value, plugin_root.into())),
                            location!(),
                        );
                        thonk.insert_instruction(Instruction::PluginNew(1), location!());

                        // let id = lu_dog.exhume_woog_struct_id_by_name(&plugin.name).unwrap();
                        // let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
                        // let woog_struct = s_read!(woog_struct);

                        // Ok(Some(
                        //     (*s_read!(woog_struct.r1_value_type(&lu_dog)[0])).clone(),
                        // ))

                        Ok(None)
                    }
                    missing_method => {
                        panic!("plugin only supports `new`, found: {missing_method}");
                    }
                }
            } else {
                let func1 = lu_dog.exhume_function_id_by_name(func).unwrap();
                let func1 = lu_dog.exhume_function(&func1).unwrap();
                let body = s_read!(func1).r19_body(&lu_dog)[0].clone();
                let a_sink = s_read!(body).a_sink;

                let func_name = format!("{ty}::{func}");
                let name = new_ref!(String, func_name);
                // These instructions will be patched by the VM.
                thonk.insert_instruction(Instruction::CallDestination(name.clone()), location!());
                thonk.insert_instruction(Instruction::LocalCardinality(name), location!());

                for expr in args {
                    compile_expression(expr, thonk, context)?;
                }

                if a_sink {
                    thonk.insert_instruction(Instruction::AsyncCall(args.len()), location!());
                } else {
                    thonk.insert_instruction(Instruction::Call(args.len()), location!());
                }

                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use test_log::test;

    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm, run_vm_with_args, setup_logging},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn empty_func() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {}";
        let ast = parse_dwarf("empty_func", ore).unwrap();
        let ctx = new_lu_dog(
            "empty_func".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 2);

        run_vm(&program).unwrap();
    }

    #[test]
    fn empty_funcs() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {}
                   fn foo() {}
                   fn bar() {}";
        let ast = parse_dwarf("empty_func", ore).unwrap();
        let ctx = new_lu_dog(
            "empty_func".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        assert_eq!(program.get_thonk_card(), 3);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 2);
        assert_eq!(program.get_thonk("foo").unwrap().instruction_card(), 2);
        assert_eq!(program.get_thonk("bar").unwrap().instruction_card(), 2);

        run_vm(&program).unwrap();
    }

    #[test]
    fn func_call() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {
                       foo();
                   }
                   fn foo() {
                       print(\"Hello, world!\");
                   }";
        let ast = parse_dwarf("func_call", ore).unwrap();
        let ctx = new_lu_dog(
            "func_call".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 5);
        assert_eq!(program.get_thonk("foo").unwrap().instruction_card(), 4);

        run_vm(&program).unwrap();
    }

    #[test]
    fn test_func_args() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       foo(1, 2, 3)
                   }
                   fn foo(x: int, y: int, z: int) -> int {
                       x + y + z
                   }";
        let ast = parse_dwarf("test_func_args", ore).unwrap();
        let ctx = new_lu_dog(
            "test_func_args".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(6));
    }

    #[test]
    fn test_func_with_args_and_locals() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       foo(1, 2, 3)
                   }
                   fn foo(x: int, y: int, z: int) -> int {
                       let a = 1;
                       let b = 2;
                       let c = 3;
                       x + y + z + a + b + c
                   }";
        let ast = parse_dwarf("test_func_args_and_locals", ore).unwrap();
        let ctx = new_lu_dog(
            "test_func_args_and_locals".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().instruction_card(), 18);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(12));
    }

    #[test]
    fn test_argument_ordering() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {
                       foo(1, 2, 3)
                   }
                   fn foo(x: int, y: int, z: int) {
                       chacha::assert_eq(x, 1);
                       chacha::assert_eq(y, 2);
                       chacha::assert_eq(z, 3);
                   }";
        let ast = parse_dwarf("test_argument_ordering", ore).unwrap();
        let ctx = new_lu_dog(
            "test_argument_ordering".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().instruction_card(), 29);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Empty);
    }

    // #[test]
    fn string_format_locals() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
                         let a = 1;
                         let b = 2;
                         let c = 3;
                         \"{a} {b} {c}\".format()
                   }";
        let ast = parse_dwarf("test_or_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "test_or_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 11);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &"test 1 2 3".into());
    }

    // #[test]
    fn string_format_indices() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
                       \"test {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}\".format(
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                       )
                   }";
        let ast = parse_dwarf("test_or_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "test_or_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 11);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &"test 0 1 2 3 4 5 6 7 8 9".into()
        );
    }

    #[test]
    fn test_assert() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() {
                       chacha::assert(true);
                   }";
        let ast = parse_dwarf("test_or_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "test_or_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 11);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Empty);
    }

    #[test]
    fn test_method_call() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                struct Foo {
                    bar: int,
                }
                impl Foo {
                    fn new() -> Foo {
                        Foo { bar: 42 }
                    }

                    fn bar(self) -> int {
                        self.bar
                    }
                }
                fn main() -> int {
                    let foo = Foo::new();
                    foo.bar()
                }";
        let ast = parse_dwarf("test_method_call", ore).unwrap();
        let ctx = new_lu_dog(
            "test_method_call".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 3);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 9);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &42.into());
    }

    #[test]
    fn test_args() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> () {
                       let first = chacha::args()[0];
                       let second = chacha::args()[1];
                       chacha::assert_eq(first, 42);
                       chacha::assert_eq(second, \"Hello World\");
                   }";
        let ast = parse_dwarf("test_args", ore).unwrap();
        let ctx = new_lu_dog(
            "test_args".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 28);

        let args = vec![
            new_ref!(Value, 42.into()),
            new_ref!(Value, "Hello World".into()),
        ];

        assert!(run_vm_with_args(&program, &args).is_ok());
    }

    #[test]
    fn test_lambda() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ore = "
                   fn main() -> int {
                       let _0 = 0;
                       let a = 42;
                       let b = 96;
                       let foo = |x: int, y: int| -> int {
                           x + y + a
                       };
                       foo(1, 2)
                   }";
        let ast = parse_dwarf("test_lambda", ore).unwrap();
        let ctx = new_lu_dog(
            "test_lambda".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);

        assert_eq!(program.get_instruction_card(), 20);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &45.into());
    }

    #[test]
    fn test_lambda_empty() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ore = "
                   fn main() -> () {
                       let _0 = 0;
                       let a = 42;
                       let b = 96;
                       let foo = |x: int, y: int| -> () {
                           x + y + a;
                       };
                       foo(1, 2)
                   }";
        let ast = parse_dwarf("test_lambda", ore).unwrap();
        let ctx = new_lu_dog(
            "test_lambda".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);

        assert_eq!(program.get_instruction_card(), 21);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Empty);
    }

    // #[test]
    fn test_call_chain() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ore = "
                struct Callee {
                    count: int,
                }
                impl Callee {
                    fn new() -> Callee {
                        Callee { count: 0 }
                    }

                    fn call(self) -> Callee {
                        self.count = self.count + 1;
                        self
                    }
                }
                fn main() -> bool {
                    let callee = Callee::new();
                    callee.call().call();
                    chacha::assert(callee.count == 2)
                }";
        let ast = parse_dwarf("test_call_chain", ore).unwrap();
        let ctx = new_lu_dog(
            "test_call_chain".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 3);

        // assert_eq!(program.get_instruction_count(), 39);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &true.into());
    }
}
