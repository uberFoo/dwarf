use snafu::{location, Location};
use uuid::Uuid;

#[cfg(feature = "async")]
use crate::keywords::SPAWN;

use crate::{
    bubba::{
        compiler::{compile_expression, compile_statement, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    keywords::{ARGS, ASSERT, ASSERT_EQ, CHACHA, FORMAT, NEW, PLUGIN},
    lu_dog::{BodyEnum, Call, CallEnum, Expression, ValueType, ValueTypeEnum},
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Span, Value, PATH_SEP, POP_CLR,
};

pub(in crate::bubba::compiler) fn compile(
    call: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_call"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let wrapped_call = lu_dog.exhume_call(call).unwrap();
    let call = s_read!(wrapped_call);
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
pub(in crate::bubba::compiler) fn compile_lambda(
    λ: &SarzakStorePtr,
    outer_thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_lambda"), file!(), line!(), column!());

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
                        } else if thonk.returned {
                            break;
                        } else {
                            thonk.add_instruction(
                                Instruction::Push(new_ref!(Value, Value::Empty)),
                                location!(),
                            );
                            thonk.add_instruction(Instruction::Return, location!());
                            thonk.returned = true;
                            break;
                        }
                    }
                }
            } else {
                thonk.add_instruction(
                    Instruction::Push(new_ref!(Value, Value::Empty)),
                    location!(),
                );
                thonk.add_instruction(Instruction::Return, location!());
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

    let mut frame_size = thonk.inner.frame_size();
    dbg!(frame_size);
    for (var, index) in context.captures.take().unwrap() {
        let Some(symbol) = context.get_symbol(&var) else {
            panic!("capture not found: {var}");
        };
        // frame_size += 1;
        // thonk.increment_frame_size();
        thonk.prefix_instruction(Instruction::CaptureLocal(symbol.number, index), location!());
    }

    context.captures = saved_captures;

    context.get_program().add_thonk(thonk.into());

    // let pointer = Value::FubarPointer {
    //     name: name.clone(),
    //     frame_size,
    //     captures: Vec::new(),
    // };

    // outer_thonk.add_instruction(Instruction::Push(new_ref!(Value, pointer)), location!());
    outer_thonk.add_instruction(
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
fn compile_function_call(
    name: &str,
    call: RefType<Call>,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_function_call"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    match name {
        FORMAT => todo!("handle FORMAT"),
        _ => {
            let call = s_read!(call);
            let result = if let Some(ref expr) = call.expression {
                let expr = lu_dog.exhume_expression(expr).unwrap();
                let span = get_span(&expr, &lu_dog);
                // Evaluate the LHS to get at the underlying value/instance.
                // I'm not sure that this is the correct type. OTOH, what other
                // type would I use?
                compile_expression(&expr, thonk, context, span)
            } else {
                panic!();
            };

            for expr in args {
                let span = get_span(expr, &lu_dog);
                compile_expression(expr, thonk, context, span)?;
            }

            thonk.add_instruction(Instruction::Call(args.len()), location!());

            result
        }
    }
}

fn compile_method_call(
    name: String,
    call: RefType<Call>,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_method_call"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    // First off we need to evaluate the expression associated with this call.
    let result = if let Some(ref expr) = s_read!(call).expression {
        let expr = lu_dog.exhume_expression(expr).unwrap();
        let span = get_span(&expr, &lu_dog);
        // Evaluate the LHS to get at the underlying value/instance.
        let result = compile_expression(&expr, thonk, context, span);
        thonk.add_instruction(
            Instruction::MethodLookup(new_ref!(String, name)),
            location!(),
        );
        result
    } else {
        panic!();
    };

    for expr in args {
        let span = get_span(expr, &lu_dog);
        compile_expression(expr, thonk, context, span)?;
    }

    thonk.add_instruction(Instruction::Call(args.len()), location!());

    result
}

fn compile_static_method_call(
    ty: &str,
    func: &str,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_static_method_call"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let boolean = Ty::new_boolean(&s_read!(context.sarzak_heel()));
    let boolean = (*s_read!(ValueType::new_ty(true, &boolean, &mut s_write!(lu_dog)))).clone();
    let string = Ty::new_z_string(&s_read!(context.sarzak_heel()));
    let string = ValueType::new_ty(true, &string, &mut s_write!(lu_dog));
    let mut string_array = ValueType::new_empty(true, &mut s_write!(lu_dog));

    let lu_dog = s_read!(lu_dog);

    for vt in lu_dog.iter_value_type() {
        if let ValueTypeEnum::List(id) = s_read!(vt).subtype {
            let list = lu_dog.exhume_list(&id).unwrap();
            let list_ty = s_read!(list).r36_value_type(&lu_dog)[0].clone();
            if *s_read!(string) == *s_read!(list_ty) {
                string_array = vt.clone();
            }
        }
    }

    let string = (*s_read!(string)).clone();
    let string_array = (*s_read!(string_array)).clone();

    match ty {
        CHACHA => match func {
            ARGS => {
                thonk.add_instruction_with_span(Instruction::PushArgs, span.clone(), location!());
                Ok(Some(string_array))
            }
            ASSERT => {
                let expr = &args[0];
                let expr_span = get_span(expr, &lu_dog);
                compile_expression(expr, thonk, context, expr_span)?;
                thonk.add_instruction_with_span(
                    Instruction::Push(new_ref!(Value, true.into())),
                    span.clone(),
                    location!(),
                );
                thonk.add_instruction_with_span(Instruction::TestEq, span.clone(), location!());
                thonk.add_instruction_with_span(
                    Instruction::JumpIfTrue(5),
                    span.clone(),
                    location!(),
                );
                thonk.add_instruction_with_span(
                    Instruction::Push(new_ref!(
                        Value,
                        format!("assertion failed: {span:?}").into()
                    )),
                    span.clone(),
                    location!(),
                );
                thonk.add_instruction_with_span(Instruction::Out(1), span.clone(), location!());

                // Bail on false
                thonk.add_instruction(
                    Instruction::Push(new_ref!(
                        Value,
                        context.extruder_context.source.clone().into()
                    )),
                    location!(),
                );
                thonk.add_instruction(
                    Instruction::Push(new_ref!(Value, span.clone().into())),
                    location!(),
                );
                thonk.add_instruction(Instruction::HaltAndCatchFire, location!());

                Ok(Some(boolean))
            }
            ASSERT_EQ => {
                let lhs = &args[0];
                let rhs = &args[1];
                let lhs_span = get_span(lhs, &lu_dog);
                compile_expression(lhs, thonk, context, lhs_span)?;
                let rhs_span = get_span(rhs, &lu_dog);
                compile_expression(rhs, thonk, context, rhs_span)?;
                thonk.add_instruction_with_span(Instruction::TestEq, span.clone(), location!());
                thonk.add_instruction_with_span(
                    Instruction::JumpIfTrue(5),
                    span.clone(),
                    location!(),
                );
                thonk.add_instruction_with_span(
                    Instruction::Push(new_ref!(
                        Value,
                        format!("assertion failed: {span:?}").into()
                    )),
                    span.clone(),
                    location!(),
                );
                thonk.add_instruction_with_span(Instruction::Out(1), span.clone(), location!());

                // This is the bad path.
                thonk.add_instruction(
                    Instruction::Push(new_ref!(
                        Value,
                        context.extruder_context.source.clone().into()
                    )),
                    location!(),
                );
                thonk.add_instruction(
                    Instruction::Push(new_ref!(Value, span.clone().into())),
                    location!(),
                );
                thonk.add_instruction_with_span(Instruction::HaltAndCatchFire, span, location!());

                Ok(Some(boolean))
            }
            #[cfg(feature = "async")]
            SPAWN => {
                let inner = &args[0];
                let inner_span = get_span(inner, &lu_dog);
                let result = compile_expression(inner, thonk, context, inner_span);

                thonk.add_instruction(Instruction::Call(0), location!());

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
                            thonk.add_instruction(
                                Instruction::Push(new_ref!(Value, Value::String(path.to_owned()))),
                                location!(),
                            );
                        };

                        thonk.add_instruction(
                            Instruction::Push(new_ref!(Value, plugin_root.into())),
                            location!(),
                        );
                        thonk.add_instruction(Instruction::PluginNew(1), location!());

                        let id = lu_dog.exhume_woog_struct_id_by_name(&plugin.name).unwrap();
                        let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
                        let woog_struct = s_read!(woog_struct);

                        Ok(Some(
                            (*s_read!(woog_struct.r1_value_type(&lu_dog)[0])).clone(),
                        ))
                    }
                    missing_method => {
                        panic!("plugin only supports `new`, found: {missing_method}");
                    }
                }
            } else {
                let func_name = format!("{ty}::{func}");
                let name = new_ref!(String, func_name);
                // These instructions will be patched by the VM.
                thonk.add_instruction(Instruction::CallDestination(name.clone()), location!());
                thonk.add_instruction(Instruction::LocalCardinality(name), location!());

                for expr in args {
                    let span = get_span(expr, &lu_dog);
                    compile_expression(expr, thonk, context, span)?;
                }

                thonk.add_instruction(Instruction::Call(args.len()), location!());

                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm, run_vm_with_args},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn empty_func() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 2);

        run_vm(&program).unwrap();
    }

    #[test]
    fn empty_funcs() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 2);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 2);
        assert_eq!(program.get_thonk("bar").unwrap().get_instruction_card(), 2);

        run_vm(&program).unwrap();
    }

    #[test]
    fn func_call() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 5);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 4);

        run_vm(&program).unwrap();
    }

    #[test]
    fn test_func_args() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       foo(1, 2, 3)
                   }
                   fn foo(x: int, y: int, z: int) -> int {
                       x + y + z
                   }";
        let ast = parse_dwarf("test_func_args", ore).unwrap();
        let mut ctx = new_lu_dog(
            "test_func_args".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&mut ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(6));
    }

    #[test]
    fn test_func_with_args_and_locals() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 18);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(12));
    }

    #[test]
    fn test_argument_ordering() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 29);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Empty);
    }

    // #[test]
    fn string_format_locals() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            11
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &"test 1 2 3".into());
    }

    // #[test]
    fn string_format_indices() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            11
        );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &"test 0 1 2 3 4 5 6 7 8 9".into()
        );
    }

    #[test]
    fn test_assert() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            11
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Empty);
    }

    #[test]
    fn test_method_call() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 9);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &42.into());
    }

    #[test]
    fn test_args() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            28
        );

        let args = vec![
            new_ref!(Value, 42.into()),
            new_ref!(Value, "Hello World".into()),
        ];

        assert!(run_vm_with_args(&program, &args).is_ok());
    }

    #[test]
    fn test_lambda() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(program.get_instruction_count(), 20);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &45.into());
    }
}
