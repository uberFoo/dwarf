use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    keywords::{ARGS, ASSERT, ASSERT_EQ, CHACHA, FORMAT},
    lu_dog::{Call, CallEnum, Expression},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    call: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

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

            let next_id = { s_read!(next).next };
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
            compile_function_call(
                &s_read!(call).name,
                wrapped_call.clone(),
                &arg_exprs,
                thonk,
                context,
            )?;
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
            )?;
        }
        CallEnum::StaticMethodCall(ref meth) => {
            let meth = lu_dog.exhume_static_method_call(meth).unwrap();
            let meth = s_read!(meth);
            compile_static_method_call(&meth.ty, &meth.func, &arg_exprs, thonk, context, span)?;
        }
        ref call => todo!("handle the other calls: {call:?}"),
    };

    Ok(())
}

fn compile_function_call(
    name: &str,
    call: RefType<Call>,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    match name {
        FORMAT => {}
        _ => {
            if let Some(ref expr) = s_read!(call).expression {
                let expr = lu_dog.exhume_expression(expr).unwrap();
                let span = get_span(&expr, &lu_dog);
                // Evaluate the LHS to get at the underlying value/instance.
                compile_expression(&expr, thonk, context, span)?;
            };

            for expr in args {
                let span = get_span(expr, &lu_dog);
                compile_expression(expr, thonk, context, span)?;
            }

            thonk.add_instruction(Instruction::Call(args.len()), location!());
        }
    }

    Ok(())
}

fn compile_method_call(
    name: String,
    call: RefType<Call>,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    // First off we need to evaluate the expression associated with this call.
    if let Some(ref expr) = s_read!(call).expression {
        let expr = lu_dog.exhume_expression(expr).unwrap();
        let span = get_span(&expr, &lu_dog);
        // Evaluate the LHS to get at the underlying value/instance.
        // Note the magic bit.
        context.method_name = Some(name.clone());
        compile_expression(&expr, thonk, context, span)?;
        context.method_name = None;
    };

    for expr in args {
        let span = get_span(expr, &lu_dog);
        compile_expression(expr, thonk, context, span)?;
    }

    thonk.add_instruction(Instruction::Call(args.len()), location!());

    Ok(())
}

fn compile_static_method_call(
    ty: &str,
    func: &str,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    match ty {
        CHACHA => match func {
            ARGS => {
                thonk.add_instruction_with_span(Instruction::PushArgs, span.clone(), location!());
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
            }
            meth => todo!("handle chacha method: {meth}"),
        },
        ty => {
            let func_name = format!("{ty}::{func}");
            let name = new_ref!(Value, Value::String(func_name));
            // We are here because we need to look up a function.
            thonk.add_instruction(Instruction::CallDestination(name.clone()), location!());

            // This instruction will be patched by the VM with the number of locals in the
            // function.
            thonk.add_instruction(Instruction::LocalCardinality(name), location!());

            for expr in args {
                let span = get_span(expr, &lu_dog);
                compile_expression(expr, thonk, context, span)?;
            }

            thonk.add_instruction(Instruction::Call(args.len()), location!());
        }
    }

    Ok(())
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
}
