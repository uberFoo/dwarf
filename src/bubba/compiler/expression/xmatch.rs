use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    bubba::{
        compiler::{compile_expression, expression::literal, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::{DataStructureEnum, ExpressionEnum, ValueType},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_match"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let match_expr = lu_dog.exhume_x_match(expr).unwrap();
    let match_expr = s_read!(match_expr);

    let patterns = match_expr.r87_pattern(&lu_dog);
    let scrutinee = match_expr.r91_expression(&lu_dog)[0].clone();

    let label = format!("{}", Uuid::new_v4());
    for pattern in patterns {
        // Compiling the scrutinee
        compile_expression(&scrutinee, thonk, context)?;

        tracing::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

        // We push this up here because of the pattern matching needs it's own context.
        context.push_scope();

        let pattern = s_read!(pattern);
        let match_expr = pattern.r87_expression(&lu_dog)[0].clone();
        let pattern_expr = pattern.r92_expression(&lu_dog)[0].clone();

        // Compile the match expression.
        match &s_read!(match_expr).subtype {
            ExpressionEnum::Literal(ref literal) => {
                tracing::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

                literal::compile(literal, thonk, context, span.clone())?;
            }
            ExpressionEnum::StructExpression(ref id) => {
                let struct_expr = lu_dog.exhume_struct_expression(id).unwrap();
                let struct_expr = s_read!(struct_expr);

                let ds = struct_expr.r39_data_structure(&lu_dog)[0].clone();
                let DataStructureEnum::Enumeration(woog_enum) = &s_read!(ds).subtype else {
                    unreachable!()
                };
                let woog_enum = lu_dog.exhume_enumeration(woog_enum).unwrap();
                let woog_enum = s_read!(woog_enum);

                let field_exprs = struct_expr.r26_field_expression(&lu_dog);

                for f in &field_exprs {
                    let expr = s_read!(f).r15_expression(&lu_dog)[0].clone();
                    let expr = s_read!(expr);
                    match &expr.subtype {
                        ExpressionEnum::FieldExpression(ref id) => {
                            let expr = lu_dog.exhume_field_expression(id).unwrap();
                            let expr = s_read!(expr).r38_expression(&lu_dog)[0].clone();

                            let expr = s_read!(expr);
                            // 🚧 I'm already in the middle of one of these.
                            match &expr.subtype {
                                ExpressionEnum::Literal(ref literal) => {
                                    tracing::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

                                    literal::compile(literal, thonk, context, span.clone())?;
                                }
                                ExpressionEnum::VariableExpression(ref id) => {
                                    let var = lu_dog.exhume_variable_expression(id).unwrap();
                                    let var = s_read!(var);
                                    let expr = var.r15_expression(&lu_dog)[0].clone();
                                    let value = s_read!(expr).r11_x_value(&lu_dog)[0].clone();
                                    let ty = s_read!(value).r24_value_type(&lu_dog)[0].clone();

                                    let idx = match context
                                        .insert_symbol(var.name.clone(), (*s_read!(ty)).clone())
                                    {
                                        (true, index) => {
                                            thonk.increment_frame_size();
                                            index
                                        }
                                        (false, index) => index,
                                    };
                                    // thonk.add_instruction(
                                    // Instruction::DeconstructStructExpression,
                                    // location!(),
                                    // );
                                    thonk.insert_instruction(Instruction::Dup, location!());
                                    thonk.insert_instruction(
                                        Instruction::ExtractEnumValue,
                                        location!(),
                                    );
                                    thonk.insert_instruction(Instruction::Dup, location!());
                                    thonk.insert_instruction(
                                        Instruction::StoreLocal(idx),
                                        location!(),
                                    );
                                }
                                todo => {
                                    todo!("Match expression, field expression, type: {todo:?}");
                                }
                            }
                        }
                        todo => {
                            todo!("Match expression type: {todo:?}");
                        }
                    }
                }

                let expr = struct_expr.r15_expression(&lu_dog)[0].clone();
                let value = s_read!(expr).r11_x_value(&lu_dog)[0].clone();
                let ty = s_read!(value).r24_value_type(&lu_dog)[0].clone();
                let ty = new_ref!(Value, Value::ValueType((*s_read!(ty)).to_owned()));
                thonk.insert_instruction(Instruction::Push(ty), location!());

                let x_path = &lu_dog.exhume_x_path(&struct_expr.x_path).unwrap();
                // We know that there is always a pe. It's only in an option so that
                // we can construct everything.
                let mut pe = s_read!(x_path).r97_path_element(&lu_dog)[0].clone();
                let mut path = vec![s_read!(pe).name.to_owned()];

                while s_read!(pe).next.is_some() {
                    let id = {
                        let id = &s_read!(pe).next;
                        #[allow(clippy::clone_on_copy)]
                        id.as_ref().unwrap().clone()
                    };
                    // thonk.add_instruction(
                    //     Instruction::Push(new_ref!(Value, s_read!(pe).name.clone().into())),
                    //     location!(),
                    // );
                    pe = lu_dog.exhume_path_element(&id).unwrap();
                    path.push(s_read!(pe).name.to_owned());
                }

                // dbg!(")", &pe);
                let variant = path.pop().unwrap();
                let path = path.join("::");
                let path = format!("{}{path}", woog_enum.x_path);

                thonk.insert_instruction(
                    Instruction::Push(new_ref!(Value, path.into())),
                    location!(),
                );

                thonk.insert_instruction(
                    Instruction::Push(new_ref!(Value, variant.into())),
                    location!(),
                );

                thonk.insert_instruction(Instruction::NewTupleEnum(field_exprs.len()), location!());
            }
            ExpressionEnum::VariableExpression(ref id) => {
                let var = lu_dog.exhume_variable_expression(id).unwrap();
                let var = s_read!(var);
                let expr = var.r15_expression(&lu_dog)[0].clone();
                let value = s_read!(expr).r11_x_value(&lu_dog)[0].clone();
                let ty = s_read!(value).r24_value_type(&lu_dog)[0].clone();

                let idx = match context.insert_symbol(var.name.clone(), (*s_read!(ty)).clone()) {
                    (true, idx) => {
                        thonk.increment_frame_size();
                        idx
                    }
                    (false, idx) => idx,
                };

                thonk.insert_instruction(Instruction::Dup, location!());
                thonk.insert_instruction(Instruction::Dup, location!());
                thonk.insert_instruction(Instruction::StoreLocal(idx), location!());
            }
            todo => todo!("Match expression type: {todo:?}"),
        }

        thonk.insert_instruction(Instruction::TestEqual, location!());

        // Compile the match block.
        let mut match_thonk = CThonk::new("match".to_owned());

        tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_pattern_expr"), file!(), line!(), column!());
        compile_expression(&pattern_expr, &mut match_thonk, context)?;

        let fp = match_thonk.get_frame_size();
        for _ in 0..fp {
            thonk.increment_frame_size();
        }
        let match_len = match_thonk.get_instruction_card() as isize;

        // Jump over the matching block if we don't match.
        thonk.insert_instruction(Instruction::JumpIfFalse(match_len + 1), location!());

        // Insert the compiled matching block
        thonk.append(match_thonk);

        // Escape from New York
        thonk.insert_instruction(
            Instruction::Goto(new_ref!(String, label.clone())),
            location!(),
        );

        context.pop_scope();
    }

    // This should not really happen because if this were done right the compiler
    // would notice that there are cases not caught and  fall through.
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

    thonk.insert_instruction(Instruction::Label(new_ref!(String, label)), location!());

    Ok(None)
}

#[cfg(test)]
mod test {
    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm, setup_logging},
            *,
        },
        chacha::value::{EnumVariant, TupleEnum},
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn match_literal_expression() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 1 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           _ => 4,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 31);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }

    #[test]
    fn match_literal_catchall() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 6 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           a => {
                                print(a);
                                4
                           }
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 33);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &4.into());
    }

    #[test]
    fn match_literal_exp_middle() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 3 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           _ => 4,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 31);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &3.into());
    }

    #[test]
    fn match_string_literal_expression() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
                       match \"foo\" {
                           \"foo\" => \"foo\",
                           \"bar\" => \"bar\",
                           \"baz\" => \"baz\",
                           _ => \"qux\",
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 31);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("foo".to_owned())
        );
    }

    #[test]
    fn match_enum() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar,
                       Baz,
                       Qux,
                   }
                   fn main() -> Foo {
                       match Foo::Bar {
                           Foo::Bar => Foo::Bar,
                           Foo::Baz => Foo::Baz,
                           Foo::Qux => Foo::Qux,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let ty = {
            let mut lu_dog = s_write!(ctx.lu_dog);

            let id = lu_dog.exhume_enumeration_id_by_name("::Foo").unwrap();
            let woog_enum = lu_dog.exhume_enumeration(&id).unwrap();
            ValueType::new_enumeration(true, &woog_enum, &mut lu_dog)
        };

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 32);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::Enumeration(EnumVariant::Unit(ty, "::Foo".to_owned(), "Bar".to_owned()))
        );
    }

    #[test]
    fn match_tuple_enum() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> Foo {
                       match Foo::Bar(40 + 2) {
                           Foo::Baz(1) => Foo::Baz(1),
                           Foo::Bar(42) => Foo::Bar(42),
                           Foo::Qux(1) => Foo::Qux(1),
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let lu_dog = &ctx.lu_dog;

        let id = s_read!(lu_dog)
            .exhume_enumeration_id_by_name("::Foo")
            .unwrap();
        let woog_enum = s_read!(lu_dog).exhume_enumeration(&id).unwrap();
        let ty = ValueType::new_enumeration(true, &woog_enum, &mut s_write!(lu_dog));
        let user_enum = TupleEnum::new("Bar", new_ref!(Value, Value::Integer(42)));
        let user_enum = new_ref!(TupleEnum, user_enum);

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 65);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::Enumeration(EnumVariant::Tuple((ty, "Foo".to_owned()), user_enum))
        );
    }

    #[test]
    fn match_pattern_variable() {
        setup_logging();
        let sarzak_store = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> int {
                       let x = Foo::Baz(42);
                       match x {
                           Foo::Bar(u) => u,
                           Foo::Baz(v) => v,
                           Foo::Qux(w) => w,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak_store,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 50);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(42));
    }

    #[test]
    fn something_interesting_in_match() {
        setup_logging();
        let sarzak_store = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> int {
                       let x = Foo::Baz(40);
                       match x {
                           Foo::Bar(u) => u + 2,
                           Foo::Baz(v) => v + 2,
                           Foo::Qux(w) => w + 2,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak_store,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 56);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(42));
    }
}

#[cfg(test)]
mod test {
    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm},
            *,
        },
        chacha::value::{EnumVariant, TupleEnum},
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn match_literal_expression() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 1 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           _ => 4,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
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
            30
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }

    #[test]
    fn match_literal_catchall() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 6 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           a => {
                                print(a);
                                4
                           }
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
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
            33
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &4.into());
    }

    #[test]
    fn match_literal_exp_middle() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 3 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           _ => 4,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
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
            30
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &3.into());
    }

    #[test]
    fn match_string_literal_expression() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
                       match \"foo\" {
                           \"foo\" => \"foo\",
                           \"bar\" => \"bar\",
                           \"baz\" => \"baz\",
                           _ => \"qux\",
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
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
            30
        );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("foo".to_owned())
        );
    }

    #[test]
    fn match_enum() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar,
                       Baz,
                       Qux,
                   }
                   fn main() -> Foo {
                       match Foo::Bar {
                           Foo::Bar => Foo::Bar,
                           Foo::Baz => Foo::Baz,
                           Foo::Qux => Foo::Qux,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let ty = {
            let mut lu_dog = s_write!(ctx.lu_dog);

            let id = lu_dog.exhume_enumeration_id_by_name("::Foo").unwrap();
            let woog_enum = lu_dog.exhume_enumeration(&id).unwrap();
            ValueType::new_enumeration(true, &woog_enum, &mut lu_dog)
        };

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            32
        );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::Enumeration(EnumVariant::Unit(ty, "::Foo".to_owned(), "Bar".to_owned()))
        );
    }

    #[test]
    fn match_tuple_enum() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> Foo {
                       match Foo::Bar(40 + 2) {
                           Foo::Baz(1) => Foo::Baz(1),
                           Foo::Bar(42) => Foo::Bar(42),
                           Foo::Qux(1) => Foo::Qux(1),
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let lu_dog = &ctx.lu_dog;

        let id = s_read!(lu_dog)
            .exhume_enumeration_id_by_name("::Foo")
            .unwrap();
        let woog_enum = s_read!(lu_dog).exhume_enumeration(&id).unwrap();
        let ty = ValueType::new_enumeration(true, &woog_enum, &mut s_write!(lu_dog));
        let user_enum = TupleEnum::new("Bar", new_ref!(Value, Value::Integer(42)));
        let user_enum = new_ref!(TupleEnum, user_enum);

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            53
        );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::Enumeration(EnumVariant::Tuple((ty, "Foo".to_owned()), user_enum))
        );
    }

    #[test]
    fn match_pattern_variable() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak_store = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> int {
                       let x = Foo::Baz(42);
                       match x {
                           Foo::Bar(u) => u,
                           Foo::Baz(v) => v,
                           Foo::Qux(w) => w,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak_store,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            50
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(42));
    }

    #[test]
    fn something_interesting_in_match() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak_store = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> int {
                       let x = Foo::Baz(40);
                       match x {
                           Foo::Bar(u) => u + 2,
                           Foo::Baz(v) => v + 2,
                           Foo::Qux(w) => w + 2,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak_store,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            56
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(42));
    }
}
