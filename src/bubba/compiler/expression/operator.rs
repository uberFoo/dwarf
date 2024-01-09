use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::{
        BinaryEnum, BooleanOperatorEnum, ComparisonEnum, ExpressionEnum, FieldAccessTargetEnum,
        OperatorEnum, UnaryEnum,
    },
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    op_type: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);
    let operator = lu_dog.exhume_operator(op_type).unwrap();
    let operator = s_read!(operator);
    let lhs = lu_dog.exhume_expression(&operator.lhs).unwrap();
    let lhs_span = get_span(&lhs, &lu_dog);

    match operator.subtype {
        OperatorEnum::Binary(ref op_type) => {
            let binary = lu_dog.exhume_binary(op_type).unwrap();
            let binary = s_read!(binary);
            let rhs = lu_dog.exhume_expression(&operator.rhs.unwrap()).unwrap();
            let rhs_span = get_span(&rhs, &lu_dog);

            match binary.subtype {
                BinaryEnum::Addition(_) => {
                    compile_expression(&lhs, thonk, context, lhs_span)?;
                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    thonk.add_instruction_with_span(Instruction::Add, span, location!());
                }
                BinaryEnum::Assignment(_) => {
                    compile_expression(&rhs, thonk, context, rhs_span)?;

                    match &s_read!(lhs).subtype {
                        ExpressionEnum::FieldAccess(ref field) => {
                            let field = lu_dog.exhume_field_access(field).unwrap();
                            let field = s_read!(field);

                            let expr = lu_dog.exhume_expression(&field.expression).unwrap();
                            compile_expression(&expr, thonk, context, get_span(&expr, &lu_dog))?;

                            let fat = &field.r65_field_access_target(&lu_dog)[0];
                            let field_name = match s_read!(fat).subtype {
                                FieldAccessTargetEnum::EnumField(ref field) => {
                                    let field = lu_dog.exhume_enum_field(field).unwrap();
                                    let field = s_read!(field);
                                    field.name.to_owned()
                                }
                                FieldAccessTargetEnum::Field(ref field) => {
                                    let field = lu_dog.exhume_field(field).unwrap();
                                    let field = s_read!(field);
                                    field.name.to_owned()
                                }
                                FieldAccessTargetEnum::Function(ref func) => {
                                    let func = lu_dog.exhume_function(func).unwrap();
                                    let func = s_read!(func);
                                    func.name.to_owned()
                                }
                            };

                            thonk.add_instruction_with_span(
                                Instruction::Push(new_ref!(Value, Value::String(field_name))),
                                span.clone(),
                                location!(),
                            );

                            thonk.add_instruction_with_span(
                                Instruction::FieldWrite,
                                span,
                                location!(),
                            );
                        }
                        ExpressionEnum::VariableExpression(ref expr) => {
                            let expr = lu_dog.exhume_variable_expression(expr).unwrap();
                            let expr = s_read!(expr);
                            let offset = context
                                .get_symbol(&expr.name)
                                .unwrap_or_else(|| panic!("symbol lookup failed for {}", expr.name))
                                .number;

                            thonk.add_instruction_with_span(
                                Instruction::StoreLocal(offset),
                                span,
                                location!(),
                            );
                        }
                        _ => {
                            panic!("In assignment and lhs is not a variable: {lhs:?}")
                        }
                    }
                }
                BinaryEnum::BooleanOperator(ref op) => {
                    let boolean_operator = lu_dog.exhume_boolean_operator(op).unwrap();
                    let boolean_operator = s_read!(boolean_operator);
                    compile_expression(&lhs, thonk, context, lhs_span)?;
                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    match &boolean_operator.subtype {
                        BooleanOperatorEnum::And(_) => {
                            thonk.add_instruction_with_span(Instruction::And, span, location!());
                        }
                        BooleanOperatorEnum::Or(_) => {
                            thonk.add_instruction_with_span(Instruction::Or, span, location!());
                        }
                    }
                }
                BinaryEnum::Division(_) => {
                    compile_expression(&lhs, thonk, context, lhs_span)?;
                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    thonk.add_instruction_with_span(Instruction::Divide, span, location!());
                }
                BinaryEnum::Subtraction(_) => {
                    compile_expression(&lhs, thonk, context, lhs_span)?;
                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    thonk.add_instruction_with_span(Instruction::Subtract, span, location!());
                }
                BinaryEnum::Multiplication(_) => {
                    compile_expression(&lhs, thonk, context, lhs_span)?;
                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    thonk.add_instruction_with_span(Instruction::Multiply, span, location!());
                }
            }
        }
        OperatorEnum::Comparison(ref op_type) => {
            let op_type = lu_dog.exhume_comparison(op_type).unwrap();
            let op_type = s_read!(op_type);

            let rhs = lu_dog.exhume_expression(&operator.rhs.unwrap()).unwrap();
            let rhs_span = get_span(&rhs, &lu_dog);

            match &op_type.subtype {
                ComparisonEnum::Equal(_) => {
                    compile_expression(&lhs, thonk, context, lhs_span)?;
                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    thonk.add_instruction_with_span(Instruction::TestEq, span, location!());
                }
                ComparisonEnum::LessThanOrEqual(_) => {
                    compile_expression(&lhs, thonk, context, lhs_span)?;
                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    thonk.add_instruction_with_span(
                        Instruction::TestLessThanOrEqual,
                        span,
                        location!(),
                    );
                }
                _ => todo!("comparison"),
            }
        }
        OperatorEnum::Unary(ref id) => {
            let unary = lu_dog.exhume_unary(id).unwrap();
            let unary = s_read!(unary);
            compile_expression(&lhs, thonk, context, lhs_span)?;
            match &unary.subtype {
                UnaryEnum::Not(_) => {
                    thonk.add_instruction_with_span(Instruction::Not, span, location!());
                }
                _ => todo!("unary"),
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn test_add_strings() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> string {
                       \"Hello, \" + \"world!\"
                   }";
        let ast = parse_dwarf("test_add_strings", ore).unwrap();
        let ctx = new_lu_dog(
            "test_add_strings".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("Hello, world!".to_owned())
        );
    }

    #[test]
    fn test_subtraction() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       5 - 2
                   }";
        let ast = parse_dwarf("test_subtraction", ore).unwrap();
        let ctx = new_lu_dog(
            "test_subtraction".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(3));
    }

    #[test]
    fn test_multiplication() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       5 * 2
                   }";
        let ast = parse_dwarf("test_multiplication", ore).unwrap();
        let ctx = new_lu_dog(
            "test_multiplication".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(10));
    }

    #[test]
    fn test_division() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       5 / 2
                   }";
        let ast = parse_dwarf("test_division", ore).unwrap();
        let ctx = new_lu_dog(
            "test_division".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(2));
    }

    #[test]
    fn test_assignment() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 5;
                       x = 10;
                       x
                   }";
        let ast = parse_dwarf("test_assignment", ore).unwrap();
        let ctx = new_lu_dog(
            "test_assignment".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(10));
    }

    #[test]
    fn test_and_expression() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> bool {
                       true && true
                   }";
        let ast = parse_dwarf("test_and_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "test_and_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &true.into());
    }

    #[test]
    fn test_or_expression() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> bool {
                       true || false
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

        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &true.into());
    }

    #[test]
    fn test_binary_not() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> bool {
                       !false
                   }";
        let ast = parse_dwarf("test_binary_not", ore).unwrap();
        let ctx = new_lu_dog(
            "test_binary_not".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 3);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &true.into());
    }

    #[test]
    fn test_assign_to_struct_field() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ore = "struct Foo {
                       bar: int,
                   }
                   fn main() -> int {
                       let foo = Foo { bar: 42 };
                       foo.bar = 43;
                       foo.bar
                   }";
        let ast = parse_dwarf("test_assign_to_struct_field", ore).unwrap();
        let ctx = new_lu_dog(
            "test_assign_to_struct_field".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        let result = run_vm(&program);
        assert!(result.is_ok());

        assert_eq!(&*s_read!(result.unwrap()), &43.into());
    }
}
