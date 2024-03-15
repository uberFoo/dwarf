use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result, BOOL, EMPTY, INT},
        instr::Instruction,
        value::Value,
    },
    lu_dog::{
        BinaryEnum, BooleanOperatorEnum, ComparisonEnum, ExpressionEnum, FieldAccessTargetEnum,
        OperatorEnum, UnaryEnum, ValueType,
    },
    s_read, SarzakStorePtr, Span, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(thonk, context)))]
pub(in crate::bubba::compiler) fn compile(
    op_type: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_operator"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);
    let operator = lu_dog.exhume_operator(op_type).unwrap();
    let operator = s_read!(operator);
    let lhs = lu_dog.exhume_expression(&operator.lhs).unwrap();

    let ty = match operator.subtype {
        OperatorEnum::Binary(ref op_type) => {
            let binary = lu_dog.exhume_binary(op_type).unwrap();
            let binary = s_read!(binary);
            let rhs = lu_dog.exhume_expression(&operator.rhs.unwrap()).unwrap();

            match binary.subtype {
                BinaryEnum::Addition(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(Instruction::Add, span, location!());

                    context.get_type(INT).unwrap().clone()
                }
                BinaryEnum::Assignment(_) => {
                    compile_expression(&rhs, thonk, context)?;

                    match &s_read!(lhs).subtype {
                        ExpressionEnum::FieldAccess(ref field) => {
                            let field = lu_dog.exhume_field_access(field).unwrap();
                            let field = s_read!(field);

                            let expr = lu_dog.exhume_expression(&field.expression).unwrap();
                            compile_expression(&expr, thonk, context)?;

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

                            thonk.insert_instruction_with_span(
                                Instruction::Push(Value::String(field_name)),
                                span.clone(),
                                location!(),
                            );

                            thonk.insert_instruction_with_span(
                                Instruction::FieldWrite,
                                span,
                                location!(),
                            );

                            context.get_type(EMPTY).unwrap().clone()
                        }
                        ExpressionEnum::VariableExpression(ref expr) => {
                            let expr = lu_dog.exhume_variable_expression(expr).unwrap();
                            let expr = s_read!(expr);
                            let offset = context
                                .get_symbol(&expr.name)
                                .unwrap_or_else(|| panic!("symbol lookup failed for {}", expr.name))
                                .number;

                            thonk.insert_instruction_with_span(
                                Instruction::StoreLocal(offset),
                                span,
                                location!(),
                            );

                            context.get_type(EMPTY).unwrap().clone()
                        }
                        _ => {
                            panic!("In assignment and lhs is not a variable: {lhs:?}")
                        }
                    }
                }
                BinaryEnum::BooleanOperator(ref op) => {
                    let boolean_operator = lu_dog.exhume_boolean_operator(op).unwrap();
                    let boolean_operator = s_read!(boolean_operator);
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    match &boolean_operator.subtype {
                        BooleanOperatorEnum::And(_) => {
                            thonk.insert_instruction_with_span(Instruction::And, span, location!());
                        }
                        BooleanOperatorEnum::Or(_) => {
                            thonk.insert_instruction_with_span(Instruction::Or, span, location!());
                        }
                    }

                    context.get_type(BOOL).unwrap().clone()
                }
                BinaryEnum::Division(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(Instruction::Divide, span, location!());

                    context.get_type(INT).unwrap().clone()
                }
                BinaryEnum::Subtraction(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(Instruction::Subtract, span, location!());

                    context.get_type(INT).unwrap().clone()
                }
                BinaryEnum::Multiplication(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(Instruction::Multiply, span, location!());

                    context.get_type(INT).unwrap().clone()
                }
            }
        }
        OperatorEnum::Comparison(ref op_type) => {
            let op_type = lu_dog.exhume_comparison(op_type).unwrap();
            let op_type = s_read!(op_type);

            let rhs = lu_dog.exhume_expression(&operator.rhs.unwrap()).unwrap();

            match &op_type.subtype {
                ComparisonEnum::Equal(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(Instruction::TestEqual, span, location!());

                    context.get_type(BOOL).unwrap().clone()
                }
                ComparisonEnum::GreaterThan(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(
                        Instruction::TestGreaterThan,
                        span,
                        location!(),
                    );

                    context.get_type(BOOL).unwrap().clone()
                }
                ComparisonEnum::GreaterThanOrEqual(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(
                        Instruction::TestGreaterThanOrEqual,
                        span,
                        location!(),
                    );

                    context.get_type(BOOL).unwrap().clone()
                }
                ComparisonEnum::LessThan(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(
                        Instruction::TestLessThan,
                        span,
                        location!(),
                    );

                    context.get_type(BOOL).unwrap().clone()
                }
                ComparisonEnum::LessThanOrEqual(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(
                        Instruction::TestLessThanOrEqual,
                        span,
                        location!(),
                    );

                    context.get_type(BOOL).unwrap().clone()
                }
                ComparisonEnum::NotEqual(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.insert_instruction_with_span(
                        Instruction::TestNotEqual,
                        span,
                        location!(),
                    );

                    context.get_type(BOOL).unwrap().clone()
                }
            }
        }
        OperatorEnum::Unary(ref id) => {
            let unary = lu_dog.exhume_unary(id).unwrap();
            let unary = s_read!(unary);
            compile_expression(&lhs, thonk, context)?;
            match &unary.subtype {
                UnaryEnum::Negation(_) => {
                    thonk.insert_instruction_with_span(
                        Instruction::Push(Value::Integer(-1)),
                        span.clone(),
                        location!(),
                    );
                    thonk.insert_instruction_with_span(Instruction::Multiply, span, location!());

                    // 🚧 WHat if it's a float?
                    context.get_type(INT).unwrap().clone()
                }
                UnaryEnum::Not(_) => {
                    thonk.insert_instruction_with_span(Instruction::Not, span, location!());

                    context.get_type(BOOL).unwrap().clone()
                }
            }
        }
    };

    Ok(Some(ty))
}

#[cfg(test)]
mod test {
    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm, setup_logging},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn add_strings() {
        setup_logging();
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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 4);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("Hello, world!".to_owned())
        );
    }

    #[test]
    fn subtraction() {
        setup_logging();
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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(3));
    }

    #[test]
    fn multiplication() {
        setup_logging();
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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(10));
    }

    #[test]
    fn division() {
        setup_logging();
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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(2));
    }

    #[test]
    fn assignment() {
        setup_logging();
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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(10));
    }

    #[test]
    fn and_expression() {
        setup_logging();
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

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &true.into());
    }

    #[test]
    fn or_expression() {
        setup_logging();
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

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &true.into());
    }

    #[test]
    fn binary_not() {
        setup_logging();
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

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 3);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &true.into());
    }

    #[test]
    fn assign_to_struct_field() {
        setup_logging();
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

    #[test]
    fn greater_than() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

        let ore = "
                   fn main() -> bool {
                       5 > 2
                   }";
        let ast = parse_dwarf("test_greater_than", ore).unwrap();
        let ctx = new_lu_dog(
            "test_greater_than".to_owned(),
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

        assert_eq!(&*s_read!(result.unwrap()), &true.into());
    }
}
