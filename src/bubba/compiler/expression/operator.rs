use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::{
        BinaryEnum, BooleanOperatorEnum, ComparisonEnum, ExpressionEnum, OperatorEnum, UnaryEnum,
    },
    s_read, SarzakStorePtr, Span,
};

pub(in crate::bubba::compiler) fn compile(
    op_type: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
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
                    let offset = if let ExpressionEnum::VariableExpression(ref expr) =
                        &s_read!(lhs).subtype
                    {
                        let expr = lu_dog.exhume_variable_expression(expr).unwrap();
                        let expr = s_read!(expr);
                        context
                            .get_symbol(&expr.name)
                            .unwrap_or_else(|| panic!("symbol lookup failed for {}", expr.name))
                    } else {
                        panic!("In assignment and lhs is not a variable.")
                    };

                    compile_expression(&rhs, thonk, context, rhs_span)?;
                    thonk.add_instruction_with_span(
                        Instruction::StoreLocal(offset),
                        span,
                        location!(),
                    );
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
