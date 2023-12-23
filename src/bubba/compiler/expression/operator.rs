use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::{BinaryEnum, ComparisonEnum, ExpressionEnum, OperatorEnum},
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile(
    op_type: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);
    let operator = lu_dog.exhume_operator(op_type).unwrap();
    let operator = s_read!(operator);
    let lhs = lu_dog.exhume_expression(&operator.lhs).unwrap();

    match operator.subtype {
        OperatorEnum::Binary(ref op_type) => {
            let binary = lu_dog.exhume_binary(op_type).unwrap();
            let binary = s_read!(binary);
            let rhs = lu_dog.exhume_expression(&operator.rhs.unwrap()).unwrap();

            match binary.subtype {
                BinaryEnum::Addition(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.add_instruction(Instruction::Add);
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

                    compile_expression(&rhs, thonk, context)?;
                    thonk.add_instruction(Instruction::StoreLocal(offset));
                }
                BinaryEnum::BooleanOperator(_) => {
                    todo!("BooleanOperator")
                }
                BinaryEnum::Division(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.add_instruction(Instruction::Divide);
                }
                BinaryEnum::Subtraction(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.add_instruction(Instruction::Subtract);
                }
                BinaryEnum::Multiplication(_) => {
                    compile_expression(&lhs, thonk, context)?;
                    compile_expression(&rhs, thonk, context)?;
                    thonk.add_instruction(Instruction::Multiply);
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
                    thonk.add_instruction(Instruction::TestEq);
                }
                _ => todo!("comparison"),
            }
        }
        OperatorEnum::Unary(ref _id) => {
            todo!("unary");
        }
    }

    Ok(())
}
