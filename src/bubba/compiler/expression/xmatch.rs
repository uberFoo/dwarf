use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, expression::literal, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ExpressionEnum,
    s_read, SarzakStorePtr, Span,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let match_expr = lu_dog.exhume_x_match(expr).unwrap();
    let match_expr = s_read!(match_expr);

    let patterns = match_expr.r87_pattern(&lu_dog);
    let scrutinee = match_expr.r91_expression(&lu_dog)[0].clone();
    let scrutinee_span = get_span(&scrutinee, &lu_dog);
    compile_expression(&scrutinee, thonk, context, scrutinee_span)?;

    for pattern in patterns {
        // We push this up here because of the pattern matching needs it's own context.
        context.push_symbol_table();

        let pattern = s_read!(pattern);
        let match_expr = pattern.r87_expression(&lu_dog)[0].clone();
        let expr = pattern.r92_expression(&lu_dog)[0].clone();

        // Duplicate the scrutinee with which to compare against.
        thonk.add_instruction(Instruction::Dup, location!());

        // This is the pattern we are matching against.
        match &s_read!(match_expr).subtype {
            ExpressionEnum::Literal(ref literal) => {
                literal::compile(literal, thonk, context, span.clone())?
            }
            ExpressionEnum::StructExpression(ref id) => {
                let struct_expr = lu_dog.exhume_struct_expression(id).unwrap();
                let struct_expr = s_read!(struct_expr);
                let field_exprs = struct_expr.r26_field_expression(&lu_dog);
                for f in field_exprs {
                    let expr = s_read!(f).r15_expression(&lu_dog)[0].clone();
                    let expr = s_read!(expr);
                    match &expr.subtype {
                        ExpressionEnum::FieldExpression(ref id) => {
                            let expr = lu_dog.exhume_field_expression(id).unwrap();
                            let expr = s_read!(expr).r38_expression(&lu_dog)[0].clone();

                            let expr = s_read!(expr);
                            // 🚧 I'm already in the middle of one of these.
                            match &expr.subtype {
                                ExpressionEnum::Literal(_) => {}
                                ExpressionEnum::VariableExpression(ref id) => {
                                    // let var = lu_dog.exhume_variable_expression(id).unwrap();
                                    // let var = s_read!(var);
                                    // let expr = var.r15_expression(&lu_dog)[0].clone();
                                    // let value = s_read!(expr).r11_x_value(&lu_dog)[0].clone();
                                    // let ty = s_read!(value).r24_value_type(&lu_dog)[0].clone();

                                    // let idx = context
                                    //     .insert_symbol(var.name.clone(), s_read!(ty).clone());
                                    // thonk.increment_frame_size();

                                    // thonk.add_instruction(Instruction::Dup, location!());
                                    // thonk
                                    //     .add_instruction(Instruction::StoreLocal(idx), location!());
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
            }
            ExpressionEnum::VariableExpression(ref id) => {
                let var = lu_dog.exhume_variable_expression(id).unwrap();
                let var = s_read!(var);
                let expr = var.r15_expression(&lu_dog)[0].clone();
                let value = s_read!(expr).r11_x_value(&lu_dog)[0].clone();
                let ty = s_read!(value).r24_value_type(&lu_dog)[0].clone();

                let idx = context.insert_symbol(var.name.clone(), s_read!(ty).clone());
                thonk.increment_frame_size();

                thonk.add_instruction(Instruction::Dup, location!());
                thonk.add_instruction(Instruction::StoreLocal(idx), location!());
            }
            todo => todo!("Match expression type: {todo:?}"),
        }

        // compile_expression(&match_expr, thonk, context, get_span(&match_expr, &lu_dog))?;
        thonk.add_instruction(Instruction::TestEq, location!());

        // Compile the match block.
        let mut match_thonk = CThonk::new("match".to_owned());

        compile_expression(&expr, &mut match_thonk, context, get_span(&expr, &lu_dog))?;
        let fp = match_thonk.get_frame_size();
        for _ in 0..fp {
            thonk.increment_frame_size();
        }
        let match_len = match_thonk.get_instruction_card() as isize;
        context.pop_symbol_table();

        // Jump over the matching block if we don't match.
        thonk.add_instruction(Instruction::JumpIfFalse(match_len + 1), location!());

        // Insert the compiled matching block
        thonk.append(match_thonk);

        // Return if we matched.
        thonk.add_instruction(Instruction::Return, location!());
    }

    thonk.add_instruction(Instruction::HaltAndCatchFire, location!());

    Ok(())
}
