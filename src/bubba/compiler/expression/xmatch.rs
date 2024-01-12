use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, expression::literal, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ExpressionEnum,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let match_expr = lu_dog.exhume_x_match(expr).unwrap();
    let match_expr = s_read!(match_expr);

    let patterns = match_expr.r87_pattern(&lu_dog);
    let scrutinee = match_expr.r91_expression(&lu_dog)[0].clone();
    let scrutinee_span = get_span(&scrutinee, &lu_dog);

    // Compiling the scrutinee
    compile_expression(&scrutinee, thonk, context, scrutinee_span)?;

    for pattern in patterns {
        log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

        // We push this up here because of the pattern matching needs it's own context.
        context.push_child_symbol_table();

        let pattern = s_read!(pattern);
        let match_expr = pattern.r87_expression(&lu_dog)[0].clone();
        let pattern_expr = pattern.r92_expression(&lu_dog)[0].clone();

        // Duplicate the scrutinee with which to compare against.
        thonk.add_instruction(Instruction::Dup, location!());

        // Compile the match expression.
        match &s_read!(match_expr).subtype {
            ExpressionEnum::Literal(ref literal) => {
                log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

                literal::compile(literal, thonk, context, span.clone())?
            }
            ExpressionEnum::StructExpression(ref id) => {
                let struct_expr = lu_dog.exhume_struct_expression(id).unwrap();
                let struct_expr = s_read!(struct_expr);

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
                                    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

                                    literal::compile(literal, thonk, context, span.clone())?
                                }
                                ExpressionEnum::VariableExpression(ref id) => {
                                    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

                                    let var = lu_dog.exhume_variable_expression(id).unwrap();
                                    let var = s_read!(var);
                                    let expr = var.r15_expression(&lu_dog)[0].clone();
                                    let value = s_read!(expr).r11_x_value(&lu_dog)[0].clone();
                                    let ty = s_read!(value).r24_value_type(&lu_dog)[0].clone();

                                    let idx = match context
                                        .insert_symbol(var.name.clone(), s_read!(ty).clone())
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
                                    thonk.add_instruction(Instruction::Dup, location!());
                                    thonk.add_instruction(
                                        Instruction::ExtractEnumValue,
                                        location!(),
                                    );
                                    thonk.add_instruction(Instruction::Dup, location!());
                                    thonk
                                        .add_instruction(Instruction::StoreLocal(idx), location!());
                                    // let pattern_span = get_span(&pattern_expr, &lu_dog);

                                    // compile_expression(
                                    //     &pattern_expr,
                                    //     thonk,
                                    //     context,
                                    //     pattern_span,
                                    // )?;
                                }

                                // thonk.add_instruction(Instruction::Dup, location!());
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
                thonk.add_instruction(Instruction::Push(ty), location!());

                let x_path = &lu_dog.exhume_x_path(&struct_expr.x_path).unwrap();
                // We know that there is always a pe. It's only in an option so that
                // we can construct everything.
                let mut pe = s_read!(x_path).r97_path_element(&lu_dog)[0].clone();
                while s_read!(pe).next.is_some() {
                    let id = {
                        let id = &s_read!(pe).next;
                        #[allow(clippy::clone_on_copy)]
                        id.as_ref().unwrap().clone()
                    };
                    thonk.add_instruction(
                        Instruction::Push(new_ref!(Value, s_read!(pe).name.clone().into())),
                        location!(),
                    );
                    pe = lu_dog.exhume_path_element(&id).unwrap();
                }
                thonk.add_instruction(
                    Instruction::Push(new_ref!(Value, s_read!(pe).name.clone().into())),
                    location!(),
                );

                thonk.add_instruction(Instruction::NewTupleEnum(field_exprs.len()), location!());
            }
            ExpressionEnum::VariableExpression(ref id) => {
                let var = lu_dog.exhume_variable_expression(id).unwrap();
                let var = s_read!(var);
                let expr = var.r15_expression(&lu_dog)[0].clone();
                let value = s_read!(expr).r11_x_value(&lu_dog)[0].clone();
                let ty = s_read!(value).r24_value_type(&lu_dog)[0].clone();

                let idx = match context.insert_symbol(var.name.clone(), s_read!(ty).clone()) {
                    (true, idx) => {
                        thonk.increment_frame_size();
                        idx
                    }
                    (false, idx) => idx,
                };

                thonk.add_instruction(Instruction::Dup, location!());
                thonk.add_instruction(Instruction::StoreLocal(idx), location!());
            }
            todo => todo!("Match expression type: {todo:?}"),
        }

        thonk.add_instruction(Instruction::TestEq, location!());

        // Compile the match block.
        let mut match_thonk = CThonk::new("match".to_owned());

        log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());
        compile_expression(
            &pattern_expr,
            &mut match_thonk,
            context,
            get_span(&pattern_expr, &lu_dog),
        )?;

        let fp = match_thonk.get_frame_size();
        for _ in 0..fp {
            thonk.increment_frame_size();
        }
        let match_len = match_thonk.get_instruction_card() as isize;

        // Jump over the matching block if we don't match.
        thonk.add_instruction(Instruction::JumpIfFalse(match_len + 1), location!());

        // Insert the compiled matching block
        thonk.append(match_thonk);

        // Return if we matched.
        thonk.add_instruction(Instruction::Return, location!());

        context.pop_symbol_table();
    }

    // This should not really happen because if this were done right the compiler
    // would notice that there are cases not caught and  fall through.
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

    Ok(())
}
