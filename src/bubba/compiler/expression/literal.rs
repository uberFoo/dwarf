use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Error, Result, EMPTY, STRING},
        instr::Instruction,
    },
    lu_dog::{BooleanLiteralEnum, FormatBits, FormatBitsEnum, LiteralEnum, ValueType},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    literal: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_literal"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let literal = lu_dog.exhume_literal(literal).unwrap();

    let literal = match &s_read!(literal).subtype {
        //
        // BooleanLiteral
        //
        LiteralEnum::BooleanLiteral(ref literal) => {
            let literal = lu_dog.exhume_boolean_literal(literal).unwrap();
            let literal = s_read!(literal);

            match literal.subtype {
                BooleanLiteralEnum::FalseLiteral(_) => {
                    Ok::<RefType<Value>, Error>(new_ref!(Value, Value::Boolean(false,)))
                }
                BooleanLiteralEnum::TrueLiteral(_) => Ok(new_ref!(Value, Value::Boolean(true,))),
            }
        }
        //
        // FloatLiteral
        //
        LiteralEnum::FloatLiteral(ref literal) => {
            let literal = lu_dog.exhume_float_literal(literal).unwrap();
            let value = s_read!(literal).x_value;
            let value = Value::Float(value);
            Ok(new_ref!(Value, value))
        }
        //
        // FormatString
        //
        LiteralEnum::FormatString(ref string) => {
            // let mut compile_bit = |bit: &FormatBits| -> Result<Option<ValueType>> {
            //     match bit.subtype {
            //         FormatBitsEnum::ExpressionBit(ref expr) => {
            //             let expr = lu_dog.exhume_expression(expr).unwrap();
            //             let span = get_span(&expr, &lu_dog);
            //             let result = compile_expression(&expr, thonk, context);
            //             thonk.insert_instruction_with_span(
            //                 Instruction::ToString,
            //                 span,
            //                 location!(),
            //             );
            //             result
            //         }
            //         FormatBitsEnum::StringBit(ref string) => {
            //             let string = lu_dog.exhume_string_literal(string).unwrap();
            //             let string = s_read!(string);
            //             let literal = &string.r22_literal(&lu_dog)[0];
            //             let expr = &s_read!(literal).r15_expression(&lu_dog)[0];
            //             let span = get_span(expr, &lu_dog);

            //             let value = Value::String(string.x_value.clone());
            //             thonk.insert_instruction_with_span(
            //                 Instruction::Push(new_ref!(Value, value)),
            //                 span,
            //                 location!(),
            //             );
            //             Ok(Some(context.get_type(STRING).unwrap().clone()))
            //         }
            //     }
            // };

            let string = lu_dog.exhume_format_string(string).unwrap();
            if let Some(ref first) = s_read!(string).first_format_bit {
                let first = lu_dog.exhume_format_bits(first).unwrap();
                let first = s_read!(first);

                // compile_bit(&first)?;
                match first.subtype {
                    FormatBitsEnum::ExpressionBit(ref expr) => {
                        let expr = lu_dog.exhume_expression(expr).unwrap();
                        let span = get_span(&expr, &lu_dog);
                        compile_expression(&expr, thonk, context)?;
                        thonk.insert_instruction_with_span(
                            Instruction::ToString,
                            span,
                            location!(),
                        );
                    }
                    FormatBitsEnum::StringBit(ref string) => {
                        let string = lu_dog.exhume_string_literal(string).unwrap();
                        let string = s_read!(string);
                        let literal = &string.r22_literal(&lu_dog)[0];
                        let expr = &s_read!(literal).r15_expression(&lu_dog)[0];
                        let span = get_span(expr, &lu_dog);

                        let value = Value::String(string.x_value.clone());
                        thonk.insert_instruction_with_span(
                            Instruction::Push(new_ref!(Value, value)),
                            span,
                            location!(),
                        );
                    }
                }

                while let Some(ref next) = first.next {
                    let next = lu_dog.exhume_format_bits(next).unwrap();
                    let next = s_read!(next);
                    // compile_bit(&next)?;
                    match next.subtype {
                        FormatBitsEnum::ExpressionBit(ref expr) => {
                            let expr = lu_dog.exhume_expression(expr).unwrap();
                            let span = get_span(&expr, &lu_dog);
                            compile_expression(&expr, thonk, context)?;
                            thonk.insert_instruction_with_span(
                                Instruction::ToString,
                                span,
                                location!(),
                            );
                        }
                        FormatBitsEnum::StringBit(ref string) => {
                            let string = lu_dog.exhume_string_literal(string).unwrap();
                            let string = s_read!(string);
                            let literal = &string.r22_literal(&lu_dog)[0];
                            let expr = &s_read!(literal).r15_expression(&lu_dog)[0];
                            let span = get_span(expr, &lu_dog);

                            let value = Value::String(string.x_value.clone());
                            thonk.insert_instruction_with_span(
                                Instruction::Push(new_ref!(Value, value)),
                                span,
                                location!(),
                            );
                        }
                    }
                    thonk.insert_instruction_with_span(Instruction::Add, span.clone(), location!());
                }

                return Ok(Some(context.get_type(STRING).unwrap().clone()));
            }
            return Ok(Some(context.get_type(EMPTY).unwrap().clone()));
        }
        //
        // IntegerLiteral
        //
        LiteralEnum::IntegerLiteral(ref literal) => {
            let literal = lu_dog.exhume_integer_literal(literal).unwrap();
            let value = s_read!(literal).x_value;
            let value = Value::Integer(value);
            Ok(new_ref!(Value, value))
        }
        //
        // StringLiteral
        //
        LiteralEnum::StringLiteral(ref literal) => {
            let literal = lu_dog.exhume_string_literal(literal).unwrap();
            let value = Value::String(s_read!(literal).x_value.clone());
            Ok(new_ref!(Value, value))
        }
    };

    thonk.insert_instruction_with_span(Instruction::Push(literal?), span, location!());

    Ok(None)
}
