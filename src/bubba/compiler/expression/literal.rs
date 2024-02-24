use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Error, Result, EMPTY, STRING},
        instr::Instruction,
        value::Value,
    },
    lu_dog::{BooleanLiteralEnum, FormatBitEnum, LiteralEnum, ValueType},
    s_read, SarzakStorePtr, Span, POP_CLR,
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
                BooleanLiteralEnum::FalseLiteral(_) => Ok::<Value, Error>(Value::Boolean(false)),
                BooleanLiteralEnum::TrueLiteral(_) => Ok(Value::Boolean(true)),
            }
        }
        //
        // StringLiteral
        //
        LiteralEnum::CharLiteral(ref literal) => {
            let literal = lu_dog.exhume_char_literal(literal).unwrap();
            let literal = std::char::from_u32(s_read!(literal).x_value as u32).unwrap();
            let value = Value::Char(literal);
            Ok(value)
        }
        //
        // FloatLiteral
        //
        LiteralEnum::FloatLiteral(ref literal) => {
            let literal = lu_dog.exhume_float_literal(literal).unwrap();
            let value = s_read!(literal).x_value;
            let value = Value::Float(value);
            Ok(value)
        }
        //
        // FormatString
        //
        LiteralEnum::FormatString(ref string) => {
            let string = lu_dog.exhume_format_string(string).unwrap();
            let string = s_read!(string);
            if let Some(ref first) = string.first_format_bit {
                let mut next = lu_dog.exhume_format_bit(first).unwrap();

                let mut first = true;
                loop {
                    {
                        let current = s_read!(next);
                        match current.subtype {
                            FormatBitEnum::ExpressionBit(ref expr) => {
                                let expr_bit = lu_dog.exhume_expression_bit(expr).unwrap();
                                let expr_bit = s_read!(expr_bit);
                                let expr = lu_dog.exhume_expression(&expr_bit.expression).unwrap();
                                let span = get_span(&expr, &lu_dog);
                                compile_expression(&expr, thonk, context)?;
                                thonk.insert_instruction_with_span(
                                    Instruction::ToString,
                                    span,
                                    location!(),
                                );
                            }
                            FormatBitEnum::StringBit(ref string) => {
                                let string_bit = lu_dog.exhume_string_bit(string).unwrap();
                                let string_bit = s_read!(string_bit);
                                let string =
                                    lu_dog.exhume_string_literal(&string_bit.z_string).unwrap();
                                let string = s_read!(string);
                                let literal = &string.r22_literal(&lu_dog)[0];
                                let expr = &s_read!(literal).r15_expression(&lu_dog)[0];
                                let span = get_span(expr, &lu_dog);

                                let value = Value::String(string.x_value.clone());
                                thonk.insert_instruction_with_span(
                                    Instruction::Push(value),
                                    span,
                                    location!(),
                                );
                            }
                        }
                        if !first {
                            thonk.insert_instruction_with_span(
                                Instruction::Add,
                                span.clone(),
                                location!(),
                            );
                        } else {
                            first = false;
                        }
                    }

                    if let Some(ref id) = s_read!(next.clone()).next {
                        next = lu_dog.exhume_format_bit(id).unwrap();
                    } else {
                        break;
                    }
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
            Ok(value)
        }
        //
        // StringLiteral
        //
        LiteralEnum::StringLiteral(ref literal) => {
            let literal = lu_dog.exhume_string_literal(literal).unwrap();
            // 🚧 It'd be great if this were an Rc...
            let value = Value::String(s_read!(literal).x_value.clone());
            Ok(value)
        }
    };

    thonk.insert_instruction_with_span(Instruction::Push(literal?), span, location!());

    Ok(None)
}
