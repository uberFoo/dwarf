use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{
            compile_expression, get_span, CThonk, Context, Result, BOOL, CHAR, EMPTY, FLOAT,
            INTEGER, MAP, STRING,
        },
        instr::Instruction,
        value::Value,
    },
    lu_dog::{BooleanLiteralEnum, FormatBitEnum, LiteralEnum, ValueType},
    s_read, SarzakStorePtr, Span, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(thonk, context)))]
pub(in crate::bubba::compiler) fn compile(
    literal: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}", POP_CLR.paint("compile_literal"));

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let literal = lu_dog.exhume_literal(literal).unwrap();

    tracing::debug!(target: "instr", "literal: {literal:?}");

    let ty = match &s_read!(literal).subtype {
        //
        // BooleanLiteral
        //
        LiteralEnum::BooleanLiteral(ref literal) => {
            let literal = lu_dog.exhume_boolean_literal(literal).unwrap();
            let literal = s_read!(literal);

            let value = match literal.subtype {
                BooleanLiteralEnum::FalseLiteral(_) => Value::Boolean(false),
                BooleanLiteralEnum::TrueLiteral(_) => Value::Boolean(true),
            };
            thonk.insert_instruction_with_span(Instruction::Push(value), span, location!());

            context.get_type(BOOL).unwrap().clone()
        }
        //
        // CharLiteral
        //
        LiteralEnum::CharLiteral(ref literal) => {
            let literal = lu_dog.exhume_char_literal(literal).unwrap();
            let literal = std::char::from_u32(s_read!(literal).x_value as u32).unwrap();
            let value = Value::Char(literal);
            thonk.insert_instruction_with_span(Instruction::Push(value), span, location!());

            context.get_type(CHAR).unwrap().clone()
        }
        //
        // FloatLiteral
        //
        LiteralEnum::FloatLiteral(ref literal) => {
            let literal = lu_dog.exhume_float_literal(literal).unwrap();
            let value = s_read!(literal).x_value;
            let value = Value::Float(value);
            thonk.insert_instruction_with_span(Instruction::Push(value), span, location!());

            context.get_type(FLOAT).unwrap().clone()
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
                                let ty = compile_expression(&expr, thonk, context)?.unwrap();
                                if ty != *context.get_type(STRING).unwrap() {
                                    thonk.insert_instruction_with_span(
                                        Instruction::ToString,
                                        span,
                                        location!(),
                                    );
                                }
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

            context.get_type(EMPTY).unwrap().clone()
        }
        //
        // IntegerLiteral
        //
        LiteralEnum::IntegerLiteral(ref literal) => {
            let literal = lu_dog.exhume_integer_literal(literal).unwrap();
            let value = s_read!(literal).x_value;
            let value = Value::Integer(value);
            thonk.insert_instruction_with_span(Instruction::Push(value), span, location!());

            context.get_type(INTEGER).unwrap().clone()
        }
        //
        // MapExpression
        //
        LiteralEnum::MapExpression(ref literal) => {
            let literal = lu_dog.exhume_map_expression(literal).unwrap();
            let literal = s_read!(literal);
            // let key_ty = literal.r115_value_type(&lu_dog);
            // let value_ty = literal.r116_value_type(&lu_dog);

            // thonk.insert_instruction_with_span(
            //     Instruction::Push(value_ty),
            //     span.clone(),
            //     location!(),
            // );
            // thonk.insert_instruction_with_span(
            //     Instruction::Push(key_ty),
            //     span.clone(),
            //     location!(),
            // );
            thonk.insert_instruction_with_span(Instruction::NewMap, span.clone(), location!());

            for pair in literal.r117_map_element(&lu_dog) {
                let pair = s_read!(pair);
                let key = lu_dog.exhume_expression(&pair.key).unwrap();
                compile_expression(&key, thonk, context)?.unwrap();
                let value = lu_dog.exhume_expression(&pair.x_value).unwrap();
                compile_expression(&value, thonk, context)?.unwrap();
                thonk.insert_instruction_with_span(
                    Instruction::MapInsert,
                    span.clone(),
                    location!(),
                );
            }

            context.get_type(MAP).unwrap().clone()
        }
        //
        // StringLiteral
        //
        LiteralEnum::StringLiteral(ref literal) => {
            let literal = lu_dog.exhume_string_literal(literal).unwrap();
            let value = Value::String(s_read!(literal).x_value.clone());
            thonk.insert_instruction_with_span(Instruction::Push(value), span, location!());

            context.get_type(STRING).unwrap().clone()
        }
    };

    Ok(Some(ty))
}

#[cfg(test)]
mod test {
    use std::env;

    use test_log::test;

    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, setup_logging},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn map_literal() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = r#"
fn main() {
    let map = {{"a": 42, "b": 96}};
    print(map);
}"#;
        let ast = parse_dwarf("map_literal", ore).unwrap();
        let ctx = new_lu_dog(
            "for_in_range".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &env::current_dir().unwrap(),
true,            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx, true).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 12);
    }
}
