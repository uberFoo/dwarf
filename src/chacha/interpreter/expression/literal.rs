use ansi_term::Colour;

use crate::{
    bubba::VM,
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{BooleanLiteralEnum, FormatBitEnum, LiteralEnum},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    literal: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let literal = lu_dog.exhume_literal(literal).unwrap();

    debug!("literal {literal:?}");

    let z = match &s_read!(literal).subtype {
        //
        // BooleanLiteral
        //
        LiteralEnum::BooleanLiteral(ref literal) => {
            let literal = lu_dog.exhume_boolean_literal(literal).unwrap();
            let literal = s_read!(literal);

            match literal.subtype {
                BooleanLiteralEnum::FalseLiteral(_) => Ok(new_ref!(Value, Value::Boolean(false,))),
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
            let string = lu_dog.exhume_format_string(string).unwrap();
            let string = s_read!(string);
            if let Some(ref first) = string.first_format_bit {
                let mut next = lu_dog.exhume_format_bit(first).unwrap();

                let mut result = String::new();
                loop {
                    {
                        let current = s_read!(next);
                        match current.subtype {
                            FormatBitEnum::ExpressionBit(ref expr) => {
                                let expr_bit = lu_dog.exhume_expression_bit(expr).unwrap();
                                let expr_bit = s_read!(expr_bit);
                                let expr = lu_dog.exhume_expression(&expr_bit.expression).unwrap();
                                let value = eval_expression(expr, context, vm)?;

                                result += &s_read!(value).to_inner_string();
                            }
                            FormatBitEnum::StringBit(ref string) => {
                                let string_bit = lu_dog.exhume_string_bit(string).unwrap();
                                let string_bit = s_read!(string_bit);
                                let string =
                                    lu_dog.exhume_string_literal(&string_bit.z_string).unwrap();
                                let string = s_read!(string);

                                result += &string.x_value;
                            }
                        }
                    }

                    if let Some(ref id) = s_read!(next.clone()).next {
                        next = lu_dog.exhume_format_bit(id).unwrap();
                    } else {
                        break;
                    }
                }

                Ok(new_ref!(Value, result.into()))
            } else {
                Ok(new_ref!(Value, Value::Empty))
            }
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
            // 🚧 It'd be great if this were an Rc...
            let value = Value::String(s_read!(literal).x_value.clone());
            Ok(new_ref!(Value, value))
        }
    };

    #[allow(clippy::let_and_return)]
    z
}
