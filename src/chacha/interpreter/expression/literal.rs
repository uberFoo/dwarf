use ansi_term::Colour;

use crate::{
    chacha::error::Result,
    interpreter::{debug, function, Context},
    lu_dog::{BooleanLiteralEnum, LiteralEnum, ValueType},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_literal(
    literal: &SarzakStorePtr,
    context: &mut Context,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let literal = s_read!(lu_dog).exhume_literal(literal).unwrap();

    debug!("literal {literal:?}");

    let z = match &s_read!(literal).subtype {
        //
        // BooleanLiteral
        //
        LiteralEnum::BooleanLiteral(ref literal) => {
            let literal = s_read!(lu_dog).exhume_boolean_literal(literal).unwrap();
            let literal = s_read!(literal);
            let ty = Value::Boolean(true).get_type(&s_read!(lu_dog));

            match literal.subtype {
                BooleanLiteralEnum::FalseLiteral(_) => {
                    Ok((new_ref!(Value, Value::Boolean(false,)), ty))
                }
                BooleanLiteralEnum::TrueLiteral(_) => {
                    Ok((new_ref!(Value, Value::Boolean(true,)), ty))
                }
            }
        }
        //
        // FloatLiteral
        //
        LiteralEnum::FloatLiteral(ref literal) => {
            let literal = s_read!(lu_dog).exhume_float_literal(literal).unwrap();
            let value = s_read!(literal).x_value;
            let value = Value::Float(value);
            let ty = value.get_type(&s_read!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
        //
        // IntegerLiteral
        //
        LiteralEnum::IntegerLiteral(ref literal) => {
            let literal = s_read!(lu_dog).exhume_integer_literal(literal).unwrap();
            let value = s_read!(literal).x_value;
            let value = Value::Integer(value);
            let ty = value.get_type(&s_read!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
        //
        // StringLiteral
        //
        LiteralEnum::StringLiteral(ref literal) => {
            let literal = s_read!(lu_dog).exhume_string_literal(literal).unwrap();
            // 🚧 It'd be great if this were an Rc...
            let value = Value::String(s_read!(literal).x_value.clone());
            let ty = value.get_type(&s_read!(lu_dog));
            Ok((new_ref!(Value, value), ty))
        }
    };

    #[allow(clippy::let_and_return)]
    z
}
