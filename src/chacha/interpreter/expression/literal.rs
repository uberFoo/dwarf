use ansi_term::Colour;

use crate::{
    bubba::VM,
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{BooleanLiteralEnum, LiteralEnum},
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
        LiteralEnum::FormatString(ref literal) => {
            // let literal = lu_dog.exhume_format_string(literal).unwrap();
            // let mut list = s_read!(literal).r108_list_expression(&lu_dog);
            // let first = if let Some(list) = list.pop() {
            //     let values = s_read!(list).r54_list_element(&lu_dog);
            //     values.into_iter().find(|value| {
            //         let value = s_read!(value);
            //         if value.position == 0 {
            //             true
            //         } else {
            //             false
            //         }
            //     })
            // } else {
            //     None
            // };
            // if let Some(first) = first {
            //     let mut result = String::new();
            //     let first = s_read!(first);
            //     let next = first.next;

            //     let expr = lu_dog.exhume_expression(&first.expression).unwrap();
            //     let value = eval_expression(expr, context, vm)?;
            //     result += &s_read!(value).to_inner_string();

            //     while let Some(ref next) = next {
            //         let next = lu_dog.exhume_list_element(next).unwrap();
            //         let next = s_read!(next);
            //         let expr = lu_dog.exhume_expression(&next.expression).unwrap();
            //         let value = eval_expression(expr, context, vm)?;
            //         result += &s_read!(value).to_inner_string();

            //         next.next;
            //     }

            //     Ok(new_ref!(Value, result.into()))
            // } else {
            Ok(new_ref!(Value, Value::Empty))
            // }
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
