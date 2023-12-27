use crate::{
    bubba::{
        compiler::{CThonk, Context, Error, Result},
        instr::Instruction,
    },
    lu_dog::{BooleanLiteralEnum, LiteralEnum},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    literal: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
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

    thonk.add_instruction_with_span(Instruction::Push(literal?), span);

    Ok(())
}
