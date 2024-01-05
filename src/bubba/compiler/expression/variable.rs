use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{BubbaError, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ValueTypeEnum,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_variable_expression(expr).unwrap();
    let expr = s_read!(expr);
    let name = &expr.name;

    if let Some(symbol) = context.get_symbol(name) {
        if let Some(method) = &context.method_name {
            let ty = if let ValueTypeEnum::Enumeration(ref id) = symbol.ty.subtype {
                let enum_ty = lu_dog.exhume_enumeration(id).unwrap();
                let enum_ty = s_read!(enum_ty);
                enum_ty.name.to_owned()
            } else if let ValueTypeEnum::WoogStruct(ref id) = symbol.ty.subtype {
                let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
                let woog_struct = s_read!(woog_struct);
                woog_struct.name.to_owned()
            } else {
                return Err(BubbaError::InternalCompilerError {
                    message: "Asked to dereference something that isn't a function.".to_owned(),
                    location: location!(),
                }
                .into());
            };

            let func_name = format!("{ty}::{method}");
            let name = new_ref!(Value, Value::String(func_name));
            // We are here because we need to look up a function.
            thonk.add_instruction(Instruction::CallDestination(name.clone()), location!());

            // This instruction will be patched by the VM with the number of locals in the
            // function.
            thonk.add_instruction(Instruction::LocalCardinality(name), location!());
        } else {
            thonk.add_instruction_with_span(
                Instruction::FetchLocal(symbol.number),
                span,
                location!(),
            );
        }
    } else {
        let name = new_ref!(Value, Value::String(name.to_owned()));
        // We are here because we need to look up a function.
        thonk.add_instruction(Instruction::CallDestination(name.clone()), location!());

        // This instruction will be patched by the VM with the number of locals in the
        // function.
        thonk.add_instruction(Instruction::LocalCardinality(name), location!());
    }

    Ok(())
}
