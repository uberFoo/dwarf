use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{BubbaError, CThonk, Context, Result},
        instr::Instruction,
    },
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, POP_CLR,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<String>> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_variable"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_variable_expression(expr).unwrap();
    let expr = s_read!(expr);
    let name = &expr.name;

    if let Some(symbol) = context.get_symbol(name) {
        thonk.add_instruction_with_span(Instruction::FetchLocal(symbol.number), span, location!());
    } else if context.check_function(name) {
        let name = new_ref!(String, name.to_owned());
        // We are here because we need to look up a function.
        thonk.add_instruction(Instruction::CallDestination(name.clone()), location!());

        // This instruction will be patched by the VM with the number of locals in the
        // function.
        thonk.add_instruction(Instruction::LocalCardinality(name), location!());
    } else {
        let (new, number) = context.insert_symbol(name.to_owned());
        if new {
            thonk.increment_frame_size();
        }
        if let Some(ref mut captures) = context.captures {
            captures.insert(name.to_owned(), number);
            thonk.add_instruction_with_span(Instruction::FetchLocal(number), span, location!());
        } else {
            return Err(BubbaError::InternalCompilerError {
                message: format!("Variable {} not found", name),
                location: location!(),
            }
            .into());
        }
    }

    Ok(None)
}
