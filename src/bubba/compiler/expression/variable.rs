use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{BubbaError, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}:\n  --> {}:{}:{}", POP_CLR.paint("compile_variable"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_variable_expression(expr).unwrap();
    let expr = s_read!(expr);
    let name = &expr.name;

    tracing::debug!(target: "instr", "Variable: {}", POP_CLR.paint(name));

    if let Some(symbol) = context.get_symbol(name) {
        thonk.insert_instruction_with_span(
            Instruction::FetchLocal(symbol.number),
            span,
            location!(),
        );
        Ok(Some(symbol.ty.clone()))
    } else if let Some(ty) = context.check_function(name) {
        // We are here because we need to look up a function.
        thonk.insert_instruction(Instruction::CallDestination(name.to_owned()), location!());

        // This instruction will be patched by the VM with the number of locals in the
        // function.
        thonk.insert_instruction(Instruction::LocalCardinality(name.to_owned()), location!());

        Ok(Some(ty.clone()))
    } else {
        // We get down here if the variable is not in scope, and it's not a function.
        // Therefor we must be doing a lambda and it's accessing it's enclosing
        // scope. We store it in the table, so that it has a local index, and then
        // we place it in the captures list.
        let unknown = context.get_type("UNKNOWN").unwrap().clone();

        let (new, number) = context.insert_symbol(name.to_owned(), unknown.clone());
        if new {
            thonk.increment_frame_size();
        }
        if let Some(ref mut captures) = context.captures {
            captures.insert(name.to_owned(), number);
            thonk.insert_instruction_with_span(Instruction::FetchLocal(number), span, location!());
        } else {
            return Err(BubbaError::InternalCompilerError {
                message: format!("Variable {} not found", name),
                location: location!(),
            }
            .into());
        }

        Ok(Some(unknown))
    }
}
