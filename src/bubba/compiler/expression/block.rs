use crate::{
    bubba::compiler::{compile_statement, CThonk, Context, Result},
    lu_dog::ValueType,
    s_read, SarzakStorePtr, POP_CLR,
};

pub(in crate::bubba::compiler) fn compile(
    block: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}:\n  --> {}:{}:{}", POP_CLR.paint("block"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    // let empty = (*s_read!(ValueType::new_empty(true, &mut s_write!(lu_dog)))).clone();
    let lu_dog = s_read!(lu_dog);

    let block = lu_dog.exhume_block(block).unwrap();
    let stmts = s_read!(block).r18_statement(&lu_dog);
    if !stmts.is_empty() {
        context.push_scope();
        let mut next = s_read!(block).r71_statement(&lu_dog)[0].clone();
        // let mut ty = Some(empty.clone());

        loop {
            // ty = compile_statement(&next, thonk, context)?;
            compile_statement(&next, thonk, context)?;

            if let Some(ref id) = s_read!(next.clone()).next {
                next = lu_dog.exhume_statement(id).unwrap();
            } else {
                break;
            }
        }
        context.pop_scope();

        // Ok(ty)
        Ok(None)
    } else {
        // Ok(Some(empty))
        Ok(None)
    }
}
