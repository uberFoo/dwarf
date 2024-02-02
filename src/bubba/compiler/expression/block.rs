use crate::{
    bubba::compiler::{compile_statement, CThonk, Context, Result},
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile(
    block: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<String>> {
    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let block = lu_dog.exhume_block(block).unwrap();
    let stmts = s_read!(block).r18_statement(&lu_dog);
    if !stmts.is_empty() {
        context.push_scope();
        let mut next = s_read!(block).r71_statement(&lu_dog)[0].clone();

        loop {
            compile_statement(&next, thonk, context)?;

            if let Some(ref id) = s_read!(next.clone()).next {
                next = lu_dog.exhume_statement(id).unwrap();
            } else {
                break;
            }
        }
        context.pop_scope();
    }

    Ok(None)
}
