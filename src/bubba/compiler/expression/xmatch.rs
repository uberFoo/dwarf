use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::CallEnum,
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let match_expr = lu_dog.exhume_x_match(expr).unwrap();
    let match_expr = s_read!(match_expr);

    let patterns = match_expr.r87_pattern(&lu_dog);
    let scrutinee = match_expr.r91_expression(&lu_dog)[0].clone();

    compile_expression(&scrutinee, thonk, context)?;

    for pattern in patterns {
        let pattern = s_read!(pattern);
        let match_expr = pattern.r87_expression(&lu_dog)[0].clone();
        let expr = pattern.r92_expression(&lu_dog)[0].clone();

        compile_expression(&match_expr, thonk, context)?;
    }

    Ok(())
}
