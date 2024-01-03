use crate::{
    bubba::compiler::{compile_expression, get_span, CThonk, Context, Result},
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile_field_expression(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let field_expr = lu_dog.exhume_field_expression(expr).unwrap();
    let expr = s_read!(field_expr).r38_expression(&lu_dog)[0].clone();

    compile_expression(&expr, thonk, context, get_span(&expr, &lu_dog))?;

    Ok(())
}
