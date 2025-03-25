use std::ops::Range;

use crate::{
    dwarf::{
        error::Result,
        extruder::{inter_expression, Context, ExprSpan},
        Expression as ParserExpression,
    },
    lu_dog::{store::ObjectStore as LuDogStore, Block, Span, ValueType},
    new_ref, NewRef, RefType,
};

pub fn inter(
    expr: Box<(ParserExpression, Range<usize>)>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let (expr, ty) = inter_expression(
        &new_ref!(ParserExpression, expr.0.to_owned()),
        &expr.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;
    Ok((expr, ty))
}
