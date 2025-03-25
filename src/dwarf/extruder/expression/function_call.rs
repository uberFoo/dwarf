use std::ops::Range;

use log::debug;
use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{
            inter_expression, link_argument, update_span_value, Context, ExprSpan,
            PrintableValueType,
        },
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Argument, Block, Call, Expression, FunctionCall, Span,
        ValueType, ValueTypeEnum, XValue,
    },
    new_ref, s_read, s_write, DwarfInteger, NewRef, RefType, SarzakStorePtr,
};

pub fn inter(
    func: Box<(ParserExpression, Range<usize>)>,
    args: &Vec<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("func {func:?}");
    let fspan = &func.1;
    let func = &func.0;
    debug!("args {args:?}");

    let (func_expr, ret_ty) = inter_expression(
        &new_ref!(ParserExpression, func.to_owned()),
        fspan,
        block,
        context,
        import_stack,
        lu_dog,
    )?;
    debug!("func_expr {func_expr:?}");

    let ret_ty = if let ValueTypeEnum::Lambda(ref l) = s_read!(ret_ty).subtype {
        let l = lu_dog.exhume_lambda(l).unwrap();
        let ret_ty = s_read!(l).return_type.clone();
        let ret_ty = lu_dog.exhume_value_type(&ret_ty).unwrap();
        ret_ty
    } else {
        ret_ty.clone()
    };

    let name = match func {
        ParserExpression::LocalVariable(name) => name,
        _ => "not-a-local-variable",
    };

    let func_call = FunctionCall::new(name.to_owned(), lu_dog);
    let func_call = Call::new_function_call(true, None, Some(&func_expr.0), &func_call, lu_dog);
    let func = Expression::new_call(true, &func_call, lu_dog);
    let value = XValue::new_expression(block, &ret_ty, &func, lu_dog);
    update_span_value(&span, &value, location!());

    let mut last_arg_uuid: Option<SarzakStorePtr> = None;
    // Note that position makes each arg unique. I don't remember if
    // that is the explicit intention or not.
    for (position, arg) in args.iter().enumerate() {
        let (arg_expr, _ty) = inter_expression(
            &new_ref!(ParserExpression, arg.0.to_owned()),
            &arg.1,
            block,
            context,
            import_stack,
            lu_dog,
        )?;
        let arg = Argument::new(
            position as DwarfInteger,
            &arg_expr.0,
            &func_call,
            None,
            lu_dog,
        );

        if position == 0 {
            s_write!(func_call).argument = Some(s_read!(arg).id);
        }

        last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
    }

    debug!(
        "ParserExpression::FunctionCall exit {:?}",
        (&func_call, s_read!(func_call).r28_argument(lu_dog))
    );

    debug!(
        "return type {}",
        PrintableValueType(true, &ret_ty, context, lu_dog).to_string()
    );

    Ok(((func, span), ret_ty))
}
