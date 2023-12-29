use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    keywords::{ASSERT, ASSERT_EQ, CHACHA},
    lu_dog::{Call, CallEnum, Expression},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    call: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let call_rt = lu_dog.exhume_call(call).unwrap();
    let call = s_read!(call_rt);
    let first_arg = call.argument;
    let args = call.r28_argument(&lu_dog);

    let arg_exprs = if let Some(next) = first_arg {
        let mut exprs = Vec::with_capacity(args.len());
        let mut next = lu_dog.exhume_argument(&next).unwrap();

        loop {
            let expr = s_read!(next).r37_expression(&lu_dog)[0].clone();
            exprs.push(expr);

            let next_id = { s_read!(next).next };
            if let Some(ref id) = next_id {
                next = lu_dog.exhume_argument(id).unwrap();
            } else {
                break;
            }
        }

        exprs
    } else {
        Vec::new()
    };

    match call.subtype {
        CallEnum::FunctionCall(ref call) => {
            let call = lu_dog.exhume_function_call(call).unwrap();
            compile_function_call(
                &s_read!(call).name,
                call_rt.clone(),
                &arg_exprs,
                thonk,
                context,
            )?;
        }
        CallEnum::StaticMethodCall(ref meth) => {
            let meth = lu_dog.exhume_static_method_call(meth).unwrap();
            let meth = s_read!(meth);
            compile_static_method_call(&meth.ty, &meth.func, &arg_exprs, thonk, context, span)?;
        }
        _ => todo!("handle the other calls"),
    };

    Ok(())
}

fn compile_static_method_call(
    ty: &String,
    func: &String,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    match ty.as_str() {
        CHACHA => match func.as_str() {
            ASSERT => {
                let expr = &args[0];
                let expr_span = get_span(expr, &lu_dog);
                compile_expression(expr, thonk, context, expr_span)?;
                thonk.add_instruction_with_span(
                    Instruction::Push(new_ref!(Value, true.into())),
                    span.clone(),
                );
                thonk.add_instruction_with_span(Instruction::TestEq, span.clone());
                thonk.add_instruction_with_span(Instruction::JumpIfTrue(1), span.clone());
                thonk.add_instruction_with_span(Instruction::HaltAndCatchFire, span);
            }
            ASSERT_EQ => {
                let lhs = &args[0];
                let rhs = &args[1];
                let lhs_span = get_span(lhs, &lu_dog);
                compile_expression(lhs, thonk, context, lhs_span)?;
                let rhs_span = get_span(rhs, &lu_dog);
                compile_expression(rhs, thonk, context, rhs_span)?;
                thonk.add_instruction_with_span(Instruction::TestEq, span.clone());
                thonk.add_instruction_with_span(Instruction::JumpIfTrue(1), span.clone());
                thonk.add_instruction_with_span(Instruction::HaltAndCatchFire, span);
            }
            _ => todo!("handle other chacha methods"),
        },
        _ => todo!("handle other static methods"),
    }

    Ok(())
}

fn compile_function_call(
    name: &String,
    call: RefType<Call>,
    args: &[RefType<Expression>],
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let func = lu_dog.exhume_function_id_by_name(&name).unwrap();
    let func = lu_dog.exhume_function(&func).unwrap();
    let func = s_read!(func);
    let params = func.r13_parameter(&lu_dog);

    let params = if !params.is_empty() {
        let mut params = Vec::with_capacity(params.len());
        let mut next = func
            .r13_parameter(&lu_dog)
            .iter()
            .find(|p| s_read!(p).r14c_parameter(&lu_dog).is_empty())
            .unwrap()
            .clone();

        loop {
            // Apparently I'm being clever. I don't typecheck against an actual
            // type associated with the parameter. No, I am looking up the variable
            // associated with the parameter and using it's type. I guess that's cool,
            // but it's tricky if you aren't aware.
            let var = s_read!(s_read!(next).r12_variable(&lu_dog)[0]).clone();
            params.push(var.name.clone());

            let next_id = { s_read!(next).next };
            if let Some(ref id) = next_id {
                next = lu_dog.exhume_parameter(id).unwrap();
            } else {
                break;
            }
        }

        params
    } else {
        Vec::new()
    };

    if let Some(ref expr) = s_read!(call).expression {
        let expr = lu_dog.exhume_expression(expr).unwrap();
        let span = get_span(&expr, &lu_dog);
        // Evaluate the LHS to get at the underlying value/instance.
        compile_expression(&expr, thonk, context, span)?;
    };

    for (name, expr) in params.into_iter().zip(args) {
        let span = get_span(&expr, &lu_dog);
        compile_expression(&expr, thonk, context, span)?;
        context.insert_symbol(name);
    }

    thonk.add_instruction(Instruction::Call(args.len()));

    Ok(())
}
