use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::CallEnum,
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile(
    call: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let call = lu_dog.exhume_call(call).unwrap();
    let call = s_read!(call);
    let first_arg = call.argument;
    let args = call.r28_argument(&lu_dog);

    let name = if let CallEnum::FunctionCall(ref call) = call.subtype {
        let call = lu_dog.exhume_function_call(call).unwrap();
        let call = s_read!(call);
        call.name.clone()
    } else {
        todo!("handle the other calls");
    };

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

    if let Some(ref expr) = call.expression {
        let expr = lu_dog.exhume_expression(expr).unwrap();
        // Evaluate the LHS to get at the underlying value/instance.
        compile_expression(&expr, thonk, context)?;
    };

    for (name, expr) in params.into_iter().zip(arg_exprs) {
        compile_expression(&expr, thonk, context)?;
        context.insert_symbol(name);
    }

    thonk.add_instruction(Instruction::Call(args.len()));

    Ok(())
}
