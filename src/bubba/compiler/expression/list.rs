use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile_list_element(
    element: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let element = lu_dog.exhume_list_element(element).unwrap();
    let element = s_read!(element);
    let expr = element.r55_expression(&lu_dog)[0].clone();
    let span = get_span(&expr, &lu_dog);
    compile_expression(&expr, thonk, context, span)?;

    Ok(())
}

/// Compile a List Expression
///
/// Note that we can't do too much here in the compiler. We can evaluate the
/// list elements and push them on the stack, but we don't have a way of building
/// the list. That has to be taken care of at runtime.
pub(in crate::bubba::compiler) fn compile_list_expression(
    list: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    entry_span: Span,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);
    let sarzak = context.sarzak_heel().clone();
    let sarzak = s_read!(sarzak);

    let list = lu_dog.exhume_list_expression(list).unwrap();
    let list = s_read!(list);
    if let Some(ref element) = list.elements {
        let mut size = 1;
        let element = lu_dog.exhume_list_element(element).unwrap();
        let element = s_read!(element);
        let expr = element.r15_expression(&lu_dog)[0].clone();
        let span = get_span(&expr, &lu_dog);
        compile_expression(&expr, thonk, context, span)?;

        let mut next = element.next;
        while let Some(ref id) = next {
            let element = lu_dog.exhume_list_element(id).unwrap();
            let element = s_read!(element);
            let expr = element.r15_expression(&lu_dog)[0].clone();
            let span = get_span(&expr, &lu_dog);
            compile_expression(&expr, thonk, context, span)?;
            size += 1;

            next = element.next;
        }

        let expr = &list.r15_expression(&lu_dog)[0];
        let ty = &s_read!(expr).r11_x_value(&lu_dog)[0];
        let ty = s_read!(ty).r24_value_type(&lu_dog)[0].clone();
        let ty = (*s_read!(ty)).clone();
        thonk.add_instruction(
            Instruction::Push(new_ref!(Value, Value::ValueType(ty))),
            location!(),
        );

        thonk.add_instruction(Instruction::NewList(size), location!());
    } else {
        let ty = Value::Vector {
            ty: Value::Empty.get_value_type(&sarzak, &lu_dog),
            inner: vec![],
        }
        .get_value_type(&sarzak, &lu_dog);
        let ty = (*s_read!(ty)).clone();
        thonk.add_instruction(
            Instruction::Push(new_ref!(Value, Value::ValueType(ty))),
            location!(),
        );
        thonk.add_instruction_with_span(Instruction::NewList(0), entry_span, location!());
    }

    Ok(())
}
