use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
        value::Value,
    },
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile_list_element(
    element: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_list_element"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let element = lu_dog.exhume_list_element(element).unwrap();
    let element = s_read!(element);
    let expr = element.r55_expression(&lu_dog)[0].clone();

    compile_expression(&expr, thonk, context)?;

    Ok(None)
}

/// Compile a List Expression
///
/// Note that we can't do too much here in the compiler. We can evaluate the
/// list elements and push them on the stack, but we don't have a way of building
/// the list. That has to be taken care of at runtime.
#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile_list_expression(
    list: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    entry_span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_list_expression"), file!(), line!(), column!());

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

        compile_expression(&expr, thonk, context)?;

        let mut next = element.next;
        while let Some(ref id) = next {
            let element = lu_dog.exhume_list_element(id).unwrap();
            let element = s_read!(element);
            let expr = element.r15_expression(&lu_dog)[0].clone();

            compile_expression(&expr, thonk, context)?;
            size += 1;

            next = element.next;
        }

        let expr = &list.r15_expression(&lu_dog)[0];
        let ty = &s_read!(expr).r11_x_value(&lu_dog)[0];
        let ty = s_read!(ty).r24_value_type(&lu_dog)[0].clone();
        let ty = (*s_read!(ty)).clone();
        thonk.insert_instruction(Instruction::Push(Value::ValueType(ty)), location!());

        thonk.insert_instruction(Instruction::NewList(size), location!());
    } else {
        let ty = Value::Vector {
            ty: Value::Empty.get_value_type(&sarzak, &lu_dog),
            inner: new_ref!(Vec<RefType<Value>>, vec![]),
        }
        .get_value_type(&sarzak, &lu_dog);
        let ty = (*s_read!(ty)).clone();
        thonk.insert_instruction(Instruction::Push(Value::ValueType(ty)), location!());
        thonk.insert_instruction_with_span(Instruction::NewList(0), entry_span, location!());
    }

    Ok(None)
}

#[cfg(test)]
mod test {

    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };
}
