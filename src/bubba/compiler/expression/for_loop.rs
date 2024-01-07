use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::{ValueType, ValueTypeEnum},
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    for_loop: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let for_loop = lu_dog.exhume_for_loop(for_loop).unwrap();
    let for_loop = s_read!(for_loop);
    let ident = for_loop.ident.to_owned();
    let body = lu_dog.exhume_expression(&for_loop.block).unwrap();
    let body_span = get_span(&body, &lu_dog);
    let list = lu_dog.exhume_expression(&for_loop.expression).unwrap();
    let list_span = get_span(&list, &lu_dog);
    compile_expression(&list, thonk, context, list_span)?;

    context.push_symbol_table();
    let mut inner_thonk = CThonk::new(format!("for_{}", ident));

    let get_integer = || -> RefType<ValueType> {
        let ty = Ty::new_integer(&mut s_read!(context.sarzak_heel()));
        for vt in lu_dog.iter_value_type() {
            if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                if ty.read().unwrap().id() == _ty {
                    return vt.clone();
                }
            }
        }
        unreachable!();
    };

    inner_thonk.increment_frame_size();
    let index = context.insert_symbol(ident, s_read!(get_integer()).clone());

    compile_expression(&body, &mut inner_thonk, context, body_span)?;
    let fp = inner_thonk.get_frame_size();
    for _ in 0..fp {
        thonk.increment_frame_size();
    }

    // Store the starting value
    thonk.add_instruction_with_span(Instruction::StoreLocal(index), span, location!());

    let top_of_loop = thonk.get_instruction_card() as isize;

    thonk.append(inner_thonk);

    // Duplicate the range end so that we can compare against it.
    thonk.add_instruction(Instruction::Dup, location!());

    // Increment the index
    thonk.add_instruction(Instruction::FetchLocal(index), location!());
    thonk.add_instruction(
        Instruction::Push(new_ref!(Value, Value::Integer(1))),
        location!(),
    );
    thonk.add_instruction(Instruction::Add, location!());
    thonk.add_instruction(Instruction::Dup, location!());
    thonk.add_instruction(Instruction::StoreLocal(index), location!());

    // Test the index against the length of the list
    thonk.add_instruction(Instruction::TestLessThanOrEqual, location!());

    // go do it again if index is < end.
    thonk.add_instruction(
        Instruction::JumpIfFalse(top_of_loop - thonk.get_instruction_card() as isize - 1),
        location!(),
    );

    context.pop_symbol_table();

    Ok(())
}
