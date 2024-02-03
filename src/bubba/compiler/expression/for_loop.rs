use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, BubbaError, CThonk, Context, Result},
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
) -> Result<Option<ValueType>> {
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

    // Whatever comes of this we expect two values on the stack. The first is
    // the end of the range, and the second is the start of the range.
    // This works as-is for range. For a list we need to do some extra work,
    // below.
    let ty = compile_expression(&list, thonk, context, list_span.clone())?;

    context.push_scope();
    let mut inner_thonk = CThonk::new(format!("for_{}", ident));

    let get_integer = || -> RefType<ValueType> {
        let ty = Ty::new_integer(&s_read!(context.sarzak_heel()));
        for vt in lu_dog.iter_value_type() {
            if let ValueTypeEnum::Ty(_ty) = s_read!(vt).subtype {
                if ty.read().unwrap().id() == _ty {
                    return vt.clone();
                }
            }
        }
        unreachable!();
    };

    let index = match context.insert_symbol(ident, (*s_read!(get_integer())).clone()) {
        (true, index) => {
            inner_thonk.increment_frame_size();
            index
        }
        (false, index) => index,
    };

    compile_expression(&body, &mut inner_thonk, context, body_span)?;
    for _ in 0..inner_thonk.get_frame_size() {
        thonk.increment_frame_size();
    }

    // Store the starting value
    // Here's where that extra work comes in when iterating over a list.

    // This unwrap is really going to become unnecessary as I plan on having
    // compile_expression return just a Result. I'm just being lazy now.
    match ty.unwrap().subtype {
        ValueTypeEnum::List(_) => {
            thonk.add_instruction_with_span(
                Instruction::ListLength,
                list_span.clone(),
                location!(),
            );
            thonk.add_instruction_with_span(
                Instruction::Push(new_ref!(Value, Value::Integer(1))),
                list_span.clone(),
                location!(),
            );
            thonk.add_instruction_with_span(Instruction::Add, list_span.clone(), location!());
            thonk.add_instruction_with_span(
                Instruction::Push(new_ref!(Value, Value::Integer(0))),
                list_span,
                location!(),
            );
            thonk.add_instruction_with_span(Instruction::StoreLocal(index), span, location!());
        }
        ValueTypeEnum::Range(_) => {
            thonk.add_instruction_with_span(Instruction::StoreLocal(index), span, location!());
        }
        _ => {
            return Err(BubbaError::InternalCompilerError {
                message: "For loop expression is not a list".to_owned(),
                location: location!(),
            }
            .into())
        }
    }

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

    context.pop_scope();

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
    #[test]
    fn test_for_in_range() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 0;
                       for i in 0..10 {
                           x = x + i;
                       }
                       x
                   }";
        let ast = parse_dwarf("test_for_in_range", ore).unwrap();
        let ctx = new_lu_dog(
            "test_for_in_range".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            19
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }

    #[test]
    fn nested_for_loop() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 0;
                       for i in 0..10 {
                           for j in 0..10 {
                               x = x + i + j;
                           }
                       }
                       x
                   }";
        let ast = parse_dwarf("nested_for_loop", ore).unwrap();
        let ctx = new_lu_dog(
            "nested_for_loop".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            32
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(900));
    }

    #[test]
    fn for_loop_variable() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let max = 10;
                       let x = 0;
                       for i in 0..max {
                           x = x + i;
                       }
                       x
                   }";
        let ast = parse_dwarf("for_loop_variable", ore).unwrap();
        let ctx = new_lu_dog(
            "for_loop_variable".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            21
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }

    #[test]
    fn test_for_loop_iterator_param() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn add(x: int) -> int {
                       let accum = 0;
                       for i in 0..x {
                            accum = accum + i;
                       }
                       accum
                   }
                   fn main() -> int {
                    let x = 10;
                       add(x)
                   }";
        let ast = parse_dwarf("test_for_loop_iterator_param", ore).unwrap();
        let ctx = new_lu_dog(
            "test_for_loop_iterator_param".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 7);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }

    #[test]
    fn for_loop_iter_array() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 0;
                       let arr = [1, 2, 3, 4, 5];
                       for i in arr {
                           x = x + i;
                       }
                       x
                   }";
        let ast = parse_dwarf("for_loop_iter_array", ore).unwrap();
        let ctx = new_lu_dog(
            "for_loop_iter_array".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        // assert_eq!(
        //     program.get_thonk("main").unwrap().get_instruction_card(),
        //     26
        // );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(15));
    }
}
