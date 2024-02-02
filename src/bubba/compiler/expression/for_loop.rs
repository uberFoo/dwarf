use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value,
};

pub(in crate::bubba::compiler) fn compile(
    for_loop: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<String>> {
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

    context.push_scope();
    let mut inner_thonk = CThonk::new(format!("for_{}", ident));

    let index = match context.insert_symbol(ident) {
        (true, index) => {
            inner_thonk.increment_frame_size();
            index
        }
        (false, index) => index,
    };

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
}
