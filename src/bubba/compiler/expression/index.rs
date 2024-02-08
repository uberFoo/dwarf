use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ValueType,
    s_read, SarzakStorePtr, Span, POP_CLR,
};

pub(in crate::bubba::compiler) fn compile(
    index: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_index"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let index = lu_dog.exhume_index(index).unwrap();
    let index = s_read!(index);
    let target = lu_dog.exhume_expression(&index.target).unwrap();
    let target_span = get_span(&target, &lu_dog);
    compile_expression(&target, thonk, context, target_span)?;

    let index_expr = lu_dog.exhume_expression(&index.index).unwrap();
    let index_expr_span = get_span(&index_expr, &lu_dog);
    compile_expression(&index_expr, thonk, context, index_expr_span)?;

    thonk.insert_instruction_with_span(Instruction::ListIndex, span, location!());

    Ok(None)
}

#[cfg(test)]
mod test {

    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm, setup_logging},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn index_into_list() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       let y = [];
                       let x = [1, 2, 3];
                       x[1]
                   }";
        let ast = parse_dwarf("index_into_list", ore).unwrap();
        let ctx = new_lu_dog(
            "index_into_list".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 13);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &2.into());
    }

    #[test]
    fn index_out_of_bounds() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       let x = [1, 2, 3];
                       x[3]
                   }";
        let ast = parse_dwarf("index_out_of_bounds", ore).unwrap();
        let ctx = new_lu_dog(
            "index_out_of_bounds".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{}", run_vm(&program).unwrap_err());
    }

    // #[test]
    fn index_into_string() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
                       let x = \"foo\";
                       x[1]
                   }";
        let ast = parse_dwarf("index_into_string", ore).unwrap();
        let ctx = new_lu_dog(
            "index_into_string".to_owned(),
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
        //     8
        // );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("o".to_owned())
        );
    }
}
