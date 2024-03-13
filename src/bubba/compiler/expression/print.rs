use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ValueType,
    s_read, SarzakStorePtr, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(thonk, context)))]
pub(in crate::bubba::compiler) fn compile(
    print: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_print"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let print = lu_dog.exhume_x_print(print).unwrap();
    let expr = s_read!(print).r32_expression(&lu_dog)[0].clone();
    compile_expression(&expr, thonk, context)?;
    thonk.insert_instruction(Instruction::Out(0), location!());

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
    fn print_hello_world() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {
                       print(\"Hello, world!\");
                   }";
        let ast = parse_dwarf("print_hello_world", ore).unwrap();
        let ctx = new_lu_dog(
            "print_hello_world".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 4);

        run_vm(&program).unwrap();
    }

    #[test]
    fn format_string() {
        setup_logging();
        let ore = r#"
                   fn main() -> string {
                       let x = 42;
                       let y = "Hello";
                       let z = "world";
                       let α = `MOTD: ${y} ${z}!, the magic number is ${x}.`;
                       print(α);
                       α
                   }
                       "#;

        let mut ast = parse_dwarf("format_string_with_func_call", ore);
        while let Err(e) = ast {
            eprintln!("{}", e);
            ast = parse_dwarf("format_string_with_func_call", ore);
        }

        let ast = ast.unwrap();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ctx = new_lu_dog(
            "format_string".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_instruction_card(), 31);

        let run = run_vm(&program);
        assert!(run.is_ok());
        assert_eq!(
            &*s_read!(run.unwrap()),
            &Value::String("MOTD: Hello world!, the magic number is 42.".to_owned())
        );
    }

    #[test]
    fn format_string_with_func_call() {
        setup_logging();
        let ore = r#"
        struct Foo {}

        impl Foo {
            fn magic() -> int {
                42
            }
        }

                   fn main() -> string {
                    let x = Foo {};
                       let y = "Hello";
                       let z = "world";
                       let α = `MOTD: ${y} ${z}!, the magic number is ${x.magic()}.`;
                       print(α);
                        α
                   }
                       "#;

        let mut ast = parse_dwarf("format_string_with_func_call", ore);
        while let Err(e) = ast {
            eprintln!("{}", e);
            ast = parse_dwarf("format_string_with_func_call", ore);
        }

        let ast = ast.unwrap();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ctx = new_lu_dog(
            "format_string_with_func_call".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_instruction_card(), 38);

        let run = run_vm(&program);
        assert!(run.is_ok());
        assert_eq!(
            &*s_read!(run.unwrap()),
            &Value::String("MOTD: Hello world!, the magic number is 42.".to_owned())
        );
    }
}
