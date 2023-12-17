use std::path::PathBuf;

use crate::{
    bubba::{
        error::{Error, Result},
        instr::{Program, Thonk},
    },
    lu_dog::{Function, Statement},
    s_read,
    sarzak::ObjectStore as SarzakStore,
    Context as ExtruderContext, RefType,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

pub fn compile(e_context: &ExtruderContext, sarzak: &SarzakStore) -> Result<Program> {
    let lu_dog = e_context.lu_dog.clone();
    let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());

    for func in s_read!(lu_dog).iter_function() {
        program.add_thonk(compile_func(&func, e_context, sarzak)?);
    }

    Ok(program)
}

pub fn compile_func(
    func: &RefType<Function>,
    e_context: &ExtruderContext,
    sarzak: &SarzakStore,
) -> Result<Thonk> {
    let name = s_read!(func).name.clone();
    Ok(Thonk::new(name))
}

pub fn compile_statement(
    _statement: &RefType<Statement>,
    _e_context: &ExtruderContext,
    _sarzak: &SarzakStore,
) -> Result<()> {
    Ok(())
}

mod test {
    use std::env;

    use super::*;

    use crate::{
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    fn get_dwarf_home() -> PathBuf {
        env::var("DWARF_HOME")
            .unwrap_or_else(|_| {
                let mut home = env::var("HOME").unwrap();
                home.push_str("/.dwarf");
                home
            })
            .into()
    }

    #[test]
    fn empty_func() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {}";
        let ast = parse_dwarf("empty_func", ore).unwrap();
        let ctx = new_lu_dog(
            "empty_func".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx, &sarzak).unwrap();

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 0);
    }

    #[test]
    fn empty_funcs() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {}
                   fn foo() {}
                   fn bar() {}";
        let ast = parse_dwarf("empty_func", ore).unwrap();
        let ctx = new_lu_dog(
            "empty_func".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx, &sarzak).unwrap();

        assert_eq!(program.get_thonk_card(), 3);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 0);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 0);
        assert_eq!(program.get_thonk("bar").unwrap().get_instruction_card(), 0);
    }
}
