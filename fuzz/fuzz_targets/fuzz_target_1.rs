#![no_main]
use std::path::PathBuf;

use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    if let Ok(program) = std::str::from_utf8(data) {
        if let Ok(ast) = parse_dwarf(&program) {
            if let Ok(lu_dog) = new_lu_dog(None, Some((program.to_owned(), &ast)), &[], &sarzak) {
                let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
                let _ = start_main(false, ctx);
            }
        }
    }
});
