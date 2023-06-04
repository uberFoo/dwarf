use std::{fs, path::PathBuf};

use chacha::{
    dwarf::{parse_dwarf, populate_lu_dog},
    initialize_interpreter,
    interpreter::{initialize_interpreter_paths, start_main},
};
use criterion::{criterion_group, criterion_main, Criterion};
use sarzak::sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};
use tracy_client::Client;

const SOURCE_FILE: &str = "./benches/mandelbrot.tao";
const OBJECT_FILE: &str = "./benches/mandelbrot.道";

fn criterion_benchmark(c: &mut Criterion) {
    let _client = Client::start();
    let source = fs::read_to_string(&SOURCE_FILE).unwrap();
    let ast = parse_dwarf(&source).unwrap();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let lu_dog = populate_lu_dog(None, source.clone(), &ast, &[], &sarzak).unwrap();

    // let ctx = initialize_interpreter_paths(OBJECT_FILE).unwrap();
    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();

    c.bench_function("mandelbrot-7x2", |b| {
        b.iter(|| start_main(false, true, ctx.clone()).unwrap())
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
