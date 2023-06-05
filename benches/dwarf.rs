use std::{fs, path::PathBuf};

use chacha::{
    dwarf::{parse_dwarf, populate_lu_dog},
    initialize_interpreter,
    interpreter::{start_main, start_vm},
};
use criterion::{criterion_group, criterion_main, Criterion};
use sarzak::sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};
use tracy_client::Client;

const MANDEL_SOURCE_FILE: &str = "./benches/mandelbrot.tao";
const FIB_SOURCE_FILE: &str = "./benches/fib.tao";

fn mandelbrot(c: &mut Criterion) {
    let _client = Client::start();
    let source = fs::read_to_string(&MANDEL_SOURCE_FILE).unwrap();
    let ast = parse_dwarf(&source).unwrap();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let lu_dog = populate_lu_dog(None, source.clone(), &ast, &[], &sarzak).unwrap();

    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();

    c.bench_function("mandelbrot-7x2", |b| {
        b.iter(|| start_main(false, true, ctx.clone()).unwrap())
    });
}

fn fib(c: &mut Criterion) {
    let _client = Client::start();
    let source = fs::read_to_string(&FIB_SOURCE_FILE).unwrap();
    let ast = parse_dwarf(&source).unwrap();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let lu_dog = populate_lu_dog(None, source.clone(), &ast, &[], &sarzak).unwrap();

    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();

    c.bench_function("fib-17", |b| {
        b.iter(|| start_main(false, true, ctx.clone()).unwrap())
    });
}

fn vm_25(c: &mut Criterion) {
    let _client = Client::start();
    c.bench_function("vm-fib-25", |b| b.iter(|| start_vm(25.into()).unwrap()));
}

fn vm_17(c: &mut Criterion) {
    let _client = Client::start();
    c.bench_function("vm-fib-17", |b| b.iter(|| start_vm(17.into()).unwrap()));
}

criterion_group!(benches, mandelbrot, fib, vm_25, vm_17);
criterion_main!(benches);
