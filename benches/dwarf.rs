use std::{env, fs};

use criterion::{criterion_group, criterion_main, Criterion};
use dwarf::{
    chacha::interpreter::{initialize_interpreter, start_func, start_vm},
    dwarf::{new_lu_dog, parse_dwarf},
};
use sarzak::sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};
use tracy_client::Client;

const MANDEL_SOURCE_FILE: &str = "./benches/mandelbrot.tao";
const FIB_SOURCE_FILE: &str = "./benches/fib.tao";

fn mandelbrot(c: &mut Criterion) {
    let _client = Client::start();
    let source = fs::read_to_string(MANDEL_SOURCE_FILE).unwrap();
    let ast = parse_dwarf("mandelbrot", &source).unwrap();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    let ctx = new_lu_dog(Some((source, &ast)), &dwarf_home, &sarzak).unwrap();

    let ctx = initialize_interpreter(dwarf_home, ctx, sarzak).unwrap();

    c.bench_function("mandelbrot-14x4", |b| {
        b.iter(|| start_func("main", false, ctx.clone()).unwrap())
    });
}

fn fib(c: &mut Criterion) {
    let _client = Client::start();
    let source = fs::read_to_string(FIB_SOURCE_FILE).unwrap();
    let ast = parse_dwarf("fib", &source).unwrap();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    let ctx = new_lu_dog(Some((source, &ast)), &dwarf_home, &sarzak).unwrap();

    let mut ctx = initialize_interpreter(dwarf_home, ctx, sarzak).unwrap();
    ctx.add_args(vec!["fib".to_owned(), "17".to_owned()]);

    c.bench_function("fib-17", |b| {
        b.iter(|| start_func("main", false, ctx.clone()).unwrap())
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

fn vm_5(c: &mut Criterion) {
    let _client = Client::start();
    c.bench_function("vm-fib-5", |b| b.iter(|| start_vm(5.into()).unwrap()));
}

criterion_group!(benches, mandelbrot, fib, vm_25, vm_17, vm_5);
criterion_main!(benches);
