use std::{env, fs, path::PathBuf};

use criterion::{criterion_group, criterion_main, Criterion};
use dwarf::{
    bubba::{compiler::compile, value::Value as BubbaValue, VM},
    chacha::interpreter::{initialize_interpreter, start_func},
    dwarf::{new_lu_dog, parse_dwarf},
    new_ref, NewRef, RefType, Value,
};
use sarzak::sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};
#[cfg(feature = "tracy")]
use tracy_client::Client;

const MANDEL_SOURCE_FILE: &str = "./benches/mandelbrot.tao";
const FIB_SOURCE_FILE: &str = "./benches/fib.tao";
const LOOP_SOURCE_FILE: &str = "./benches/loop.ore";

fn mandelbrot(c: &mut Criterion) {
    #[cfg(feature = "tracy")]
    Client::start();

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

    let ctx = new_lu_dog(
        "mandelbrot".to_owned(),
        Some((source, &ast)),
        &dwarf_home,
        &sarzak,
    )
    .unwrap();

    let ctx = initialize_interpreter(num_cpus::get(), dwarf_home, ctx).unwrap();

    c.bench_function("mandelbrot-14x4", |b| {
        b.iter(|| start_func("main", false, &mut ctx.clone()).unwrap())
    });
}

fn fib_vm(c: &mut Criterion) {
    #[cfg(feature = "tracy")]
    Client::start();

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

    let lu_dog_ctx =
        new_lu_dog("fib".to_owned(), Some((source, &ast)), &dwarf_home, &sarzak).unwrap();

    let Ok(program) = compile(&lu_dog_ctx) else {
        panic!("Failed to compile program");
    };

    let args = vec![
        new_ref!(BubbaValue, "fib".into()),
        new_ref!(BubbaValue, "17".into()),
    ];
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &args, &PathBuf::new(), num_cpus::get());
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &args, &PathBuf::new());
    c.bench_function("fib-vm-17", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));

    let args = vec![
        new_ref!(BubbaValue, "fib".into()),
        new_ref!(BubbaValue, "28".into()),
    ];
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &args, &PathBuf::new(), num_cpus::get());
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &args, &PathBuf::new());
    c.bench_function("fib-vm-28", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));

    let args = vec![
        new_ref!(BubbaValue, "fib".into()),
        new_ref!(BubbaValue, "5".into()),
    ];
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &args, &PathBuf::new(), num_cpus::get());
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &args, &PathBuf::new());
    c.bench_function("fib-vm-5", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));
}

fn fib(c: &mut Criterion) {
    #[cfg(feature = "tracy")]
    Client::start();

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

    let lu_dog_ctx =
        new_lu_dog("fib".to_owned(), Some((source, &ast)), &dwarf_home, &sarzak).unwrap();

    let mut ctx =
        initialize_interpreter(num_cpus::get(), dwarf_home.clone(), lu_dog_ctx.clone()).unwrap();
    ctx.add_args(vec!["fib".to_owned(), "17".to_owned()]);

    c.bench_function("fib-17", |b| {
        b.iter(|| start_func("main", false, &mut ctx.clone()).unwrap())
    });

    // let mut ctx = initialize_interpreter(num_cpus::get(), dwarf_home, lu_dog_ctx).unwrap();
    // ctx.add_args(vec!["fib".to_owned(), "28".to_owned()]);

    // c.bench_function("fib-28", |b| {
    //     b.iter(|| start_func("main", false, &mut ctx.clone()).unwrap())
    // });
}

fn loop_(c: &mut Criterion) {
    #[cfg(feature = "tracy")]
    Client::start();
    let _ = env_logger::builder().is_test(true).try_init();
    let source = fs::read_to_string(LOOP_SOURCE_FILE).unwrap();
    let ast = parse_dwarf("loop", &source).unwrap();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    let lu_dog_ctx = new_lu_dog(
        "loop".to_owned(),
        Some((source, &ast)),
        &dwarf_home,
        &sarzak,
    )
    .unwrap();

    let ctx = initialize_interpreter(num_cpus::get(), dwarf_home, lu_dog_ctx).unwrap();

    c.bench_function("loop", |b| {
        b.iter(|| start_func("main", false, &mut ctx.clone()).unwrap())
    });
}

fn loop_vm(c: &mut Criterion) {
    #[cfg(feature = "tracy")]
    Client::start();
    let _ = env_logger::builder().is_test(true).try_init();
    let source = fs::read_to_string(LOOP_SOURCE_FILE).unwrap();
    let ast = parse_dwarf("loop", &source).unwrap();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    let lu_dog_ctx = new_lu_dog(
        "loop".to_owned(),
        Some((source, &ast)),
        &dwarf_home,
        &sarzak,
    )
    .unwrap();

    let Ok(program) = compile(&lu_dog_ctx) else {
        panic!("Failed to compile program");
    };
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &[], &PathBuf::new(), num_cpus::get());
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &[], &PathBuf::new());

    c.bench_function("loop-vm", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));
}

// criterion_group!(benches, loop_, mandelbrot, fib, vm_28, vm_25, vm_17, vm_5);
// criterion_group!(benches, vm_25, vm_17, vm_5);
criterion_group!(benches, fib, fib_vm, loop_vm);
criterion_main!(benches);
