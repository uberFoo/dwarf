use std::{env, fs, path::PathBuf};

use criterion::{criterion_group, criterion_main, Criterion};
use dwarf::{
    bubba::{compiler::compile, VM},
    dwarf::{new_lu_dog, parse_dwarf},
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};
#[cfg(feature = "tracy")]
use tracy_client::Client;

const FIB_SOURCE_FILE: &str = "./benches/fib.tao";
const LOOP_SOURCE_FILE: &str = "./benches/loop.ore";

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

    let lu_dog_ctx = new_lu_dog(
        "fib".to_owned(),
        Some((source, &ast)),
        &dwarf_home,
        &env::current_dir().unwrap(),
        false,
        &sarzak,
    )
    .unwrap();

    let Ok(program) = compile(&lu_dog_ctx, false) else {
        panic!("Failed to compile program");
    };

    let args = vec![
        std::sync::Arc::new(std::sync::RwLock::new("fib".into())),
        std::sync::Arc::new(std::sync::RwLock::new(17.into())),
    ];
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &args, &PathBuf::new(), num_cpus::get(), false);
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &args, &PathBuf::new());
    c.bench_function("fib-vm-17", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));

    let args = vec![
        std::sync::Arc::new(std::sync::RwLock::new("fib".into())),
        std::sync::Arc::new(std::sync::RwLock::new(28.into())),
    ];
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &args, &PathBuf::new(), num_cpus::get(), false);
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &args, &PathBuf::new());
    c.bench_function("fib-vm-28", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));

    let args = vec![
        std::sync::Arc::new(std::sync::RwLock::new("fib".into())),
        std::sync::Arc::new(std::sync::RwLock::new(5.into())),
    ];
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &args, &PathBuf::new(), num_cpus::get(), false);
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &args, &PathBuf::new());
    c.bench_function("fib-vm-5", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));
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
        &env::current_dir().unwrap(),
        false,
        &sarzak,
    )
    .unwrap();

    let Ok(program) = compile(&lu_dog_ctx, false) else {
        panic!("Failed to compile program");
    };
    #[cfg(feature = "async")]
    let mut vm = VM::new(&program, &[], &PathBuf::new(), num_cpus::get(), false);
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &[], &PathBuf::new());

    c.bench_function("loop-vm", |b| b.iter(|| vm.invoke("main", &[]).unwrap()));
}

// criterion_group!(benches, loop_, mandelbrot, fib, vm_28, vm_25, vm_17, vm_5);
// criterion_group!(benches, vm_25, vm_17, vm_5);
criterion_group!(benches, fib_vm, loop_vm);
criterion_main!(benches);
