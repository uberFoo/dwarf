use std::{
    io,
    ops::Range,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use clap::Args;
use crossbeam::channel::SendError;
use rustyline::error::ReadlineError;
use serde::{Deserialize, Serialize};
use snafu::{prelude::*, Location};
use svm::Instruction;

pub mod dap;
pub mod dwarf;
pub mod interpreter;
pub mod lu_dog;
pub mod merlin;
pub mod svm;
pub(crate) mod value;
pub(crate) mod woog_structs;

pub use interpreter::{initialize_interpreter, start_repl, Memory};
pub use value::{StoreProxy, Value};

// These should eventually come from the domain.
pub type DwarfInteger = i64;
pub type DwarfFloat = f64;

use lu_dog::ValueType;
//
// Command line parameters
#[derive(Args, Clone, Debug, Deserialize, Serialize)]
pub struct ChaChaOptions {
    /// Lu-Dog Source Store
    ///
    /// Path to the store.
    source: PathBuf,
    /// Model File
    ///
    /// Path to the model, corresponding to the source file, to build the
    /// Lu-Dog domain.
    model: PathBuf,
    /// Meta-Model File
    ///
    /// Path to the meta-model, sarzak.
    sarzak: PathBuf,
}

//
// Error handling
const ERR_CLR: Colour = Colour::Red;
const OK_CLR: Colour = Colour::Green;
const POP_CLR: Colour = Colour::Yellow;
const OTH_CLR: Colour = Colour::Cyan;

#[derive(Debug, Snafu)]
pub struct Error(ChaChaError);

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum ChaChaError {
    #[snafu(display("\n{}: internal error: {message}\n  --> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    BadJuJu {
        message: String,
        location: Location,
    },
    #[snafu(display("\n{}: could not convent `{}` to `{}`", ERR_CLR.bold().paint("error"), src, dst))]
    Conversion {
        src: String,
        dst: String,
    },
    #[snafu(display("\n{}: internal error: {}", ERR_CLR.bold().paint("error"), message))]
    InternalCompilerChannel {
        source: SendError<String>,
        message: String,
    },
    #[snafu(display("\n{}: invalid instruction `{}`.", ERR_CLR.bold().paint("error"), OTH_CLR.paint(instr.to_string())))]
    InvalidInstruction {
        instr: Instruction,
    },
    #[snafu(display("\n{}: named item `main` found, but it is not a function.", ERR_CLR.bold().paint("error")))]
    MainIsNotAFunction,
    #[snafu(display("\n{}: could not find method `{}::{}`.", ERR_CLR.bold().paint("error"), OTH_CLR.paint(ty), OTH_CLR.paint(method)))]
    NoSuchMethod {
        method: String,
        ty: String,
    },
    #[snafu(display("\n{}: could not find static method `{}::{}`.", ERR_CLR.bold().paint("error"), OTH_CLR.paint(ty), OTH_CLR.paint(method)))]
    NoSuchStaticMethod {
        method: String,
        ty: String,
    },
    #[snafu(display("\n{}: no such field `{}`.", ERR_CLR.bold().paint("error"), POP_CLR.paint(field)))]
    NoSuchField {
        field: String,
    },
    #[snafu(display("\n{}: not an instance", ERR_CLR.bold().paint("error")))]
    NotAnInstance,
    #[snafu(display("\n{}: {message}\n  --> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    Unimplemented {
        message: String,
        location: Location,
    },
    #[snafu(display("\nThat was the last stack frame 🥞. Your secret value is {}.", OK_CLR.paint(value.read().unwrap().to_string())))]
    Return {
        value: Arc<RwLock<Value>>,
        ty: Arc<RwLock<ValueType>>,
    },
    RustyLine {
        source: ReadlineError,
    },
    Store {
        source: io::Error,
    },
    #[snafu(display("\n{}: type mismatch -- expected `{}`, found `{}`\n  --> {}:{}", ERR_CLR.bold().paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.bold().paint(got.to_string()), span.start, span.end))]
    TypeMismatch {
        expected: String,
        got: String,
        span: Range<usize>,
    },
    #[snafu(display("\n{}: variable `{}` not found.", ERR_CLR.bold().paint("error"), POP_CLR.paint(var)))]
    VariableNotFound {
        var: String,
    },
    #[snafu(display("\n{}: vm panic: {}", ERR_CLR.bold().paint("error"), message = OTH_CLR.paint(message)))]
    VmPanic {
        message: String,
    },
    #[snafu(display("\n{}: wrong number of arguments. Expected `{}`, found `{}`.", ERR_CLR.bold().paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.bold().paint(got.to_string())))]
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
    },
}

type Result<T, E = ChaChaError> = std::result::Result<T, E>;
