use std::{io, path::PathBuf};

use ansi_term::Colour;
use clap::Args;
use rustyline::error::ReadlineError;
use serde::{Deserialize, Serialize};
use snafu::{prelude::*, Location};

pub mod interpreter;
pub mod merlin;
pub(crate) mod value;

pub use interpreter::{initialize_interpreter, start_repl, Stack};
pub use value::{StoreProxy, Value};

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
// Errors
const ERR_CLR: Colour = Colour::Red;
const OK_CLR: Colour = Colour::Green;
const POP_CLR: Colour = Colour::Yellow;
const OTH_CLR: Colour = Colour::Cyan;

#[derive(Debug, Snafu)]
pub struct Error(InnerError);

#[derive(Debug, Snafu)]
pub enum InnerError {
    #[snafu(display("\n{}: could not convent `{}` to `{}`", ERR_CLR.paint("error"), src, dst))]
    Conversion {
        src: String,
        dst: String,
    },
    #[snafu(display("\n{}: could not find method `{}::{}`.", ERR_CLR.paint("error"), OTH_CLR.paint(ty), OTH_CLR.paint(method)))]
    NoSuchMethod {
        method: String,
        ty: String,
    },
    #[snafu(display("\n{}: could not find static method `{}::{}`.", ERR_CLR.paint("error"), OTH_CLR.paint(ty), OTH_CLR.paint(method)))]
    NoSuchStaticMethod {
        method: String,
        ty: String,
    },
    #[snafu(display("\n{}: type mismatch -- expected `{}`, found `{}`.", ERR_CLR.paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.paint(got.to_string())))]
    TypeMismatch {
        expected: String,
        got: String,
    },
    #[snafu(display("\n{}: no such field `{}`.", ERR_CLR.paint("error"), POP_CLR.paint(field)))]
    NoSuchField {
        field: String,
    },
    #[snafu(display("\n{}: {message}\n  --> {}:{}:{}", ERR_CLR.paint("error"), location.file, location.line, location.column))]
    Unimplemented {
        message: String,
        location: Location,
    },
    #[snafu(display("\n{}: wrong number of arguments. Expected `{}`, found `{}`.", ERR_CLR.paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.paint(got.to_string())))]
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
    },
    RustyLine {
        source: ReadlineError,
    },
    Store {
        source: io::Error,
    },
}

type Result<T, E = InnerError> = std::result::Result<T, E>;
