use std::{
    fmt, io,
    ops::{self, Range},
    path::PathBuf,
};

use ansi_term::Colour;
use ariadne::{Color, Label, Report, ReportKind, Source};
use chacha::vm::Instruction;
use clap::Args;
use crossbeam::channel::SendError;
#[cfg(feature = "repl")]
use rustyline::error::ReadlineError;
use serde::{Deserialize, Serialize};
use snafu::{prelude::*, Location};

pub mod chacha;
pub mod dwarf;
#[cfg(all(
    not(feature = "print-std-out"),
    not(any(feature = "single", feature = "single-vec"))
))]
pub mod tui;
pub(crate) mod value;
// pub mod merlin;
// pub(crate) mod woog_structs;

pub use ::sarzak::{lu_dog, sarzak};
pub use chacha::interpreter::{self, initialize_interpreter};
pub use value::{StoreProxy, Value};

// These should eventually come from the domain.
pub type DwarfInteger = i64;
pub type DwarfFloat = f64;

use lu_dog::ValueType;

cfg_if::cfg_if! {
    if #[cfg(feature = "single")] {
        type RcType<T> = std::rc::Rc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::rc::Rc::new(value)
            }
        }

        type RefType<T> = std::rc::Rc<std::cell::RefCell<T>>;

        impl<T> NewRef<T> for RefType<T> {
            fn new_ref(value: T) -> RefType<T> {
                std::rc::Rc::new(std::cell::RefCell::new(value))
            }
        }

        // Macros to abstract the underlying read/write operations.
        #[macro_export]
        macro_rules! ref_read {
            ($arg:expr) => {
                $arg.borrow()
            };
        }

        #[macro_export]
        macro_rules! ref_write {
            ($arg:expr) => {
                $arg.borrow_mut()
            };
        }

    } else if #[cfg(feature = "single-vec")] {
        type RcType<T> = std::rc::Rc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::rc::Rc::new(value)
            }
        }

        type RefType<T> = std::rc::Rc<std::cell::RefCell<T>>;

        impl<T> NewRef<T> for RefType<T> {
            fn new_ref(value: T) -> RefType<T> {
                std::rc::Rc::new(std::cell::RefCell::new(value))
            }
        }

        // Macros to abstract the underlying read/write operations.
        #[macro_export]
        macro_rules! ref_read {
            ($arg:expr) => {
                $arg.borrow()
            };
        }

        #[macro_export]
        macro_rules! ref_write {
            ($arg:expr) => {
                $arg.borrow_mut()
            };
        }

    } else if #[cfg(feature = "multi-std-mutex")] {
        compile_error!("std mutex is not currently supported");

        type RefType<T> = std::sync::Arc<std::sync::Mutex<T>>;

        impl<T> NewRef<T> for RefType<T> {
            fn new_ref_type(value: T) -> RefType<T> {
                std::sync::Arc::new(std::sync::Mutex::new(value))
            }
        }

        // Macros to abstract the underlying read/write operations.
        #[macro_export]
        macro_rules! ref_read {
            ($arg:expr) => {
                $arg.lock().unwrap()
            };
        }

        #[macro_export]
        macro_rules! ref_write {
            ($arg:expr) => {
                $arg.lock().unwrap()
            };
        }

   } else if #[cfg(feature = "multi-std-rwlock")] {
        type RcType<T> = std::sync::Arc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::sync::Arc::new(value)
            }
        }

        // type RefType<T> = std::sync::Arc<std::sync::RwLock<T>>;
        type RefType<T> = std::sync::Arc<no_deadlocks::RwLock<T>>;
        impl<T> NewRef<T> for RefType<T> {
            fn new_ref(value: T) -> RefType<T> {
                // std::sync::Arc::new(std::sync::RwLock::new(value))
                std::sync::Arc::new(no_deadlocks::RwLock::new(value))
            }
        }

        // Macros to abstract the underlying read/write operations.
        #[macro_export]
        macro_rules! ref_read {
            ($arg:expr) => {
                $arg.read().unwrap()
            };
        }

        #[macro_export]
        macro_rules! ref_write {
            ($arg:expr) => {
                $arg.write().unwrap()
            };
        }

    } else if #[cfg(feature = "multi-parking-lot-mutex")] {
        // compile_error!("parking lot mutex is not currently supported");
        type RcType<T> = std::sync::Arc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::sync::Arc::new(value)
            }
        }

        type RefType<T> = std::sync::Arc<parking_lot::Mutex<T>>;

        impl<T> NewRef<T> for RefType<T> {
            fn new_ref(value: T) -> RefType<T> {
                std::sync::Arc::new(parking_lot::Mutex::new(value))
            }
        }

        // Macros to abstract the underlying read/write operations.
        #[macro_export]
        macro_rules! ref_read {
            ($arg:expr) => {
                $arg.lock()
            };
        }

        #[macro_export]
        macro_rules! ref_write {
            ($arg:expr) => {
                $arg.lock()
            };
        }

    } else if #[cfg(feature = "multi-parking-lot-rwlock")] {
        type RefType<T> = std::sync::Arc<parking_lot::RwLock<T>>;

        impl<T> NewRef<T> for RefType<T> {
            fn new_ref_type(value: T) -> RefType<T> {
                std::sync::Arc::new(parking_lot::RwLock::new(value))
            }
        }

        // Macros to abstract the underlying read/write operations.
        #[macro_export]
        macro_rules! ref_read {
            ($arg:expr) => {
                $arg.read()
            };
        }

        #[macro_export]
        macro_rules! ref_write {
            ($arg:expr) => {
                $arg.write()
            };
        }
    }
}

// This is ugly, but it's the only way I could find to get the macro to work.
pub(crate) use ref_read as s_read;
pub(crate) use ref_write as s_write;

trait NewRcType<T> {
    fn new_rc_type(value: T) -> RcType<T>;
}

trait NewRef<T> {
    fn new_ref(value: T) -> RefType<T>;
}

#[allow(unused_macros)]
macro_rules! new_rc {
    ($type:ty, $value:expr) => {
        <RcType<$type> as NewRcType<$type>>::new_rc_type($value)
    };
}

macro_rules! new_ref {
    ($type:ty, $value:expr) => {
        <RefType<$type> as NewRef<$type>>::new_ref($value)
    };
}
pub(crate) use new_ref;

macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        name.strip_suffix("::f").unwrap()
    }};
}
pub(crate) use function;

macro_rules! debug {
    ($target:literal, $($arg:tt)*) => {
        log::debug!(
            target: $target,
            "{}: {}\n  --> {}:{}:{}",
            Colour::Cyan.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
pub(crate) use debug;

#[allow(unused_macros)]
macro_rules! warning {
    ($target:literal, $($arg:tt)*) => {
        log::warn!(
            target: $target,
            "{}: {}\n  --> {}:{}:{}",
            Colour::Cyan.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
#[allow(unused_imports)]
pub(crate) use warning;

#[allow(unused_macros)]
macro_rules! error {
    ($target:literal, $($arg:tt)*) => {
        log::error!(
            target: $target,
            "{}: {}\n  --> {}:{}:{}",
            Colour::Red.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
#[allow(unused_imports)]
pub(crate) use error;

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

pub type Span = ops::Range<usize>;

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
    #[snafu(display("\n{}: assertion failed at {}.\n  --> Found `{}`, expected `{}`.\n", ERR_CLR.bold().paint("error"), POP_CLR.underline().paint(code), s_read!(found), s_read!(expected)))]
    Assertion {
        found: RefType<Value>,
        expected: RefType<Value>,
        code: String,
    },
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
    #[snafu(display("\n{}: error with input/output `{}`.", ERR_CLR.bold().paint("error"), OTH_CLR.paint(message)))]
    IO {
        message: String,
        source: std::io::Error,
    },
    #[snafu(display("\n{}: named item `main` found, but it is not a function.", ERR_CLR.bold().paint("error")))]
    MainIsNotAFunction,
    #[snafu(display("\n{}: `{}` is not a function.", ERR_CLR.bold().paint("error"), POP_CLR.paint(value.to_string())))]
    NotAFunction {
        value: Value,
        span: Span,
    },
    #[snafu(display("\n{}: not an instance", ERR_CLR.bold().paint("error")))]
    NotAnInstance,
    #[snafu(display("\n{}: `main` function not found.", ERR_CLR.bold().paint("error")))]
    NoMainFunction,
    #[snafu(display("\n{}: no such method `{}`.", ERR_CLR.bold().paint("error"), OTH_CLR.paint(method)))]
    NoSuchMethod {
        method: String,
        span: Span,
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
    #[snafu(display("\n{}: {message}\n  --> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    Unimplemented {
        message: String,
        location: Location,
    },
    #[snafu(display("\nThat was the last stack frame 🥞. Your secret value is {}.", OK_CLR.paint(s_read!(value).to_string())))]
    Return {
        value: RefType<Value>,
        ty: RefType<ValueType>,
    },
    #[snafu(display("\n{}: chacha was not built with repl support.\n", ERR_CLR.bold().paint("error")))]
    ReplNotEnabled,
    #[cfg(feature = "repl")]
    RustyLine {
        source: ReadlineError,
    },
    Store {
        source: io::Error,
    },
    #[snafu(display("\n{}: type mismatch -- expected `{}`, found `{}`", ERR_CLR.bold().paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.bold().paint(found.to_string())))]
    TypeMismatch {
        expected: String,
        found: String,
        // expected_span: Span,
        // found_span: Span,
        span: Span,
    },
    /// A Variable was not found
    ///
    /// While happily interpreting away, we ran into a variable that we could
    /// not resolve.
    #[snafu(display("\n{}: variable `{}` not found.", ERR_CLR.bold().paint("error"), POP_CLR.paint(var)))]
    VariableNotFound {
        var: String,
        span: Span,
    },
    #[snafu(display("\n{}: vm panic: {}", ERR_CLR.bold().paint("error"), OTH_CLR.paint(message)))]
    VmPanic {
        message: String,
    },
    #[snafu(display("\n{}: wrong number of arguments. Expected `{}`, found `{}`.", ERR_CLR.bold().paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.bold().paint(got.to_string())))]
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
        defn_span: Span,
        invocation_span: Span,
    },
}

type Result<T, E = ChaChaError> = std::result::Result<T, E>;

pub struct ChaChaErrorReporter<'a, 'b>(pub &'a Error, pub &'b str);
impl fmt::Display for ChaChaErrorReporter<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let program = &self.1;
        let mut std_err = Vec::new();

        match &self.0 .0 {
            ChaChaError::TypeMismatch {
                expected,
                found,
                // expected_span,
                // found_span,
                span,
            } => {
                let msg = format!(
                    "{}: Type mismatch: expected `{expected}`, found `{found}`.",
                    Colour::Red.bold().paint("error")
                );

                Report::build(ReportKind::Error, (), span.start)
                    .with_message(&msg)
                    .with_label(
                        Label::new(span.to_owned())
                            .with_message(format!("expected {}", POP_CLR.paint(expected)))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(span.to_owned())
                            .with_message(format!("found {}", POP_CLR.paint(found)))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write(Source::from(&program), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::NotAFunction { value, span } => {
                Report::build(ReportKind::Error, (), span.start)
                    .with_message("not a function")
                    .with_label(
                        Label::new(span.clone())
                            .with_message("found here")
                            .with_color(Color::Red),
                    )
                    .with_note(value.to_string())
                    .finish()
                    .write(Source::from(&program), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::VariableNotFound { var, span } => {
                Report::build(ReportKind::Error, (), span.start)
                    .with_message("variable not found")
                    .with_label(
                        Label::new(span.clone())
                            .with_message("found here")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write(Source::from(&program), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::WrongNumberOfArguments {
                expected,
                got,
                defn_span,
                invocation_span,
            } => {
                let msg = format!("expected `{expected}`, found `{got}`.");

                Report::build(ReportKind::Error, (), invocation_span.start)
                    .with_message("wrong number of arguments")
                    .with_label(
                        Label::new(defn_span.clone()).with_message("for function defined here"), // .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new(invocation_span.clone()).with_message(msg), // .with_color(Color::Red),
                    )
                    .finish()
                    .write(Source::from(&program), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            _ => write!(f, "{}", self.0),
        }
    }
}
