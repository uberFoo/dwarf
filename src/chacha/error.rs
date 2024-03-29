use std::{fmt, io};

use ariadne::{Color, Label, Report, ReportKind, Source};
use crossbeam::channel::SendError;
#[cfg(feature = "repl")]
use rustyline::error::ReadlineError;
use snafu::{prelude::*, Backtrace, Location};

use crate::{
    bubba::Instruction, lu_dog::ValueType, s_read, RefType, Span, Value, ERR_CLR, OK_CLR,
    OTHER_CLR, POP_CLR,
};

#[derive(Debug, Snafu)]
pub struct Error(pub(super) ChaChaError);
pub(super) type Result<T, E = ChaChaError> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum ChaChaError {
    Addition {
        left: Value,
        right: Value,
    },
    #[snafu(display("\n{}: assertion failed at {}.\n  --> Found `{}`, expected `{}`.\n", ERR_CLR.bold().paint("error"), POP_CLR.underline().paint(code), s_read!(found), s_read!(expected)))]
    AssertEqual {
        found: RefType<Value>,
        expected: RefType<Value>,
        code: String,
    },
    #[snafu(display("\n{}: assertion failed at {}.\n  --> Found `{}`.\n", ERR_CLR.bold().paint("error"), POP_CLR.underline().paint(code), s_read!(found)))]
    AssertTrue {
        found: RefType<Value>,
        code: String,
    },
    #[snafu(display("\n{}: async not supported.", ERR_CLR.bold().paint("error")))]
    AsyncNotSupported,
    #[snafu(display("\n{}: internal error: {message}\n  --> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    BadnessHappened {
        message: String,
        location: Location,
    },
    Bang {
        value: Value,
    },
    #[snafu(display("\n{}: could not convert `{}` to `{}`", ERR_CLR.bold().paint("error"), src, dst))]
    Conversion {
        src: String,
        dst: String,
    },
    Division {
        left: Value,
        right: Value,
    },
    #[snafu(display("\n{}: evaluation error", ERR_CLR.bold().paint("error")))]
    Eval {
        src: String,
        span: Span,
    },
    #[snafu(display("\n{}: ffi error: {}", ERR_CLR.bold().paint("error"), message))]
    FfiError {
        message: String,
    },
    /// Index out of bounds
    ///
    #[snafu(display("\n{}: index `{}` is out of bounds for array of length `{}`.", ERR_CLR.bold().paint("error"), POP_CLR.paint(index.to_string()), POP_CLR.paint(len.to_string())))]
    IndexOutOfBounds {
        index: usize,
        len: usize,
        span: Span,
        location: Location,
    },
    #[snafu(display("\n{}: internal error: {}", ERR_CLR.bold().paint("error"), message))]
    InternalCompilerChannel {
        source: SendError<String>,
        message: String,
    },
    #[snafu(display("\n{}: invalid instruction `{}`.", ERR_CLR.bold().paint("error"), OTHER_CLR.paint(instr.to_string())))]
    InvalidInstruction {
        instr: Instruction,
    },
    #[snafu(display("\n{}: error with input/output `{}`.", ERR_CLR.bold().paint("error"), OTHER_CLR.paint(message)))]
    IO {
        message: String,
        source: std::io::Error,
    },
    #[snafu(display("\n{}: named item `main` found, but it is not a function.", ERR_CLR.bold().paint("error")))]
    MainIsNotAFunction,
    #[snafu(display("\n{}: missing definition `{}` --> {}", ERR_CLR.bold().paint("error"), OTHER_CLR.paint(name), ERR_CLR.italic().paint("this should be caught by the extruder!")))]
    MissingDefinition {
        name: String,
        span: Span,
    },
    Multiplication {
        left: Value,
        right: Value,
    },
    Negation {
        value: Value,
    },
    #[snafu(display("\n{}: `{}` is not a function.", ERR_CLR.bold().paint("error"), POP_CLR.paint(value.to_string())))]
    NotAFunction {
        value: Value,
        span: Span,
        location: Location,
    },
    #[snafu(display("\n{}: not an instance", ERR_CLR.bold().paint("error")))]
    NotAnInstance,
    #[snafu(display("\n{}: not indexable.", ERR_CLR.bold().paint("error")))]
    NotIndexable {
        span: Span,
        location: Location,
    },
    #[snafu(display("\n{}: `main` function not found.", ERR_CLR.bold().paint("error")))]
    NoMainFunction,
    NoSuchField {
        field: String,
        ty: String,
    },
    #[snafu(display("\n{}: no such method `{}`.", ERR_CLR.bold().paint("error"), OTHER_CLR.paint(method)))]
    NoSuchMethod {
        method: String,
        span: Span,
        location: Location,
    },
    #[snafu(display("\n{}: could not find static method `{}::{}`.", ERR_CLR.bold().paint("error"), OTHER_CLR.paint(ty), OTHER_CLR.paint(method)))]
    NoSuchStaticMethod {
        method: String,
        ty: String,
        span: Span,
        location: Location,
    },
    #[snafu(display("\n{}: parse error: {}", ERR_CLR.bold().paint("error"), src))]
    Parse {
        src: String,
        span: Span,
    },
    #[snafu(display("\n{}: plugin error: {}", ERR_CLR.bold().paint("error"), message))]
    PluginError {
        message: String,
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
    Subtraction {
        left: Value,
        right: Value,
    },
    #[snafu(display("\n{}: type mismatch -- expected `{}`, found `{}`", ERR_CLR.bold().paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.bold().paint(found.to_string())))]
    TypeMismatch {
        expected: String,
        found: String,
        // expected_span: Span,
        // found_span: Span,
        span: Span,
        location: Location,
    },
    /// A Variable was not found
    ///
    /// While happily interpreting away, we ran into a variable that we could
    /// not resolve.
    #[snafu(display("\n{}: variable `{}` not found.", ERR_CLR.bold().paint("error"), POP_CLR.paint(var)))]
    VariableNotFound {
        var: String,
        span: Span,
        location: Location,
        backtrace: Backtrace,
    },
    #[snafu(display("\n{}: wrong number of arguments. Expected `{}`, found `{}`.", ERR_CLR.bold().paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.bold().paint(got.to_string())))]
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
        defn_span: Span,
        invocation_span: Span,
        location: Location,
    },
}

pub struct ChaChaErrorReporter<'a, 'b, 'c>(pub &'a Error, pub bool, pub &'b str, pub &'c str);
impl fmt::Display for ChaChaErrorReporter<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let is_uber = self.1;
        let program = &self.2;
        let file_name = &self.3;

        let mut std_err = Vec::new();

        match &self.0 .0 {
            ChaChaError::IndexOutOfBounds {
                index,
                len,
                span,
                location,
            } => {
                let mut note = format!(
                    "and the length of the array is {}",
                    POP_CLR.paint(format!("{len}"))
                );

                if is_uber {
                    note += &format!(
                        " --> {}:{}:{}",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    );
                }

                Report::build(ReportKind::Error, file_name, span.start)
                    .with_message("index out of bounds 💥")
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message(format!(
                                "the index is {}",
                                POP_CLR.paint(format!("{index}"))
                            ))
                            .with_color(Color::Red),
                    )
                    .with_note(note)
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::NoSuchMethod {
                method,
                span,
                location,
            } => {
                let report = Report::build(ReportKind::Error, file_name, span.start)
                    .with_message("no such method")
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message(format!(
                                "in this invocation: {}",
                                POP_CLR.paint(method.to_string())
                            ))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::NoSuchStaticMethod {
                method,
                ty,
                span,
                location,
            } => {
                let report = Report::build(ReportKind::Error, file_name, span.start)
                    .with_message("no such static method")
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message("in this invocation".to_string())
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    ))
                } else {
                    report.with_note(format!(
                        "{} does not have a static method named {}",
                        POP_CLR.paint(ty.to_string()),
                        POP_CLR.paint(method.to_string())
                    ))
                };

                report
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::NotAFunction {
                value,
                span,
                location,
            } => {
                let report = Report::build(ReportKind::Error, file_name, span.start)
                    .with_message(format!("{value} is not a function"))
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message("in this invocation")
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    ))
                } else {
                    report.with_note(value.to_string())
                };

                report
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::NotIndexable { span, location } => {
                let report = Report::build(ReportKind::Error, file_name, span.start)
                    .with_message("not indexable")
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message("in this expression")
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::TypeMismatch {
                expected,
                found,
                // expected_span,
                // found_span,
                span,
                location,
            } => {
                let msg = format!("Type mismatch: expected `{expected}`, found `{found}`.");

                let report = Report::build(ReportKind::Error, file_name, span.start)
                    .with_message(&msg)
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message(format!("expected {}", POP_CLR.paint(expected)))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message(format!("found {}", POP_CLR.paint(found)))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    ))
                } else if expected == found {
                    report
                        .with_note("The types have the same name, but they are two distinct types.")
                } else {
                    report
                };

                report
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::VariableNotFound {
                var,
                span,
                location,
                backtrace,
            } => {
                let report = Report::build(ReportKind::Error, file_name, span.start)
                    .with_message(format!("variable `{}` not found", POP_CLR.paint(var)))
                    .with_label(
                        Label::new((file_name, span.clone()))
                            .with_message("used here")
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}\n{}",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                        backtrace
                    ))
                } else if var == "assert_eq" || var == "time" || var == "eps" {
                    report.with_note(format!(
                        "This is a built-in function. Try adding `chacha::` before \
                         the name, e.g. `chacha::{}`.",
                        POP_CLR.paint(var)
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            ChaChaError::WrongNumberOfArguments {
                expected,
                got,
                defn_span,
                invocation_span,
                location,
            } => {
                let msg = format!("expected `{expected}`, found `{got}`.");

                let report = Report::build(ReportKind::Error, file_name, invocation_span.start)
                    .with_message("wrong number of arguments")
                    .with_label(
                        Label::new((file_name, defn_span.clone()))
                            .with_message("for function defined here")
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((file_name, invocation_span.clone()))
                            .with_message(msg)
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}\n",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            _ => write!(f, "{}", self.0),
        }
    }
}
