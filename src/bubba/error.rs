use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};
use snafu::{prelude::*, Location};

use crate::{
    bubba::{instr::Instruction, value::Value},
    Span, ERR_CLR, OK_CLR, OTHER_CLR, POP_CLR,
};

#[derive(Debug, Snafu)]
pub enum BubbaError {
    #[snafu(display("\n{}: addition error: {} + {}", ERR_CLR.bold().paint("error"), left, right))]
    Addition { left: Value, right: Value },
    #[snafu(display("\n{}: negation error", ERR_CLR.bold().paint("error")))]
    Bang { value: Value },
    #[snafu(display("\n{}: could not convert `{}` to `{}`", ERR_CLR.bold().paint("error"), src, dst))]
    Conversion { src: String, dst: String },
    #[snafu(display("\n{}: division error: `{}` ÷ `{}`", ERR_CLR.bold().paint("error"), left, right))]
    Division { left: Value, right: Value },
    #[snafu(display("\n{}: Halt and catch fire...🔥", ERR_CLR.bold().paint("error")))]
    HaltAndCatchFire { file: String, span: Span, ip: usize },
    /// Index out of bounds
    ///
    #[snafu(display("\n{}: index `{}` is out of bounds for array of length `{}`.", ERR_CLR.bold().paint("error"), POP_CLR.paint(index.to_string()), POP_CLR.paint(len.to_string())))]
    IndexOutOfBounds {
        index: usize,
        len: usize,
        span: Span,
        location: Location,
    },
    #[snafu(display("\n{}: invalid instruction: {instr}", ERR_CLR.bold().paint("error")))]
    InvalidInstruction { instr: Instruction },
    #[snafu(display("\n{}: ip out of bounds at {ip}", ERR_CLR.bold().paint("error")))]
    IPOutOfBounds { ip: usize },
    #[snafu(display("\n{}: multiplication error: {} × {}", ERR_CLR.bold().paint("error"), left, right))]
    Multiplication { left: Value, right: Value },
    #[snafu(display("\n{}: negation error: {}", ERR_CLR.bold().paint("error"), value))]
    Negation { value: Value },
    #[snafu(display("\n{}: no such field: {} in {}", ERR_CLR.bold().paint("error"), field, ty))]
    NoSuchField { field: String, ty: String },
    #[snafu(display("\n{}: not indexable.", ERR_CLR.bold().paint("error")))]
    NotIndexable {
        span: Span,
        value: Value,
        location: Location,
    },
    #[snafu(display("\n{}: subtraction error: {} - {}", ERR_CLR.bold().paint("error"), left, right))]
    Subtraction { left: Value, right: Value },
    // #[snafu(display("\n{}: value error: {value}\n\t--> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    // ValueError { value: Value, location: Location },
    #[snafu(display("\n{}: vm panic: {message}\n\t--> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    VmPanic { message: String, location: Location },
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);

pub struct BubbaErrorReporter<'a, 'b, 'c>(pub &'a Error, pub bool, pub &'b str, pub &'c str);
impl fmt::Display for BubbaErrorReporter<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let is_uber = self.1;
        let program = &self.2;
        let file_name = &self.3;

        let mut std_err = Vec::new();

        match &self.0 .0 {
            BubbaError::HaltAndCatchFire { file, span, ip } => {
                Report::build(ReportKind::Error, file_name, span.start)
                    .with_message("halt and catch fire...🔥")
                    .with_label(
                        Label::new((file_name, span.to_owned()))
                            .with_message("So sorry".to_owned())
                            .with_color(Color::Red),
                    )
                    .with_note(format!("ip register: 0x{ip:08x}"))
                    .finish()
                    .write((file_name, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            BubbaError::IndexOutOfBounds {
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
            _ => write!(f, "{}", self.0),
        }
    }
}
