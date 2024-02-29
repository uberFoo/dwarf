use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};
use snafu::{prelude::*, Location};

use crate::{Span, ERR_CLR, OK_CLR, OTHER_CLR, POP_CLR};

#[derive(Clone, Debug, Snafu)]
pub struct Error(BubbaCompilerError);

#[derive(Clone, Debug, Snafu)]
pub enum BubbaCompilerError {
    #[snafu(display("\n{}: no such method `{}`.", ERR_CLR.bold().paint("error"), OTHER_CLR.paint(method)))]
    NoSuchMethod {
        method: String,
        span: Span,
        location: Location,
    },
    #[snafu(display("\n{}: `{message}`\n  --> {}::{}::{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    InternalCompilerError { location: Location, message: String },
    #[snafu(display("\n{}: variable `{}` not found.", ERR_CLR.bold().paint("error"), POP_CLR.paint(var)))]
    VariableNotFound {
        var: String,
        span: Span,
        location: Location,
    },
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub struct BubbaCompilerErrorReporter<'a, 'b, 'c>(
    pub &'a Error,
    pub bool,
    pub &'b str,
    pub &'c str,
);
impl fmt::Display for BubbaCompilerErrorReporter<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let is_uber = self.1;
        let program = &self.2;
        let file_name = &self.3;

        let mut std_err = Vec::new();

        match &self.0 .0 {
            BubbaCompilerError::InternalCompilerError { location, message } => {
                let report = Report::build(ReportKind::Error, file_name, Span::default().start)
                    .with_message("internal compiler error")
                    .with_label(
                        Label::new((file_name, Span::default()))
                            .with_message(format!(
                                "message: {}",
                                POP_CLR.paint(message.to_string())
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
            BubbaCompilerError::NoSuchMethod {
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
            BubbaCompilerError::VariableNotFound {
                var,
                span,
                location,
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
                        "{}:{}:{}\n",
                        OTHER_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
        }
    }
}
