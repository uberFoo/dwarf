use std::{fmt, path::PathBuf};

use ansi_term::Colour;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use snafu::{prelude::*, Location};

use crate::dwarf::{Item, Span};

// Error handling
const C_ERR: Colour = Colour::Red;
const C_OK: Colour = Colour::Green;
const C_WARN: Colour = Colour::Yellow;
const C_OTHER: Colour = Colour::Cyan;

pub type Result<T, E = Vec<DwarfError>> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum DwarfError {
    /// Await non-future
    ///
    AwaitNotFuture { file: String, span: Span },
    /// Self Error
    ///
    /// The Self keyword is being used outside of an impl block.
    #[snafu(display("\n{}: `{}` may only be used inside an impl block.\n  --> {}..{}", C_ERR.bold().paint("error"), C_OTHER.underline().paint("Self"), span.start, span.end))]
    BadSelf {
        file: String,
        span: Span,
        location: Location,
    },

    /// Enum not found
    ///
    /// An enum has been referenced that does not exist.
    #[snafu(display("\n{}: enum not found: {}", C_ERR.bold().paint("error"), C_OTHER.paint(name)))]
    EnumNotFound {
        name: String,
        file: String,
        span: Span,
    },

    /// File Error
    ///
    /// Something went wrong with the file system.
    #[snafu(display("\n{}: {description}\n{}:{}:{}\n  --> {source} ({})", C_ERR.bold().paint("error"), location.file, location.line, location.column, path.display()))]
    File {
        location: Location,
        description: String,
        path: PathBuf,
        source: std::io::Error,
    },

    /// Generic Error
    ///
    /// This non-specific error is a catch-all error type.
    #[snafu(display("\n{}: {description}", C_ERR.bold().paint("error")))]
    Generic { description: String },

    /// Generic Warning
    ///
    /// This non-specific error is a catch-all warning type.
    #[snafu(display("\n{}: {description}\n  --> {}..{}", C_WARN.bold().paint("warning"), span.start, span.end))]
    GenericWarning {
        description: String,
        file: String,
        span: Span,
    },

    /// Implementation Block Error
    ///
    /// An impl block may only contain functions.
    #[snafu(display("impl blocks may only contain functions."))]
    ImplementationBlock { file: String, span: Span },

    /// Internal Error
    ///
    /// This is an unrecoverable internal error.
    #[snafu(display("\n{}: unrecoverable internal error\n  -->{description}\n  --> {}:{}:{}", C_ERR.bold().paint("error"), location.file, location.line, location.column))]
    Internal {
        description: String,
        location: Location,
    },

    /// IO Related Error
    ///
    /// This is used to wrag std::io::Error into the DwarfError type.
    #[snafu(display("\n{}: {description}: {}:{}:{}\n  --> {source}", C_ERR.bold().paint("error"), location.file, location.line, location.column))]
    IO {
        source: std::io::Error,
        description: String,
        location: Location,
    },

    /// Missing Implementation
    ///
    /// This is just not done yet.
    #[snafu(display("\n{}: Missing implementation: {missing}\n  --> {}", C_WARN.bold().paint("warning"), C_WARN.underline().paint(code)))]
    NoImplementation {
        missing: String,
        code: String,
        file: String,
        span: Span,
    },

    /// Not a List Type
    ///
    /// List types are list/vec and string.
    NotAList {
        ty: String,
        file: String,
        span: Span,
        location: Location,
    },

    /// Not a Struct Type
    ///
    #[snafu(display("\n{}: Not a struct: {ty}", C_ERR.bold().paint("error")))]
    NotAStruct {
        ty: String,
        file: String,
        span: Span,
    },

    /// No Such Field
    #[snafu(display("\n{}: no such field `{}`.", C_ERR.bold().paint("error"), C_OTHER.paint(field)))]
    NoSuchField {
        name: String,
        name_span: Span,
        field: String,
        file: String,
        span: Span,
    },

    /// No Such Method
    ///
    /// The method being invoked does not exist. We can know this a priori for
    /// built in types like `string`. We should be able to make the same claim
    /// for user defined types. I don't think that we can know much about proxies.
    /// Not at our current state. Well, that's not exactly true. We generate
    /// UDTs for the proxies, so we have that information to go on. So basically
    /// we can protect the interface, which is wide open IIRC, by checking calls
    /// at extrusion time.
    #[snafu(display("\n{}: no such method `{}`.", C_ERR.bold().paint("error"), C_OTHER.paint(method)))]
    NoSuchMethod {
        method: String,
        file: String,
        span: Span,
        location: Location,
    },

    /// Struct Field Not Found Error
    ///
    /// This is used when a struct field is used and it's not found on the struct
    /// definition.
    #[snafu(display("\n{}: Struct field not found: {field}\n  --> {}:{}", C_ERR.bold().paint("error"), span.start, span.end))]
    StructFieldNotFound {
        field: String,
        file: String,
        span: Span,
        location: Location,
    },

    /// Object Name Lookup Error
    ///
    /// This is used when an object lookup in one of the domains fails.
    #[snafu(display("\n{}: Object lookup failed for {name}", C_ERR.bold().paint("error")))]
    ObjectNameNotFound {
        name: String,
        file: String,
        span: Span,
        location: Location,
    },

    /// Parse Error
    ///
    /// Error parsing the source code.
    #[snafu(display("\n{}: parser completed with errors:\n  --> {error}", C_ERR.bold().paint("error")))]
    Parse { error: String, ast: Vec<Item> },

    /// Type Mismatch
    ///
    /// This is used when one type is expected, and another is found.
    ///
    /// I think that this doesn't implement Display because it's displayed
    /// someplace other than where error go? I'm not really sure, and it needs
    /// looking into.
    TypeMismatch {
        expected: String,
        found: String,
        file: String,
        expected_span: Span,
        found_span: Span,
        location: Location,
    },

    /// Unknown Type
    ///
    /// This is used when a type is not found in any domain.
    #[snafu(display("\n{}: Unknown type: {ty}", C_ERR.bold().paint("error")))]
    UnknownType {
        ty: String,
        file: String,
        span: Span,
        location: Location,
    },
}

pub struct DwarfErrorReporter<'a, 'b>(pub &'a DwarfError, pub bool, pub &'b str);
impl fmt::Display for DwarfErrorReporter<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let is_uber = self.1;
        let program = &self.2;

        let mut std_err = Vec::new();

        match &self.0 {
            DwarfError::AwaitNotFuture { file, span } => {
                let span = span.clone();
                Report::build(ReportKind::Error, file, span.start)
                    .with_message("await may only be used on a future")
                    .with_label(
                        Label::new((file, span))
                            .with_message("this is not a future".to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::BadSelf {
                file,
                span,
                location,
            } => {
                let span = span.clone();
                let report = Report::build(ReportKind::Error, file, span.start)
                    // 🚧 Figure out some error numbering scheme and use one of
                    // the snafu magic methods to provide the value here.
                    //.with_code(&code)
                    .with_message("self may only be used inside of an implementation block")
                    .with_label(
                        Label::new((file, span))
                            .with_message("used here".to_string())
                            .with_color(Color::Red),
                    );
                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        C_OTHER.paint(location.file.to_string()),
                        C_WARN.paint(format!("{}", location.line)),
                        C_OK.paint(format!("{}", location.column)),
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::EnumNotFound { name, file, span } => {
                let span = span.clone();
                Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!("enum not found: {}", C_OTHER.paint(name)))
                    .with_label(
                        Label::new((file, span))
                            .with_message("this enum does not exist")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::GenericWarning {
                description: desc,
                file,
                span,
            } => {
                Report::build(ReportKind::Error, file, span.start)
                    .with_message(desc)
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message(format!("{}", desc.fg(Color::Red)))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::ImplementationBlock { file, span } => {
                let span = span.clone();
                Report::build(ReportKind::Error, file, span.start)
                    // 🚧 Figure out some error numbering scheme and use one of
                    // the snafu magic methods to provide the value here.
                    //.with_code(&code)
                    .with_message("implementation blocks may only contain functions")
                    .with_label(
                        Label::new((file, span))
                            .with_message("used here".to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::NotAList {
                file,
                span,
                ty,
                location,
            } => {
                let span = span.clone();
                let report = Report::build(ReportKind::Error, file, span.start)
                    // 🚧 Figure out some error numbering scheme and use one of
                    // the snafu magic methods to provide the value here.
                    //.with_code(&code)
                    .with_message("expected a list")
                    .with_label(
                        Label::new((file, span))
                            .with_message("used here".to_string())
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        C_OTHER.paint(location.file.to_string()),
                        C_WARN.paint(format!("{}", location.line)),
                        C_OK.paint(format!("{}", location.column)),
                    ))
                } else {
                    report.with_note(format!("Found {ty}"))
                };

                report
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::NotAStruct { file, span, ty } => {
                Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!("expected a struct, found {}", C_OTHER.paint(ty)))
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("this is not a struct")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::NoSuchField {
                name: _,
                name_span,
                field,
                file,
                span,
            } => {
                Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!("no such field {}", C_OTHER.paint(field)))
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("this field does not exist")
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((file, name_span.to_owned()))
                            .with_message("on this struct")
                            .with_color(Color::Yellow),
                    )
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::NoSuchMethod {
                method,
                file,
                span,
                location,
            } => {
                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!(
                        "No such method: {}",
                        C_WARN.paint(method.to_string())
                    ))
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("at or near this location")
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        C_OTHER.paint(location.file.to_string()),
                        C_WARN.paint(format!("{}", location.line)),
                        C_OK.paint(format!("{}", location.column)),
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::ObjectNameNotFound {
                name,
                file,
                span,
                location,
            } => {
                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!(
                        "Type not found: {}",
                        C_WARN.paint(name.to_string())
                    ))
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("in this location")
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        C_OTHER.paint(location.file.to_string()),
                        C_WARN.paint(format!("{}", location.line)),
                        C_OK.paint(format!("{}", location.column)),
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::TypeMismatch {
                expected,
                found,
                file,
                expected_span,
                found_span,
                location,
            } => {
                let msg = format!("Type mismatch: expected `{expected}`, found `{found}`.");

                let report = Report::build(ReportKind::Error, file, expected_span.start)
                    .with_message(&msg)
                    .with_label(
                        Label::new((file, expected_span.to_owned()))
                            .with_message(format!("expected {}", C_OTHER.paint(expected)))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((file, found_span.to_owned()))
                            .with_message(format!("found {}", C_OTHER.paint(found)))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        C_OTHER.paint(location.file.to_string()),
                        C_WARN.paint(format!("{}", location.line)),
                        C_OK.paint(format!("{}", location.column)),
                    ))
                } else if expected == found {
                    report
                        .with_note("The types have the same name, but they are two distinct types.")
                } else {
                    report
                };

                report
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::StructFieldNotFound {
                field,
                file,
                span,
                location,
            } => {
                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message("struct field not found")
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message(format!("unknown field {}", C_OTHER.paint(field)))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        C_OTHER.paint(location.file.to_string()),
                        C_WARN.paint(format!("{}", location.line)),
                        C_OK.paint(format!("{}", location.column)),
                    ))
                } else {
                    report
                };

                report
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::UnknownType {
                ty,
                file,
                span,
                location,
            } => {
                let msg = format!("Unknown type: `{}`.", ty);

                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message(&msg)
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message(format!("unknown type {}", C_OTHER.paint(ty)))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        C_OTHER.paint(location.file.to_string()),
                        C_WARN.paint(format!("{}", location.line)),
                        C_OK.paint(format!("{}", location.column)),
                    ))
                } else if ty.contains('8')
                    || ty.contains("16")
                    || ty.contains("32")
                    || ty.contains("64")
                    || ty.contains("128")
                {
                    if &ty[0..1] == "i" || &ty[0..1] == "u" {
                        report.with_note(format!("try `{}`", C_WARN.paint("int")))
                    } else if &ty[0..1] == "f" {
                        report.with_note(format!("try `{}`", C_WARN.paint("float")))
                    } else {
                        report
                    }
                } else {
                    report
                };

                report
                    .finish()
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            _ => write!(f, "{}", self.0),
        }
    }
}
