use std::{fmt, path::PathBuf};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use snafu::{prelude::*, Location};

use crate::{
    dwarf::{Item, Span},
    ERR_CLR, OK_CLR, OTH_CLR, POP_CLR,
};

pub type Result<T, E = Vec<DwarfError>> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum DwarfError {
    /// Await non-future
    ///
    AwaitNotFuture {
        file: String,
        found: String,
        span: Span,
        program: String,
    },
    /// Self Error
    ///
    /// The Self keyword is being used outside of an impl block.
    #[snafu(display("\n{}: `{}` may only be used inside an impl block.\n  --> {}..{}", ERR_CLR.bold().paint("error"), OTH_CLR.underline().paint("Self"), span.start, span.end))]
    BadSelf {
        file: String,
        span: Span,
        location: Location,
        program: String,
    },

    /// Enum not found
    ///
    /// An enum has been referenced that does not exist.
    #[snafu(display("\n{}: enum not found: {}", ERR_CLR.bold().paint("error"), OTH_CLR.paint(name)))]
    EnumNotFound {
        name: String,
        file: String,
        span: Span,
        program: String,
    },

    /// File Error
    ///
    /// Something went wrong with the file system.
    #[snafu(display("\n{}: {description}\n{}:{}:{}\n  --> {source} ({})", ERR_CLR.bold().paint("error"), location.file, location.line, location.column, path.display()))]
    File {
        location: Location,
        description: String,
        path: PathBuf,
        source: std::io::Error,
    },

    /// Generic Error
    ///
    /// This non-specific error is a catch-all error type.
    #[snafu(display("\n{}: {description}", ERR_CLR.bold().paint("error")))]
    Generic { description: String },

    /// Generic Warning
    ///
    /// This non-specific error is a catch-all warning type.
    #[snafu(display("\n{}: {description}\n  --> {}..{}", POP_CLR.bold().paint("warning"), span.start, span.end))]
    GenericWarning {
        description: String,
        file: String,
        span: Span,
        program: String,
    },

    /// Implementation Block Error
    ///
    /// An impl block may only contain functions.
    #[snafu(display("impl blocks may only contain functions."))]
    ImplementationBlock {
        file: String,
        span: Span,
        program: String,
    },

    /// Internal Error
    ///
    /// This is an unrecoverable internal error.
    #[snafu(display("\n{}: unrecoverable internal error\n  -->{description}\n  --> {}:{}:{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    Internal {
        description: String,
        location: Location,
    },

    /// IO Related Error
    ///
    /// This is used to wrag std::io::Error into the DwarfError type.
    #[snafu(display("\n{}: {description}: {}:{}:{}\n  --> {source}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    IO {
        source: std::io::Error,
        description: String,
        location: Location,
    },

    /// Missing Function Definition
    ///
    /// A function is being called, but it's not defined.
    #[snafu(display("\n{}: Missing function definition\n  --> {}..{}", ERR_CLR.bold().paint("error"), span.start, span.end))]
    MissingFunctionDefinition {
        file: String,
        span: Span,
        program: String,
    },

    /// Missing Implementation
    ///
    /// This is just not done yet.
    #[snafu(display("\n{}: Missing implementation: {missing}\n  --> {}", POP_CLR.bold().paint("warning"), POP_CLR.underline().paint(code)))]
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
        program: String,
    },

    /// Not a Struct Type
    ///
    #[snafu(display("\n{}: Not a struct: {ty}", ERR_CLR.bold().paint("error")))]
    NotAStruct {
        ty: String,
        file: String,
        span: Span,
        program: String,
    },

    /// No Such Field
    #[snafu(display("\n{}: no such field `{}`.", ERR_CLR.bold().paint("error"), OTH_CLR.paint(field)))]
    NoSuchField {
        name: String,
        name_span: Span,
        field: String,
        file: String,
        span: Span,
        location: Location,
        program: String,
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
    #[snafu(display("\n{}: no such method `{}`.", ERR_CLR.bold().paint("error"), OTH_CLR.paint(method)))]
    NoSuchMethod {
        method: String,
        file: String,
        span: Span,
        location: Location,
        program: String,
    },

    /// Struct Field Not Found Error
    ///
    /// This is used when a struct field is used and it's not found on the struct
    /// definition.
    #[snafu(display("\n{}: Struct field not found: {field}\n  --> {}:{}", ERR_CLR.bold().paint("error"), span.start, span.end))]
    StructFieldNotFound {
        field: String,
        file: String,
        span: Span,
        location: Location,
        program: String,
    },

    /// Object Name Lookup Error
    ///
    /// This is used when an object lookup in one of the domains fails.
    #[snafu(display("\n{}: Object lookup failed for {name}", ERR_CLR.bold().paint("error")))]
    ObjectNameNotFound {
        name: String,
        file: String,
        span: Span,
        location: Location,
        program: String,
    },

    /// Parse Error
    ///
    /// Error parsing the source code.
    #[snafu(display("\n{}: parser completed with errors:\n  --> {error}", ERR_CLR.bold().paint("error")))]
    Parse { error: String, ast: Vec<Item> },

    /// Type Mismatch
    ///
    /// This is used when one type is expected, and another is found.
    ///
    /// I think that this doesn't implement Display because it's displayed
    /// someplace other than where error go? I'm not really sure, and it needs
    /// looking into.
    #[snafu(display("\n{}: Type mismatch: expected `{expected}`, found `{found}`.", ERR_CLR.bold().paint("error")))]
    TypeMismatch {
        expected: String,
        found: String,
        file: String,
        expected_span: Span,
        found_span: Span,
        location: Location,
        program: String,
    },

    /// Unknown Type
    ///
    /// This is used when a type is not found in any domain.
    #[snafu(display("\n{}: Unknown type: {ty}", ERR_CLR.bold().paint("error")))]
    UnknownType {
        ty: String,
        file: String,
        span: Span,
        location: Location,
        program: String,
    },
    #[snafu(display("\n{}: wrong number of arguments. Expected `{}`, found `{}`.", ERR_CLR.bold().paint("error"), OK_CLR.paint(expected.to_string()), ERR_CLR.bold().paint(found.to_string())))]
    WrongNumberOfArguments {
        expected: usize,
        found: usize,
        file: String,
        span: Span,
        location: Location,
        program: String,
    },
}

pub struct DwarfErrorReporter<'a>(pub &'a DwarfError, pub bool);
impl fmt::Display for DwarfErrorReporter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let is_uber = self.1;

        let mut std_err = Vec::new();

        match &self.0 {
            DwarfError::AwaitNotFuture {
                file,
                found,
                span,
                program,
            } => {
                let span = span.clone();
                Report::build(ReportKind::Error, file, span.start)
                    .with_message("await may only be used on a future")
                    .with_label(
                        Label::new((file, span))
                            .with_message(format!(
                                "expected a future and found {}",
                                OTH_CLR.paint(found)
                            ))
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
                program,
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
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
            DwarfError::EnumNotFound {
                name,
                file,
                span,
                program,
            } => {
                let span = span.clone();
                Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!("enum not found: {}", OTH_CLR.paint(name)))
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
                program,
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
            DwarfError::ImplementationBlock {
                file,
                span,
                program,
            } => {
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
            DwarfError::MissingFunctionDefinition {
                file,
                span,
                program,
            } => {
                Report::build(ReportKind::Error, file, span.start)
                    .with_message("missing function definition")
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("function called here is not found")
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
                program,
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
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
            DwarfError::NotAStruct {
                file,
                span,
                ty,
                program,
            } => {
                Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!("expected a struct, found {}", OTH_CLR.paint(ty)))
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
                location,
                program,
            } => {
                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!("no such field {}", OTH_CLR.paint(field)))
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("this field does not exist")
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((file, name_span.to_owned()))
                            .with_message("on this type")
                            .with_color(Color::Yellow),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
            DwarfError::NoSuchMethod {
                method,
                file,
                span,
                location,
                program,
            } => {
                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!(
                        "No such method: {}",
                        POP_CLR.paint(method.to_string())
                    ))
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("at or near this location")
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
                program,
            } => {
                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message(format!(
                        "Type not found: {}",
                        POP_CLR.paint(name.to_string())
                    ))
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message("in this location")
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
                program,
            } => {
                let msg = format!("Type mismatch: expected `{expected}`, found `{found}`.");

                let report = Report::build(ReportKind::Error, file, expected_span.start)
                    .with_message(&msg)
                    .with_label(
                        Label::new((file, expected_span.to_owned()))
                            .with_message(format!("expected {}", OTH_CLR.paint(expected)))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((file, found_span.to_owned()))
                            .with_message(format!("found {}", OTH_CLR.paint(found)))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTH_CLR.paint(location.file.to_string()),
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
                    .write((file, Source::from(&program)), &mut std_err)
                    .map_err(|_| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::StructFieldNotFound {
                field,
                file,
                span,
                location,
                program,
            } => {
                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message("struct field not found")
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message(format!("unknown field {}", OTH_CLR.paint(field)))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
                program,
            } => {
                let msg = format!("Unknown type: `{}`.", ty);

                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message(&msg)
                    .with_label(
                        Label::new((file, span.to_owned()))
                            .with_message(format!("unknown type {}", OTH_CLR.paint(ty)))
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
                    ))
                } else if ty.contains('8')
                    || ty.contains("16")
                    || ty.contains("32")
                    || ty.contains("64")
                    || ty.contains("128")
                {
                    if &ty[0..1] == "i" || &ty[0..1] == "u" {
                        report.with_note(format!("try `{}`", POP_CLR.paint("int")))
                    } else if &ty[0..1] == "f" {
                        report.with_note(format!("try `{}`", POP_CLR.paint("float")))
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
            DwarfError::WrongNumberOfArguments {
                expected,
                found,
                file,
                span,
                location,
                program,
            } => {
                let msg = format!("expected `{expected}`, found `{found}`.");

                let report = Report::build(ReportKind::Error, file, span.start)
                    .with_message("wrong number of arguments")
                    .with_label(
                        Label::new((file, span.clone()))
                            .with_message(msg)
                            .with_color(Color::Red),
                    );

                let report = if is_uber {
                    report.with_note(format!(
                        "{}:{}:{}",
                        OTH_CLR.paint(location.file.to_string()),
                        POP_CLR.paint(format!("{}", location.line)),
                        OK_CLR.paint(format!("{}", location.column)),
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
            _ => write!(f, "{}", self.0),
        }
    }
}
