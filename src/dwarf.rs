use std::{fmt, io::Write, ops, path::PathBuf};

use ansi_term::Colour;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::Args;
use sarzak::{
    lu_dog::WoogStruct,
    sarzak::{store::ObjectStore as SarzakStore, types::Ty},
};
use serde::{Deserialize, Serialize};
use snafu::{prelude::*, Location};
use uuid::Uuid;

use crate::{
    lu_dog::{
        store::ObjectStore as LuDogStore,
        types::{ValueType, WoogOption},
        List, Reference,
    },
    s_read, NewRef, RefType,
};

pub mod extruder;
pub mod parser;

pub use extruder::{inter_statement, new_lu_dog};
pub use parser::{parse_dwarf, parse_line};

pub type Span = ops::Range<usize>;
pub type Spanned<T> = (T, Span);

// These should eventually come from the domain.
pub type DwarfInteger = i64;
pub type DwarfFloat = f64;

// Error handling
const C_ERR: Colour = Colour::Red;
const _C_OK: Colour = Colour::Green;
const C_WARN: Colour = Colour::Yellow;
const C_OTHER: Colour = Colour::Cyan;

#[derive(Args, Clone, Debug, Deserialize, Serialize)]
pub struct DwarfOptions {
    /// Dwarf Source File
    ///
    /// Path to the source file to compile.
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

pub type Result<T, E = DwarfError> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum DwarfError {
    /// Self Error
    ///
    /// The Self keyword is being used outside of an impl block.
    #[snafu(display("\n{}: `{}` may only be used inside an impl block.\n  --> {}..{}", C_ERR.bold().paint("error"), C_OTHER.underline().paint("Self"), span.start, span.end))]
    BadSelf { span: Span },

    /// File Error
    ///
    /// Something went wrong with the file system.
    #[snafu(display("\n{}: {description}: {}:{}:{}\n  --> {source} ({})", C_ERR.bold().paint("error"), location.file, location.line, location.column, path.display()))]
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
    GenericWarning { description: String, span: Span },

    /// Implementation Block Error
    ///
    /// An impl block may only contain functions.
    #[snafu(display("\n{}: impl blocks may only contain functions.\n  --> {}..{}", C_ERR.bold().paint("error"), span.start, span.end))]
    ImplementationBlock { span: Span },

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
        span: Span,
    },

    /// Struct Field Not Found Error
    ///
    /// This is used when a struct field is used and it's not found on the struct
    /// definition.
    #[snafu(display("\n{}: Struct field not found: {field}\n  --> {}:{}", C_ERR.bold().paint("error"), span.start, span.end))]
    StructFieldNotFound { field: String, span: Span },

    /// Object ID Lookup Error
    ///
    /// This is used when a reverse object lookup in one of the domains fails.
    #[snafu(display("\n{}: Object lookup failed for {id}", C_ERR.bold().paint("error")))]
    ObjectIdNotFound { id: Uuid },

    /// Object Name Lookup Error
    ///
    /// This is used when an object lookup in one of the domains fails.
    #[snafu(display("\n{}: Object lookup failed for {name}", C_ERR.bold().paint("error")))]
    ObjectNameNotFound { name: String },

    /// Parse Error
    ///
    /// Error parsing the source code.
    #[snafu(display("\n{}: parser completed with errors:\n  --> {error}", C_ERR.bold().paint("error")))]
    Parse {
        error: String,
        ast: Vec<Spanned<Item>>,
    },

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
        span: Span,
    },

    /// Unknown Type
    ///
    /// This is used when a type is not found in any domain.
    #[snafu(display("\n{}: Unknown type: {ty}", C_ERR.bold().paint("error")))]
    UnknownType { ty: String },
}

pub struct DwarfErrorReporter<'a, 'b>(pub &'a DwarfError, pub &'b str);
impl fmt::Display for DwarfErrorReporter<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let program = &self.1;
        let mut std_err = Vec::new();

        match &self.0 {
            DwarfError::BadSelf { span } | DwarfError::ImplementationBlock { span } => {
                let span = span.clone();
                let msg = format!("{}", self);

                Report::build(ReportKind::Error, (), span.start)
                    .with_message(&msg)
                    .with_label(
                        Label::new(span)
                            .with_message(format!("{}", msg.fg(Color::Red)))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write(Source::from(&program), &mut std_err)
                    .map_err(|e| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::GenericWarning {
                description: desc,
                span,
            } => {
                let span = span.clone();

                Report::build(ReportKind::Error, (), span.start)
                    .with_message(&desc)
                    .with_label(
                        Label::new(span)
                            .with_message(format!("{}", desc.fg(Color::Red)))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .write(Source::from(&program), &mut std_err)
                    .map_err(|e| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::TypeMismatch {
                expected,
                found,
                span,
            } => {
                let span = span.clone();
                let msg = format!(
                    "{}: Type mismatch: expected `{expected}`, found `{found}`.",
                    Colour::Red.bold().paint("error")
                );

                Report::build(ReportKind::Error, (), span.start)
                    .with_message(&msg)
                    .with_label(Label::new(span).with_message(msg).with_color(Color::Red))
                    .finish()
                    .write(Source::from(&program), &mut std_err)
                    .map_err(|e| fmt::Error)?;
                write!(f, "{}", String::from_utf8_lossy(&std_err))
            }
            DwarfError::Parse { error, ast } => {
                // What's up with both of these? Need to write a test and see
                // what looks good.
                std_err.write(format!("{}", error).as_bytes()).unwrap();
                std_err.write(format!("{}", self.0).as_bytes()).unwrap();

                for a in ast {
                    let msg = format!("{}", self.0);
                    let span = a.1.clone();

                    Report::build(ReportKind::Error, (), span.start)
                        .with_message(&msg)
                        .with_label(
                            Label::new(span)
                                .with_message(format!("{}", msg.fg(Color::Red)))
                                .with_color(Color::Red),
                        )
                        .finish()
                        .write(Source::from(&program), &mut std_err)
                        .map_err(|e| fmt::Error)?;
                    write!(f, "{}", String::from_utf8_lossy(&std_err))?;
                }
                Ok(())
            }
            _ => write!(f, "{}", self.0),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    As,
    Asm,
    Bool(bool),
    Debugger,
    Else,
    Empty,
    Float(String),
    Fn,
    For,
    // Global,
    Ident(String),
    If,
    Impl,
    Import,
    In,
    Integer(String),
    Let,
    None,
    Op(String),
    Option,
    Print,
    Punct(char),
    Return,
    Self_,
    SmallSelf,
    Some,
    String(String),
    Struct,
    Type(Type),
    Uuid,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::As => write!(f, "as"),
            Self::Asm => write!(f, "asm!"),
            Self::Bool(bool_) => write!(f, "{}", bool_),
            Self::Debugger => write!(f, "debugger"),
            Self::Else => write!(f, "else"),
            Self::Empty => write!(f, "()"),
            Self::Float(num) => write!(f, "{}", num),
            Self::Fn => write!(f, "fn"),
            Self::For => write!(f, "for"),
            // Self::Global => write!(f, "global"),
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::If => write!(f, "if"),
            Self::Impl => write!(f, "impl"),
            Self::Import => write!(f, "import"),
            Self::In => write!(f, "in"),
            Self::Integer(num) => write!(f, "{}", num),
            Self::Let => write!(f, "let"),
            Self::None => write!(f, "None"),
            Self::Op(op) => write!(f, "{}", op),
            Self::Option => write!(f, "Option"),
            Self::Print => write!(f, "print"),
            Self::Punct(punct) => write!(f, "{}", punct),
            Self::Return => write!(f, "return"),
            Self::Self_ => write!(f, "Self"),
            Self::SmallSelf => write!(f, "self"),
            Self::Some => write!(f, "Some"),
            Self::String(str_) => write!(f, "{}", str_),
            Self::Struct => write!(f, "struct"),
            Self::Type(type_) => write!(f, "{}", type_),
            Self::Uuid => write!(f, "Uuid"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Boolean,
    Empty,
    Float,
    Integer,
    List(Box<Spanned<Self>>),
    Option(Box<Spanned<Self>>),
    Reference(Box<Spanned<Self>>),
    Self_,
    String,
    Unknown,
    UserType(Spanned<String>),
    Uuid,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean => write!(f, "bool"),
            Self::Empty => write!(f, "()"),
            Self::Float => write!(f, "float"),
            Self::Integer => write!(f, "int"),
            Self::List(type_) => write!(f, "[{}]", type_.0),
            Self::Option(type_) => write!(f, "Option<{}>", type_.0),
            Self::Reference(type_) => write!(f, "&{}", type_.0),
            Self::Self_ => write!(f, "Self"),
            Self::String => write!(f, "string"),
            Self::Unknown => write!(f, "<unknown>"),
            Self::UserType(type_) => write!(f, "{}", type_.0),
            Self::Uuid => write!(f, "Uuid"),
        }
    }
}

impl Type {
    // 🚧 Should probably return a result
    pub fn into_value_type(
        &self,
        store: &mut LuDogStore,
        models: &[SarzakStore],
        sarzak: &SarzakStore,
    ) -> RefType<ValueType> {
        match self {
            Type::Boolean => {
                let ty = Ty::new_boolean();
                ValueType::new_ty(&<RefType<Ty> as NewRef<Ty>>::new_ref(ty), store)
            }
            Type::Empty => ValueType::new_empty(store),
            Type::Float => {
                let ty = Ty::new_float();
                ValueType::new_ty(&<RefType<Ty> as NewRef<Ty>>::new_ref(ty), store)
            }
            Type::Integer => {
                let ty = Ty::new_integer();
                ValueType::new_ty(&<RefType<Ty> as NewRef<Ty>>::new_ref(ty), store)
            }
            Type::List(type_) => {
                let ty = type_.0.into_value_type(store, models, sarzak);
                let list = List::new(&ty, store);
                ValueType::new_list(&list, store)
            }
            Type::Option(type_) => {
                let ty = type_.0.into_value_type(store, models, sarzak);
                let option = WoogOption::new_z_none(&ty, store);
                ValueType::new_woog_option(&option, store)
            }
            Type::Reference(type_) => {
                let ty = type_.0.into_value_type(store, models, sarzak);
                let reference = Reference::new(Uuid::new_v4(), false, &ty, store);
                ValueType::new_reference(&reference, store)
            }
            Type::Self_ => panic!("Self is deprecated."),
            Type::String => {
                let ty = Ty::new_s_string();
                ValueType::new_ty(&<RefType<Ty> as NewRef<Ty>>::new_ref(ty), store)
            }
            Type::Unknown => ValueType::new_unknown(store),
            Type::UserType(type_) => {
                let name = &type_.0;

                for model in models {
                    if let Some(obj_id) = model.exhume_object_id_by_name(name) {
                        let woog_struct = store
                            .iter_woog_struct()
                            .find(|ws| s_read!(ws).object == Some(obj_id))
                            .unwrap();
                        let woog_struct = s_read!(woog_struct);

                        return ValueType::new_woog_struct(
                            &<RefType<WoogStruct> as NewRef<WoogStruct>>::new_ref(
                                woog_struct.to_owned(),
                            ),
                            store,
                        );
                    }
                }

                // If it's not in one of the models, it must be in sarzak.
                if let Some(obj_id) = sarzak.exhume_object_id_by_name(name) {
                    let ty = sarzak.exhume_ty(&obj_id).unwrap();
                    ValueType::new_ty(&<RefType<Ty> as NewRef<Ty>>::new_ref(ty.to_owned()), store)
                } else {
                    panic!("Couldn't find type `{}` in any model or sarzak.\nThis should be an error. 😢", name);
                }
            }
            Type::Uuid => {
                let ty = Ty::new_s_uuid();
                ValueType::new_ty(&<RefType<Ty> as NewRef<Ty>>::new_ref(ty), store)
            }
        }
    }
}

// impl From<(&Type, &mut LuDogStore, &SarzakStore)> for ValueType {
//     fn from((type_, store, model): (&Type, &mut LuDogStore, &SarzakStore)) -> Self {
//         match type_ {
//             Type::Boolean => {
//                 let ty = Ty::new_boolean();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Empty => ValueType::new_empty(),
//             Type::Float => {
//                 let ty = Ty::new_float();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Integer => {
//                 let ty = Ty::new_integer();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Option(type_) => {
//                 let ty = (&**type_, &store, &model).into();
//                 let option = WoogOption::new_none(&ty, store);
//                 ValueType::new_woog_option(&option, store)
//             }
//             Type::Self_(type_) => panic!("Self is deprecated."),
//             Type::String => {
//                 let ty = Ty::new_s_string();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::UserType(type_) => {
//                 let name = if let Token::Object(name) = &**type_ {
//                     name
//                 } else {
//                     panic!("Expected UserType to be Token::Object.")
//                 };
//                 let obj_id = model.exhume_object_id_by_name(&name).unwrap();
//                 let ty = model.exhume_ty(obj_id).unwrap();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Uuid => {
//                 let ty = Ty::new_s_uuid();
//                 ValueType::new_ty(&ty, store)
//             }
//         }
//     }
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Empty,
    Expression(Spanned<Expression>),
    Item(Spanned<Item>),
    Let(Spanned<String>, Option<Spanned<Type>>, Spanned<Expression>),
    Result(Spanned<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Addition(Box<Spanned<Self>>, Box<Spanned<Self>>),
    And(Box<Spanned<Self>>, Box<Spanned<Self>>),
    As(Box<Spanned<Self>>, Spanned<Type>),
    Asm(Vec<Spanned<Self>>),
    /// Assignment Expression
    ///
    /// E.g.: `a = b`
    ///
    /// The first element is the left-hand side expression representing the storage
    /// and the second is the right-hand side, representing the value to be stored.
    Assignment(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Bang(Box<Spanned<Self>>),
    Block(Vec<Spanned<Statement>>),
    BooleanLiteral(bool),
    Debug,
    Division(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Empty,
    Error,
    Equals(Box<Spanned<Self>>, Box<Spanned<Self>>),
    FieldAccess(Box<Spanned<Self>>, Spanned<String>),
    FloatLiteral(f64),
    For(Spanned<String>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    // The first element is the function being called, the second is the list of
    // arguments.
    FunctionCall(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    GreaterThan(Box<Spanned<Self>>, Box<Spanned<Self>>),
    GreaterThanOrEqual(Box<Spanned<Self>>, Box<Spanned<Self>>),
    If(
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Option<Box<Spanned<Self>>>,
    ),
    Index(Box<Spanned<Self>>, Box<Spanned<Self>>),
    IntegerLiteral(i64),
    LessThanOrEqual(Box<Spanned<Self>>, Box<Spanned<Self>>),
    List(Vec<Spanned<Self>>),
    LocalVariable(String),
    MethodCall(Box<Spanned<Self>>, Spanned<String>, Vec<Spanned<Self>>),
    Multiplication(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Negation(Box<Spanned<Self>>),
    None,
    Or(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Print(Box<Spanned<Self>>),
    Range(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Return(Box<Spanned<Self>>),
    Some(Box<Spanned<Self>>),
    /// Static Method Call
    ///
    /// E.g., `Foo::bar()`.
    ///
    StaticMethodCall(Type, Spanned<String>, Vec<Spanned<Self>>),
    /// String Literal
    ///
    StringLiteral(String),
    /// Structure Expression
    ///
    /// Struct Explession, Vec<Field Name, Field Value>
    Struct(Box<Spanned<Self>>, Vec<(Spanned<String>, Spanned<Self>)>),
    Subtraction(Box<Spanned<Self>>, Box<Spanned<Self>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    /// A Function Definition
    ///
    /// name, Vec<(Parameter Name, Parameter Type)>, Return Type, Vec<Statement>
    Function(
        Spanned<String>,
        Vec<(Spanned<String>, Spanned<Type>)>,
        Spanned<Type>,
        Spanned<Expression>,
    ),
    /// name, Vec<(Function Name, Function)>
    Implementation(Spanned<String>, Vec<Spanned<Item>>),
    /// path, Option<Alias>
    Import(Spanned<Vec<Spanned<String>>>, Option<Spanned<String>>),
    /// name, Vec<(Field Name, Field Type)>
    Struct(Spanned<String>, Vec<(Spanned<String>, Spanned<Type>)>),
}
