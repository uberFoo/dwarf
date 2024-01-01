#![allow(uncommon_codepoints)]
#![allow(mixed_script_confusables)]
#![allow(clippy::disallowed_names)]
use std::{ops, path::PathBuf};

use clap::Args;
use heck::ToUpperCamelCase;
use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};

pub mod bubba;
pub mod chacha;
pub mod dwarf;
pub mod plug_in;

#[cfg(all(
    feature = "tui",
    not(feature = "print-std-out"),
    not(any(feature = "single", feature = "single-vec", feature = "multi-nd-vec"))
))]
pub mod tui;
// pub(crate) mod woog_structs;
// pub mod lu_dog_proxy;

pub use ::sarzak::{lu_dog, sarzak};
pub use chacha::value::Value;
pub(crate) use chacha::{error::ChaChaError, interpreter};

// These should eventually come from the domain.
pub type DwarfInteger = i64;
pub type DwarfFloat = f64;

mod keywords {
    pub(crate) const ADD: &str = "add";
    pub(crate) const ARGS: &str = "args";
    #[cfg(feature = "async")]
    pub(crate) const ASLEEP: &str = "asleep";
    pub(crate) const ASSERT: &str = "assert";
    pub(crate) const ASSERT_EQ: &str = "assert_eq";
    pub(crate) const CHACHA: &str = "chacha";
    pub(crate) const COMPLEX_EX: &str = "ComplexEx";
    pub(crate) const EPS: &str = "eps";
    pub(crate) const EVAL: &str = "eval";
    pub(crate) const FN_NEW: &str = "new";
    #[cfg(feature = "async")]
    pub(crate) const HTTP_GET: &str = "http_get";
    #[cfg(feature = "async")]
    pub(crate) const INTERVAL: &str = "interval";
    pub(crate) const IS_DIGIT: &str = "is_digit";
    pub(crate) const LEN: &str = "len";
    pub(crate) const LINES: &str = "lines";
    pub(crate) const FORMAT: &str = "format";
    pub(crate) const MAP: &str = "map";
    pub(crate) const MAX: &str = "max";
    pub(crate) const NEW: &str = "new";
    pub(crate) const NORM_SQUARED: &str = "norm_squared";
    #[cfg(feature = "async")]
    pub(crate) const ONE_SHOT: &str = "one_shot";
    pub(crate) const PARSE: &str = "parse";
    pub(crate) const PLUGIN: &str = "Plugin";
    pub(crate) const SLEEP: &str = "sleep";
    #[cfg(feature = "async")]
    pub(crate) const SPAWN: &str = "spawn";
    #[cfg(feature = "async")]
    pub(crate) const SPAWN_NAMED: &str = "spawn_named";
    pub(crate) const SPLIT: &str = "split";
    pub(crate) const SUM: &str = "sum";
    pub(crate) const SQUARE: &str = "square";
    pub(crate) const TIME: &str = "time";
    // 🚧 Really this should be async only, but there's a nastiness in static_method_call.rs
    // that would make changing this a pain. That's on the todo list.
    // #[cfg(feature = "async")]
    pub(crate) const TIMER: &str = "timer";
    pub(crate) const TO_DIGIT: &str = "to_digit";
    pub(crate) const TRIM: &str = "trim";
    pub(crate) const TYPEOF: &str = "typeof";
    // 🚧 We have a token already...
    pub(crate) const UUID_TYPE: &str = "Uuid";
}

pub(crate) const ROOT_LU_DOG: &str = "__root__";

use lu_dog::ObjectStore as LuDogStore;
use sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};

cfg_if::cfg_if! {
    if #[cfg(feature = "single-vec")] {
        type SarzakStorePtr = usize;
        type RcType<T> = std::rc::Rc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::rc::Rc::new(value)
            }
        }

        pub type RefType<T> = std::rc::Rc<std::cell::RefCell<T>>;

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

        #[macro_export]
        macro_rules! ref_to_inner {
            ($arg:expr) => {
                $arg.into_inner().unwrap()
            };
        }
    } else if #[cfg(feature = "single-vec-tracy")] {
        type SarzakStorePtr = usize;
        type RcType<T> = std::rc::Rc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::rc::Rc::new(value)
            }
        }

        pub type RefType<T> = std::rc::Rc<std::cell::RefCell<T>>;

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

        #[macro_export]
        macro_rules! ref_to_inner {
            ($arg:expr) => {
                $arg.into_inner().unwrap()
            };
        }

   } else if #[cfg(feature = "multi-vec")] {
        type SarzakStorePtr = usize;
        type RcType<T> = std::sync::Arc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::sync::Arc::new(value)
            }
        }

        pub type RefType<T> = std::sync::Arc<std::sync::RwLock<T>>;
        impl<T> NewRef<T> for RefType<T> {
            fn new_ref(value: T) -> RefType<T> {
                std::sync::Arc::new(std::sync::RwLock::new(value))
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

        #[macro_export]
        macro_rules! ref_to_inner {
            ($arg:expr) => {
                $arg.into_inner().unwrap()
            };
        }

    } else if #[cfg(feature = "multi-nd-vec")] {
        type SarzakStorePtr = usize;
        type RcType<T> = std::sync::Arc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::sync::Arc::new(value)
            }
        }

        pub type RefType<T> = std::sync::Arc<no_deadlocks::RwLock<T>>;
        impl<T> NewRef<T> for RefType<T> {
            fn new_ref(value: T) -> RefType<T> {
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

        #[macro_export]
        macro_rules! ref_to_inner {
            ($arg:expr) => {
                $arg.into_inner()
            };
        }
    }
}

pub use ref_read as s_read;
pub use ref_write as s_write;

trait NewRcType<T> {
    fn new_rc_type(value: T) -> RcType<T>;
}

pub trait NewRef<T> {
    fn new_ref(value: T) -> RefType<T>;
}

#[allow(unused_macros)]
macro_rules! new_rc {
    ($type:ty, $value:expr) => {
        <RcType<$type> as NewRcType<$type>>::new_rc_type($value)
    };
}
#[allow(unused_imports)]
pub(crate) use new_rc;

#[macro_export]
macro_rules! new_ref {
    ($type:ty, $value:expr) => {
        // std::rc::Rc::new(std::cell::RefCell::new($value))
        <RefType<$type> as NewRef<$type>>::new_ref($value)
    };
}

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

pub(crate) type Span = ops::Range<usize>;
pub(crate) type ModelStore =
    HashMap<String, (sarzak::ObjectStore, Option<RefType<plug_in::PluginType>>)>;

/// Things that need updating.
///
/// This type is used to signify that a struct, enum, or ObjectStore have been
/// added in the extruder. The information is picked up by the interpreter and
/// used to update the corresponding structures in the interpreter.
#[derive(Clone, Debug)]
pub enum Dirty {
    Enum(RefType<lu_dog::Enumeration>),
    Func(RefType<lu_dog::Function>),
    Store(SarzakStorePtr),
    Struct(RefType<lu_dog::WoogStruct>),
}

#[derive(Clone, Debug)]
pub struct Context {
    /// The path to the source.
    pub source: String,
    /// This is the compiled source code.
    pub lu_dog: HashMap<String, RefType<LuDogStore>>,
    /// These are the plugins that represent imported domains.
    pub models: ModelStore,
    /// This contains things that the extruder added, that the interpreter
    /// needs to know about.
    pub dirty: Vec<Dirty>,
    /// This is a reference to the sarzak store.
    pub sarzak: RefType<SarzakStore>,
}

impl Context {
    pub fn source(&self) -> String {
        let lu_dog = self.lu_dog.get("").unwrap();
        let source = s_read!(lu_dog).iter_dwarf_source_file().next().unwrap();
        let source = s_read!(source);
        source.source.clone()
    }

    pub fn add_lu_dog(&mut self, name: String, lu_dog: RefType<LuDogStore>) {
        self.lu_dog.insert(name, lu_dog);
    }
}

impl Default for Context {
    fn default() -> Self {
        let mut lu_dog = HashMap::default();
        lu_dog.insert(ROOT_LU_DOG.into(), new_ref!(LuDogStore, LuDogStore::new()));

        Self {
            source: "unknown".into(),
            lu_dog,
            models: HashMap::default(),
            dirty: Vec::default(),
            sarzak: new_ref!(
                SarzakStore,
                SarzakStore::from_bincode(SARZAK_MODEL).unwrap()
            ),
        }
    }
}

pub type ValueResult = Result<RefType<Value>, ChaChaError>;

pub(crate) trait Desanitize {
    fn desanitize(&self) -> String;
}

impl Desanitize for &str {
    fn desanitize(&self) -> String {
        let result = match *self {
            "a_sink" => "async".to_owned(),
            "a_wait" => "await".to_owned(),
            "AWait" => "Await".to_owned(),
            "x_box" => "box".to_owned(),
            "XBox" => "Box".to_owned(),
            "x_break" => "break".to_owned(),
            "krate" => "crate".to_owned(),
            "Krate" => "Crate".to_owned(),
            "woog_const" => "const".to_owned(),
            "WoogConst" => "Const".to_owned(),
            "woog_enum" => "enum".to_owned(),
            "WoogEnum" => "Enum".to_owned(),
            "x_error" => "error".to_owned(),
            "XError" => "Error".to_owned(),
            "false_literal" => "false".to_owned(),
            "FalseLiteral" => "False".to_owned(),
            "x_future" => "future".to_owned(),
            "XFuture" => "Future".to_owned(),
            "x_if" => "if".to_owned(),
            "XIf" => "If".to_owned(),
            "x_let" => "let".to_owned(),
            "XLet" => "Let".to_owned(),
            "x_macro" => "macro".to_owned(),
            "XMacro" => "Macro".to_owned(),
            "x_match" => "match".to_owned(),
            "XMatch" => "Match".to_owned(),
            "x_model" => "model".to_owned(),
            "XModel" => "Model".to_owned(),
            "ZNone" => "None".to_owned(),
            "ZObjectStore" => "Object Store".to_owned(),
            "woog_option" => "option".to_owned(),
            "WoogOption" => "Option".to_owned(),
            "x_path" => "path".to_owned(),
            "XPath" => "Path".to_owned(),
            "x_plugin" => "plugin".to_owned(),
            "XPlugin" => "Plugin".to_owned(),
            "x_print" => "print".to_owned(),
            "XPrint" => "Print".to_owned(),
            "x_ref" => "ref".to_owned(),
            "x_return" => "return".to_owned(),
            "XReturn" => "Return".to_owned(),
            "ZSome" => "Some".to_owned(),
            "z_string" => "string".to_owned(),
            "ZString" => "String".to_owned(),
            "woog_struct" => "struct".to_owned(),
            "WoogStruct" => "Struct".to_owned(),
            "z_super" => "super".to_owned(),
            "ZSuper" => "Super".to_owned(),
            "true_literal" => "true".to_owned(),
            "TrueLiteral" => "True".to_owned(),
            "ty" => "type".to_owned(),
            "Ty" => "Type".to_owned(),
            "z_uuid" => "uuid".to_owned(),
            "ZUuid" => "Uuid".to_owned(),
            // "z_uuid" => "UUID".to_owned(),
            "x_value" => "value".to_owned(),
            "XValue" => "Value".to_owned(),
            _ => self.to_upper_camel_case(),
        };

        if self != &result {
            log::debug!("sanitized: {} -> {}", self, result);
        }
        result
    }
}

impl Desanitize for String {
    fn desanitize(&self) -> String {
        self.as_str().desanitize()
    }
}
