#![allow(uncommon_codepoints)]
#![allow(mixed_script_confusables)]
#![allow(clippy::disallowed_names)]
use std::{ops, path::PathBuf};

use clap::Args;
use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};

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
    pub(crate) const ASLEEP: &str = "asleep";
    pub(crate) const ASSERT: &str = "assert";
    pub(crate) const ASSERT_EQ: &str = "assert_eq";
    pub(crate) const CHACHA: &str = "chacha";
    pub(crate) const COMPLEX_EX: &str = "ComplexEx";
    pub(crate) const EPS: &str = "eps";
    pub(crate) const EVAL: &str = "eval";
    pub(crate) const FN_NEW: &str = "new";
    pub(crate) const HTTP_GET: &str = "http_get";
    pub(crate) const INTERVAL: &str = "interval";
    pub(crate) const LEN: &str = "len";
    pub(crate) const FORMAT: &str = "format";
    pub(crate) const MAP: &str = "map";
    pub(crate) const NEW: &str = "new";
    pub(crate) const NORM_SQUARED: &str = "norm_squared";
    pub(crate) const ONE_SHOT: &str = "one_shot";
    pub(crate) const PARSE: &str = "parse";
    pub(crate) const PLUGIN: &str = "Plugin";
    pub(crate) const SLEEP: &str = "sleep";
    pub(crate) const SPAWN: &str = "spawn";
    pub(crate) const SPAWN_NAMED: &str = "spawn_named";
    pub(crate) const SUM: &str = "sum";
    pub(crate) const SQUARE: &str = "square";
    pub(crate) const TIME: &str = "time";
    pub(crate) const TIMER: &str = "timer";
    pub(crate) const TYPEOF: &str = "typeof";
    // 🚧 We have a token already...
    pub(crate) const UUID_TYPE: &str = "Uuid";
}

use lu_dog::{ObjectStore as LuDogStore, ValueType};

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
    pub lu_dog: RefType<LuDogStore>,
    /// These are the plugins that represent imported domains.
    pub models: ModelStore,
    /// This contains things that the extruder added, that the interpreter
    /// needs to know about.
    pub dirty: Vec<Dirty>,
}

impl Context {
    pub fn source(&self) -> String {
        let source = s_read!(self.lu_dog)
            .iter_dwarf_source_file()
            .next()
            .unwrap();
        let source = s_read!(source);
        source.source.clone()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            source: "unknown".into(),
            lu_dog: new_ref!(LuDogStore, LuDogStore::new()),
            models: HashMap::default(),
            dirty: Vec::default(),
        }
    }
}

pub type ValueResult = Result<RefType<Value>, ChaChaError>;

#[cfg(feature = "async")]
#[derive(Debug, Clone, Copy)]
pub struct Duration(DwarfInteger);

#[cfg(feature = "async")]
impl Duration {
    pub fn new(dur: DwarfInteger) -> Self {
        Self(dur)
    }
}

#[cfg(feature = "async")]
impl std::cmp::Ord for Duration {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0).reverse()
    }
}

#[cfg(feature = "async")]
impl std::cmp::PartialOrd for Duration {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0).reverse())
    }
}

#[cfg(feature = "async")]
impl std::cmp::PartialEq for Duration {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[cfg(feature = "async")]
impl std::cmp::Eq for Duration {}

#[cfg(feature = "async")]
impl std::fmt::Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let secs = self.0 / 1_000_000_000;
        let nanos = self.0 % 1_000_000_000;
        write!(f, "{}.{:09}", secs, nanos)
    }
}
