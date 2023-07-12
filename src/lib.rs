#![allow(uncommon_codepoints)]
use std::{ops, path::PathBuf};

use clap::Args;
use serde::{Deserialize, Serialize};

pub mod chacha;
pub mod dwarf;
#[cfg(all(
    not(feature = "print-std-out"),
    not(any(feature = "single", feature = "single-vec"))
))]
pub mod tui;
// pub(crate) mod woog_structs;
// pub mod lu_dog_proxy;

pub use ::sarzak::{lu_dog, sarzak};
pub use chacha::{
    error::{ChaChaError, ChaChaErrorReporter},
    interpreter::{self, initialize_interpreter},
    value::{StoreProxy, Value},
};

// These should eventually come from the domain.
pub type DwarfInteger = i64;
pub type DwarfFloat = f64;

use lu_dog::ValueType;

cfg_if::cfg_if! {
    if #[cfg(feature = "single")] {
        type SarzakStorePtr = Uuid;
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
        type SarzakStorePtr = usize;
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

        type SarzakStorePtr = Uuid;
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
        type SarzakStorePtr = Uuid;
        type RcType<T> = std::sync::Arc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::sync::Arc::new(value)
            }
        }

        type RefType<T> = std::sync::Arc<std::sync::RwLock<T>>;
        // type RefType<T> = std::sync::Arc<no_deadlocks::RwLock<T>>;
        impl<T> NewRef<T> for RefType<T> {
            fn new_ref(value: T) -> RefType<T> {
                std::sync::Arc::new(std::sync::RwLock::new(value))
                // std::sync::Arc::new(no_deadlocks::RwLock::new(value))
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

   } else if #[cfg(feature = "multi-vec")] {
        type SarzakStorePtr = usize;
        type RcType<T> = std::sync::Arc<T>;
        impl<T> NewRcType<T> for RcType<T> {
            fn new_rc_type(value: T) -> RcType<T> {
                std::sync::Arc::new(value)
            }
        }

        type RefType<T> = std::sync::Arc<std::sync::RwLock<T>>;
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

    } else if #[cfg(feature = "multi-parking-lot-mutex")] {
        // compile_error!("parking lot mutex is not currently supported");
        type SarzakStorePtr = Uuid;
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
        type SarzakStorePtr = Uuid;
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
