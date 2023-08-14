//! Sarzak Domain
//!
use ::uuid::{uuid, Uuid};

pub mod from;
pub mod store;
pub mod types;

pub use store::ObjectStore;
pub use types::*;

// sarzak
pub const UUID_NS: Uuid = uuid!("daccabb9-eb3a-5cde-ba7c-19a3f22ab649");

pub const MODEL: &[u8] = include_bytes!("../models/sarzak.bin");
