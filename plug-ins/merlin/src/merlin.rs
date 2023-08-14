//! Drawing Domain
//!
use uuid::{uuid, Uuid};

pub mod from;
pub mod store;
pub mod types;

pub use store::ObjectStore;
pub use types::*;

// merlin
pub const UUID_NS: Uuid = uuid!("b68c82f5-8038-55e1-98fb-1f598d8227fd");
