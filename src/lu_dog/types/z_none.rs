// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"z_none-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_none-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_none-const-documentation"}}}
/// The only purpose of this type is to represent _no_ value.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_none-const-definition"}}}
pub const Z_NONE: Uuid = uuid!["c419a3e3-5556-5475-97ac-7054e5a21ca3"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ZNone;

impl ZNone {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        Z_NONE
    }
}

impl Default for ZNone {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
