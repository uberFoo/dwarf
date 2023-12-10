// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"z_uuid-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_uuid-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_uuid-const-documentation"}}}
/// The UUID Type
///
/// I feel like there are too many implementation details here.
///
/// This UUID is expected to be version 5. Generally we produce input
/// to the hash function from other UUIDs, coupled with additional
/// information from the creator to ensure a unique UUID.
///
/// The `ns` attribute is the namespace used to generate generate UUIDs
/// given a particular instance of `UUID`.
///
/// ❗️{"singleton_object": true, "translation_name": "SarzakUuid"}
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_uuid-const-definition"}}}
pub const Z_UUID: Uuid = uuid!["8d63fc44-28ea-599b-8654-877b133f0640"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ZUuid;

impl ZUuid {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        Z_UUID
    }
}

impl Default for ZUuid {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
