// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"assignment-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"assignment-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"assignment-const-documentation"}}}
/// Assignment to a storage location
///
/// E.g., `a = b`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"assignment-const-definition"}}}
pub const ASSIGNMENT: Uuid = uuid!["f0e3fca1-6a53-53da-9048-83f8a0da060d"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Assignment;

impl Assignment {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        ASSIGNMENT
    }
}

impl Default for Assignment {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
