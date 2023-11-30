// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"addition-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"addition-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"addition-const-documentation"}}}
/// The addition operator: `+`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"addition-const-definition"}}}
pub const ADDITION: Uuid = uuid!["82056c56-acb4-590c-a0f6-895876f1dc19"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Addition;

impl Addition {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        ADDITION
    }
}

impl Default for Addition {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
