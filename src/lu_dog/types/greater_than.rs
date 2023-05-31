// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"greater_than-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"greater_than-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"greater_than-const-definition"}}}
pub const GREATER_THAN: Uuid = uuid!["b712aab1-95c2-5342-8494-d35bf3c161fe"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct GreaterThan;

impl GreaterThan {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        GREATER_THAN
    }
}

impl Default for GreaterThan {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
