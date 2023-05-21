// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"greater_than_or_equal-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"greater_than_or_equal-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"greater_than_or_equal-const-definition"}}}
pub const GREATER_THAN_OR_EQUAL: Uuid = uuid!["4f5675c8-9119-51c7-8ede-5639aebb3f19"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct GreaterThanOrEqual;

impl GreaterThanOrEqual {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        GREATER_THAN_OR_EQUAL
    }
}

impl Default for GreaterThanOrEqual {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
