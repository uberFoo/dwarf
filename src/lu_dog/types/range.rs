// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"range-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"range-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"range-const-definition"}}}
pub const RANGE: Uuid = uuid!["50e24d8f-3739-5fd9-b5ca-f537eba4b21a"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Range;

impl Range {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        RANGE
    }
}

impl Default for Range {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
