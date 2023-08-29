// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"left-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"left-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"left-const-definition"}}}
pub const LEFT: Uuid = uuid!["edfbb79f-6c49-5470-b9dd-f278c4e71ea2"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Left;

impl Left {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        LEFT
    }
}

impl Default for Left {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
