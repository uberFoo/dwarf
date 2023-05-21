// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"to-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"to-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"to-const-definition"}}}
pub const TO: Uuid = uuid!["f3610e2b-52b4-507b-812a-e1e13b493090"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct To;

impl To {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        TO
    }
}

impl Default for To {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
