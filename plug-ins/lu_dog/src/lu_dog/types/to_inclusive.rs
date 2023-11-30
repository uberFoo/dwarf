// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"to_inclusive-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"to_inclusive-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"to_inclusive-const-definition"}}}
pub const TO_INCLUSIVE: Uuid = uuid!["df17de98-7148-580e-a61a-7fb4b9c7d5d1"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ToInclusive;

impl ToInclusive {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        TO_INCLUSIVE
    }
}

impl Default for ToInclusive {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
