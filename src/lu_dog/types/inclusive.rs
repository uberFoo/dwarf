// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"inclusive-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"inclusive-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"inclusive-const-definition"}}}
pub const INCLUSIVE: Uuid = uuid!["25e8e986-5a74-5da1-b4d1-496e9f126860"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Inclusive;

impl Inclusive {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        INCLUSIVE
    }
}

impl Default for Inclusive {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
