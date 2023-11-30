// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"division-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"division-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"division-const-definition"}}}
pub const DIVISION: Uuid = uuid!["9bdbeed5-5e34-53a7-bc76-5ebf19a39481"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Division;

impl Division {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        DIVISION
    }
}

impl Default for Division {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
