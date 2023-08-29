// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"right-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"right-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"right-const-definition"}}}
pub const RIGHT: Uuid = uuid!["586d17c1-97c4-594b-b757-e83396caaf73"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Right;

impl Right {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        RIGHT
    }
}

impl Default for Right {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
