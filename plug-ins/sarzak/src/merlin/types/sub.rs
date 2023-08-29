// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"sub-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"sub-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"sub-const-definition"}}}
pub const SUB: Uuid = uuid!["146d7a75-c86b-59a7-a52a-ac522d748a47"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Sub;

impl Sub {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        SUB
    }
}

impl Default for Sub {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
