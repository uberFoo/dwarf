// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"many-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"many-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"many-const-definition"}}}
pub const MANY: Uuid = uuid!["e1717af5-5000-5f50-be5a-40dd9747f5fc"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Many;

impl Many {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        MANY
    }
}

impl Default for Many {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
