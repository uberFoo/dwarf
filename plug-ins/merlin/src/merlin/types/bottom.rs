// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"bottom-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"bottom-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"bottom-const-definition"}}}
pub const BOTTOM: Uuid = uuid!["d94419ef-1302-5b3b-9c9e-dc04842dceba"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Bottom;

impl Bottom {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        BOTTOM
    }
}

impl Default for Bottom {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
