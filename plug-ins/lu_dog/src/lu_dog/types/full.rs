// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"full-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"full-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"full-const-definition"}}}
pub const FULL: Uuid = uuid!["3f2af943-5322-5ea2-a261-1e4626a1123a"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Full;

impl Full {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        FULL
    }
}

impl Default for Full {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
