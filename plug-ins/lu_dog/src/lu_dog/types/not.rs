// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"not-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"not-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"not-const-definition"}}}
pub const NOT: Uuid = uuid!["8b7cc57b-11d4-5fcb-b764-b99c76a8c327"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Not;

impl Not {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        NOT
    }
}

impl Default for Not {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
