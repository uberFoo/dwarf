// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"from-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"from-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"from-const-definition"}}}
pub const FROM: Uuid = uuid!["5285c810-0e61-535f-b961-a43471f21357"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct From;

impl From {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        FROM
    }
}

impl Default for From {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
