// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"top-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"top-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"top-const-definition"}}}
pub const TOP: Uuid = uuid!["a6f42b8c-0532-568c-a739-87612433af1d"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Top;

impl Top {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        TOP
    }
}

impl Default for Top {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
