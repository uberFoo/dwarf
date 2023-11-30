// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"less_than-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"less_than-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"less_than-const-documentation"}}}
/// Less that operator `<`
///
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"less_than-const-definition"}}}
pub const LESS_THAN: Uuid = uuid!["42ad4f7a-0828-519b-99b7-514577d12634"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct LessThan;

impl LessThan {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        LESS_THAN
    }
}

impl Default for LessThan {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
