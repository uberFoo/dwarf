// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"subtraction-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"subtraction-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"subtraction-const-documentation"}}}
/// The subtraction operator: `-`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"subtraction-const-definition"}}}
pub const SUBTRACTION: Uuid = uuid!["f1f6f96b-31a8-5158-b940-b08b7241117d"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Subtraction;

impl Subtraction {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        SUBTRACTION
    }
}

impl Default for Subtraction {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
