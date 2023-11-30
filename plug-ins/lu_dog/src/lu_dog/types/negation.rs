// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"negation-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"negation-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"negation-const-documentation"}}}
/// The unary minus
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"negation-const-definition"}}}
pub const NEGATION: Uuid = uuid!["a4671940-9194-5585-84b4-4bd22b975f6f"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Negation;

impl Negation {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        NEGATION
    }
}

impl Default for Negation {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
