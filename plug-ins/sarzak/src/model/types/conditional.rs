// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"conditional-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditional-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditional-const-documentation"}}}
/// A constant value that indicates a conditionality of _conditional_.
///
/// ❗️{"singleton_object": true}
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditional-const-definition"}}}
pub const CONDITIONAL: Uuid = uuid!["1ef6f1f8-de66-552b-8d4a-a04215c37c1e"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Conditional;

impl Conditional {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        CONDITIONAL
    }
}

impl Default for Conditional {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
