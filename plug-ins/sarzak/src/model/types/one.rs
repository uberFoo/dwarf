// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"one-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"one-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"one-const-documentation"}}}
/// A constant value that indicates a cardinality of _one_.
///
/// ❗️{"singleton_object": true}
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"one-const-definition"}}}
pub const ONE: Uuid = uuid!["84d60cb6-04cf-5c82-9e38-79d3999b5d5c"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct One;

impl One {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        ONE
    }
}

impl Default for One {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
