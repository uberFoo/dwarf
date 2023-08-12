// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"unconditional-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"unconditional-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"unconditional-const-documentation"}}}
/// A constant value that indicates a conditionality of _unconditional_.
///
/// ❗️{"singleton_object": true}
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"unconditional-const-definition"}}}
pub const UNCONDITIONAL: Uuid = uuid!["17ee33a9-33bb-5998-ba89-7a9f2fe12080"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Unconditional;

impl Unconditional {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        UNCONDITIONAL
    }
}

impl Default for Unconditional {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
