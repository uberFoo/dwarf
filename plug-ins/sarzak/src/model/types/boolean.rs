// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"boolean-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean-const-documentation"}}}
/// The Boolean Type
///
/// This type holds `true` and `false` values. This type is just a placeholder. It's implementation
///  is determined downstream by the code generator.
///
/// ❗️{"singleton_object": true}
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean-const-definition"}}}
pub const BOOLEAN: Uuid = uuid!["b1a060e2-a30e-5291-8ef3-7fad8a6311d4"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Boolean;

impl Boolean {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        BOOLEAN
    }
}

impl Default for Boolean {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
