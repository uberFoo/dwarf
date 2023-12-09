// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"z_string-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_string-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_string-const-documentation"}}}
/// The String Type
///
/// This type holds unicode characters. This type is just a placeholder. It's implementation
///  is determined downstream by the code generator.
///
/// ❗️{"singleton_object": true}
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_string-const-definition"}}}
pub const Z_STRING: Uuid = uuid!["eb75208b-bf2e-55f9-9a38-7c1553f31960"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ZString;

impl ZString {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        Z_STRING
    }
}

impl Default for ZString {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
