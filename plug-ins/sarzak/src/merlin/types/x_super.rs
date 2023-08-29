// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"x_super-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_super-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_super-const-definition"}}}
pub const X_SUPER: Uuid = uuid!["4552a55e-dcda-5ed0-9aca-328afab813df"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct XSuper;

impl XSuper {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        X_SUPER
    }
}

impl Default for XSuper {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
