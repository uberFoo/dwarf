// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"function_call-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"function_call-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"function_call-const-definition"}}}
pub const FUNCTION_CALL: Uuid = uuid!["e6133810-843b-5dab-90a4-c424232d5205"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct FunctionCall;

impl FunctionCall {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        FUNCTION_CALL
    }
}

impl Default for FunctionCall {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
