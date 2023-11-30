// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"macro_call-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"macro_call-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"macro_call-const-documentation"}}}
/// A macro invocation
///
/// A macro invocation is a string, followed by a bang (`!`), followed by parens and arguments
/// .
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"macro_call-const-definition"}}}
pub const MACRO_CALL: Uuid = uuid!["e77de343-3d89-544c-a99c-8c7f3f392bb5"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct MacroCall;

impl MacroCall {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        MACRO_CALL
    }
}

impl Default for MacroCall {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
