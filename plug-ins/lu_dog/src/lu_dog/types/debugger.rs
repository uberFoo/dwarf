// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"debugger-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"debugger-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"debugger-const-documentation"}}}
/// An expresision to invoke the debugger;
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"debugger-const-definition"}}}
pub const DEBUGGER: Uuid = uuid!["0a02fb20-b343-530c-9467-1cb2d6957339"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Debugger;

impl Debugger {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        DEBUGGER
    }
}

impl Default for Debugger {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
