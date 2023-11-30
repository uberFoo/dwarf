// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"not_equal-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"not_equal-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"not_equal-const-documentation"}}}
/// The Not Equal Expression
///
/// This is the `!=` operator expression.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"not_equal-const-definition"}}}
pub const NOT_EQUAL: Uuid = uuid!["3596966c-fab8-5a99-86b0-1820e6a19d3e"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct NotEqual;

impl NotEqual {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        NOT_EQUAL
    }
}

impl Default for NotEqual {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
