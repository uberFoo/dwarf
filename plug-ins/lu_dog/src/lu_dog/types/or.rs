// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"or-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"or-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"or-const-documentation"}}}
/// The or `||` operator
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"or-const-definition"}}}
pub const OR: Uuid = uuid!["8b39b94c-4e7f-5663-bc4d-7bcd8d5c77ce"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Or;

impl Or {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        OR
    }
}

impl Default for Or {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
