// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"and-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"and-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"and-const-documentation"}}}
/// The Boolean And Operator
///
/// And, aka, `&&`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"and-const-definition"}}}
pub const AND: Uuid = uuid!["bd9bf026-a7db-59e2-a44a-0e88124b8ec6"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct And;

impl And {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        AND
    }
}

impl Default for And {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
