// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"item_statement-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item_statement-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item_statement-const-documentation"}}}
/// An Item in statement position, i.e., inside of a block.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item_statement-const-definition"}}}
pub const ITEM_STATEMENT: Uuid = uuid!["309ad411-ef8c-5b27-9d1d-e4e37a5806de"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ItemStatement;

impl ItemStatement {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        ITEM_STATEMENT
    }
}

impl Default for ItemStatement {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
