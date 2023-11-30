// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"empty_expression-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"empty_expression-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"empty_expression-const-definition"}}}
pub const EMPTY_EXPRESSION: Uuid = uuid!["631f47b1-f98b-54f2-886c-b4ed4c7ad909"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct EmptyExpression;

impl EmptyExpression {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        EMPTY_EXPRESSION
    }
}

impl Default for EmptyExpression {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
