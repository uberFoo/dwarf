// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"false_literal-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"false_literal-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"false_literal-const-documentation"}}}
/// False Literal
///
/// The literal `false`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"false_literal-const-definition"}}}
pub const FALSE_LITERAL: Uuid = uuid!["a904e4be-d9ae-568d-9767-1098b31aba7f"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct FalseLiteral;

impl FalseLiteral {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        FALSE_LITERAL
    }
}

impl Default for FalseLiteral {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
