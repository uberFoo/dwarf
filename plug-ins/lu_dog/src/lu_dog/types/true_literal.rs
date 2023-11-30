// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"true_literal-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"true_literal-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"true_literal-const-documentation"}}}
/// True Literal
///
/// The literal `true`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"true_literal-const-definition"}}}
pub const TRUE_LITERAL: Uuid = uuid!["acf0b446-36cc-5862-ba90-bf3fa3e6de05"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct TrueLiteral;

impl TrueLiteral {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        TRUE_LITERAL
    }
}

impl Default for TrueLiteral {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
