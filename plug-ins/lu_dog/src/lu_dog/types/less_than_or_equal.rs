// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"less_than_or_equal-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"less_than_or_equal-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"less_than_or_equal-const-documentation"}}}
/// Less than or equal: `<=`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"less_than_or_equal-const-definition"}}}
pub const LESS_THAN_OR_EQUAL: Uuid = uuid!["1694c92e-07df-54a4-8279-b93918548a61"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct LessThanOrEqual;

impl LessThanOrEqual {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        LESS_THAN_OR_EQUAL
    }
}

impl Default for LessThanOrEqual {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
