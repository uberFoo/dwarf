// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"multiplication-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"multiplication-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"multiplication-const-definition"}}}
pub const MULTIPLICATION: Uuid = uuid!["a645e709-1543-57a0-a731-15c288d6b14a"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Multiplication;

impl Multiplication {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        MULTIPLICATION
    }
}

impl Default for Multiplication {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
