// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"equal-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"equal-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"equal-const-definition"}}}
pub const EQUAL: Uuid = uuid!["afd5efd2-654b-532a-b1b8-296adeac03e8"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Equal;

impl Equal {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        EQUAL
    }
}

impl Default for Equal {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
