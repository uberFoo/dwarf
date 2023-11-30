// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"boolean_operator-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean_operator-use-statements"}}}
use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::and::AND;
use crate::lu_dog::types::binary::Binary;
use crate::lu_dog::types::or::OR;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean_operator-enum-documentation"}}}
/// A Boolean Operaator
///
/// There are two — || and &&.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean_operator-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum BooleanOperator {
    And(Uuid),
    Or(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean_operator-implementation"}}}
impl BooleanOperator {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean_operator-new-impl"}}}
    /// Create a new instance of BooleanOperator::And
    pub fn new_and(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_boolean_operator(&AND).unwrap()
    }

    /// Create a new instance of BooleanOperator::Or
    pub fn new_or(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_boolean_operator(&OR).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean_operator-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::And(id) => *id,
            Self::Or(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"boolean_operator-impl-nav-subtype-to-supertype-binary"}}}
    // Navigate to [`Binary`] across R48(isa)
    pub fn r48_binary<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Binary>>> {
        vec![store.exhume_binary(&self.id()).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
