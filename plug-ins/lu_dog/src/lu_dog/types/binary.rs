// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"binary-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-use-statements"}}}
use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::addition::ADDITION;
use crate::lu_dog::types::assignment::ASSIGNMENT;
use crate::lu_dog::types::boolean_operator::BooleanOperator;
use crate::lu_dog::types::division::DIVISION;
use crate::lu_dog::types::multiplication::MULTIPLICATION;
use crate::lu_dog::types::operator::Operator;
use crate::lu_dog::types::operator::OperatorEnum;
use crate::lu_dog::types::subtraction::SUBTRACTION;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-enum-documentation"}}}
/// Binary Operators
///
/// +, -, etc.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Binary {
    Addition(Uuid),
    Assignment(Uuid),
    BooleanOperator(Uuid),
    Division(Uuid),
    Multiplication(Uuid),
    Subtraction(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-implementation"}}}
impl Binary {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-new-impl"}}}
    /// Create a new instance of Binary::Addition
    pub fn new_addition(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_binary(&ADDITION).unwrap()
    }

    /// Create a new instance of Binary::Assignment
    pub fn new_assignment(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_binary(&ASSIGNMENT).unwrap()
    }

    /// Create a new instance of Binary::BooleanOperator
    pub fn new_boolean_operator(
        boolean_operator: &Arc<RwLock<BooleanOperator>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = boolean_operator.read().unwrap().id();
        if let Some(boolean_operator) = store.exhume_binary(&id) {
            boolean_operator
        } else {
            let new = Arc::new(RwLock::new(Self::BooleanOperator(id)));
            store.inter_binary(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of Binary::Division
    pub fn new_division(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_binary(&DIVISION).unwrap()
    }

    /// Create a new instance of Binary::Multiplication
    pub fn new_multiplication(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_binary(&MULTIPLICATION).unwrap()
    }

    /// Create a new instance of Binary::Subtraction
    pub fn new_subtraction(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_binary(&SUBTRACTION).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Addition(id) => *id,
            Self::Assignment(id) => *id,
            Self::BooleanOperator(id) => *id,
            Self::Division(id) => *id,
            Self::Multiplication(id) => *id,
            Self::Subtraction(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-impl-nav-subtype-to-supertype-operator"}}}
    // Navigate to [`Operator`] across R47(isa)
    pub fn r47_operator<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Operator>>> {
        vec![store
            .iter_operator()
            .find(|operator| {
                if let OperatorEnum::Binary(id) = operator.read().unwrap().subtype {
                    id == self.id()
                } else {
                    false
                }
            })
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
