// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"comparison-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"comparison-use-statements"}}}
use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::equal::EQUAL;
use crate::lu_dog::types::greater_than::GREATER_THAN;
use crate::lu_dog::types::greater_than_or_equal::GREATER_THAN_OR_EQUAL;
use crate::lu_dog::types::less_than::LESS_THAN;
use crate::lu_dog::types::less_than_or_equal::LESS_THAN_OR_EQUAL;
use crate::lu_dog::types::not_equal::NOT_EQUAL;
use crate::lu_dog::types::operator::Operator;
use crate::lu_dog::types::operator::OperatorEnum;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"comparison-enum-documentation"}}}
/// Comparison Operators
///
/// Things like == and !=, etc.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"comparison-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Comparison {
    Equal(Uuid),
    GreaterThan(Uuid),
    GreaterThanOrEqual(Uuid),
    LessThan(Uuid),
    LessThanOrEqual(Uuid),
    NotEqual(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"comparison-implementation"}}}
impl Comparison {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"comparison-new-impl"}}}
    /// Create a new instance of Comparison::Equal
    pub fn new_equal(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_comparison(&EQUAL).unwrap()
    }

    /// Create a new instance of Comparison::GreaterThan
    pub fn new_greater_than(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_comparison(&GREATER_THAN).unwrap()
    }

    /// Create a new instance of Comparison::GreaterThanOrEqual
    pub fn new_greater_than_or_equal(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_comparison(&GREATER_THAN_OR_EQUAL).unwrap()
    }

    /// Create a new instance of Comparison::LessThan
    pub fn new_less_than(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_comparison(&LESS_THAN).unwrap()
    }

    /// Create a new instance of Comparison::LessThanOrEqual
    pub fn new_less_than_or_equal(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_comparison(&LESS_THAN_OR_EQUAL).unwrap()
    }

    /// Create a new instance of Comparison::NotEqual
    pub fn new_not_equal(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_comparison(&NOT_EQUAL).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"comparison-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Equal(id) => *id,
            Self::GreaterThan(id) => *id,
            Self::GreaterThanOrEqual(id) => *id,
            Self::LessThan(id) => *id,
            Self::LessThanOrEqual(id) => *id,
            Self::NotEqual(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"comparison-impl-nav-subtype-to-supertype-operator"}}}
    // Navigate to [`Operator`] across R47(isa)
    pub fn r47_operator<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Operator>>> {
        vec![store
            .iter_operator()
            .find(|operator| {
                if let OperatorEnum::Comparison(id) = operator.read().unwrap().subtype {
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
