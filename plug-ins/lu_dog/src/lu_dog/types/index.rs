// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"index-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-struct-documentation"}}}
/// An index expression
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Index {
    pub id: Uuid,
    /// R56: [`Index`] '' [`Expression`]
    pub index: Uuid,
    /// R57: [`Index`] '' [`Expression`]
    pub target: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-implementation"}}}
impl Index {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-struct-impl-new"}}}
    /// Inter a new 'Index' in the store, and return it's `id`.
    pub fn new(
        index: &Arc<RwLock<Expression>>,
        target: &Arc<RwLock<Expression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Index>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Index {
            id,
            index: index.read().unwrap().id(),
            target: target.read().unwrap().id(),
        }));
        store.inter_index(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-struct-impl-nav-forward-to-index"}}}
    /// Navigate to [`Expression`] across R56(1-*)
    pub fn r56_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.index).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-struct-impl-nav-forward-to-target"}}}
    /// Navigate to [`Expression`] across R57(1-*)
    pub fn r57_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.target).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"index-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
