// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"x_if-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::block::Block;
use crate::lu_dog::types::expression::Expression;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-struct-documentation"}}}
/// The if Expression
///
/// It does include an else, at no extra charge!
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct XIf {
    pub id: Uuid,
    /// R52: [`XIf`] 'false block' [`Block`]
    pub false_block: Option<Uuid>,
    /// R44: [`XIf`] 'branches based on' [`Expression`]
    pub test: Uuid,
    /// R46: [`XIf`] 'when true, evaluates' [`Block`]
    pub true_block: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-implementation"}}}
impl XIf {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-struct-impl-new"}}}
    /// Inter a new 'If' in the store, and return it's `id`.
    pub fn new(
        false_block: Option<&Arc<RwLock<Block>>>,
        test: &Arc<RwLock<Expression>>,
        true_block: &Arc<RwLock<Block>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<XIf>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(XIf {
            id,
            false_block: false_block.map(|block| block.read().unwrap().id),
            test: test.read().unwrap().id(),
            true_block: true_block.read().unwrap().id,
        }));
        store.inter_x_if(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-struct-impl-nav-forward-cond-to-false_block"}}}
    /// Navigate to [`Block`] across R52(1-*c)
    pub fn r52_block<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Block>>> {
        match self.false_block {
            Some(ref false_block) => vec![store.exhume_block(&false_block).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-struct-impl-nav-forward-to-test"}}}
    /// Navigate to [`Expression`] across R44(1-*)
    pub fn r44_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.test).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-struct-impl-nav-forward-to-true_block"}}}
    /// Navigate to [`Block`] across R46(1-*)
    pub fn r46_block<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Block>>> {
        vec![store.exhume_block(&self.true_block).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_if-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
