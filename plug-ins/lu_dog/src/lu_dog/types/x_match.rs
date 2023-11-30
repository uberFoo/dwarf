// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"x_match-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use crate::lu_dog::types::pattern::Pattern;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-struct-documentation"}}}
/// Match a pattern to a scrutinee and evaluate a branch based on the results.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct XMatch {
    pub id: Uuid,
    pub uniqueness_generator: Uuid,
    /// R91: [`XMatch`] 'deconstructs ' [`Expression`]
    pub scrutinee: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-implementation"}}}
impl XMatch {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-struct-impl-new"}}}
    /// Inter a new 'Match' in the store, and return it's `id`.
    pub fn new(
        uniqueness_generator: Uuid,
        scrutinee: &Arc<RwLock<Expression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<XMatch>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(XMatch {
            id,
            uniqueness_generator,
            scrutinee: scrutinee.read().unwrap().id(),
        }));
        store.inter_x_match(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-struct-impl-nav-forward-to-scrutinee"}}}
    /// Navigate to [`Expression`] across R91(1-*)
    pub fn r91_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.scrutinee).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-struct-impl-nav-backward-assoc-many-to-pattern"}}}
    /// Navigate to [`Pattern`] across R87(1-M)
    pub fn r87_pattern<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Pattern>>> {
        store
            .iter_pattern()
            .filter(|pattern| pattern.read().unwrap().x_match == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_match-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
