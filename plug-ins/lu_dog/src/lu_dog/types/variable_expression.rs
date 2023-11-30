// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"variable_expression-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable_expression-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable_expression-struct-documentation"}}}
/// A Local Variable Expression
///
/// This is what happens when a variable is an r-value.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable_expression-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct VariableExpression {
    pub id: Uuid,
    pub name: String,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable_expression-implementation"}}}
impl VariableExpression {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable_expression-struct-impl-new"}}}
    /// Inter a new 'Variable Expression' in the store, and return it's `id`.
    pub fn new(name: String, store: &mut LuDogStore) -> Arc<RwLock<VariableExpression>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(VariableExpression { id, name }));
        store.inter_variable_expression(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable_expression-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
