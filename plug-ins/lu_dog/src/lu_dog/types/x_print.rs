// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"x_print-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_print-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_print-struct-documentation"}}}
/// A Print Expression?
///
/// Shold this be a statement?
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_print-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct XPrint {
    pub id: Uuid,
    /// R32: [`XPrint`] '' [`Expression`]
    pub expression: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_print-implementation"}}}
impl XPrint {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_print-struct-impl-new"}}}
    /// Inter a new 'Print' in the store, and return it's `id`.
    pub fn new(
        expression: &Arc<RwLock<Expression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<XPrint>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(XPrint {
            id,
            expression: expression.read().unwrap().id(),
        }));
        store.inter_x_print(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_print-struct-impl-nav-forward-to-expression"}}}
    /// Navigate to [`Expression`] across R32(1-*)
    pub fn r32_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.expression).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_print-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
