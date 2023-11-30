// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"expression_statement-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression_statement-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use crate::lu_dog::types::statement::Statement;
use crate::lu_dog::types::statement::StatementEnum;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression_statement-struct-documentation"}}}
/// A statement that consists of just an expression.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression_statement-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ExpressionStatement {
    pub id: Uuid,
    /// R31: [`ExpressionStatement`] '' [`Expression`]
    pub expression: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression_statement-implementation"}}}
impl ExpressionStatement {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression_statement-struct-impl-new"}}}
    /// Inter a new 'Expression Statement' in the store, and return it's `id`.
    pub fn new(
        expression: &Arc<RwLock<Expression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<ExpressionStatement>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(ExpressionStatement {
            id,
            expression: expression.read().unwrap().id(),
        }));
        store.inter_expression_statement(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression_statement-struct-impl-nav-forward-to-expression"}}}
    /// Navigate to [`Expression`] across R31(1-*)
    pub fn r31_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.expression).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"expression_statement-impl-nav-subtype-to-supertype-statement"}}}
    // Navigate to [`Statement`] across R16(isa)
    pub fn r16_statement<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Statement>>> {
        vec![store
            .iter_statement()
            .find(|statement| {
                if let StatementEnum::ExpressionStatement(id) = statement.read().unwrap().subtype {
                    id == self.id
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
