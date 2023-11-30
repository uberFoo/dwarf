// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"field_expression-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use crate::lu_dog::types::named_field_expression::NamedFieldExpression;
use crate::lu_dog::types::struct_expression::StructExpression;
use crate::lu_dog::types::unnamed_field_expression::UnnamedFieldExpression;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-hybrid-documentation"}}}
/// A Struct Field Expression
///
/// This assigns a value to a field in a structure.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-hybrid-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct FieldExpression {
    pub subtype: FieldExpressionEnum,
    pub id: Uuid,
    /// R38: [`FieldExpression`] '' [`Expression`]
    pub expression: Uuid,
    /// R26: [`FieldExpression`] 'belongs to a' [`StructExpression`]
    pub woog_struct: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-hybrid-enum-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum FieldExpressionEnum {
    NamedFieldExpression(Uuid),
    UnnamedFieldExpression(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-implementation"}}}
impl FieldExpression {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-struct-impl-new_named_field_expression"}}}
    /// Inter a new FieldExpression in the store, and return it's `id`.
    pub fn new_named_field_expression(
        expression: &Arc<RwLock<Expression>>,
        woog_struct: &Arc<RwLock<StructExpression>>,
        subtype: &Arc<RwLock<NamedFieldExpression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<FieldExpression>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(FieldExpression {
            expression: expression.read().unwrap().id(),
            woog_struct: woog_struct.read().unwrap().id,
            subtype: FieldExpressionEnum::NamedFieldExpression(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_field_expression(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-struct-impl-new_unnamed_field_expression"}}}
    /// Inter a new FieldExpression in the store, and return it's `id`.
    pub fn new_unnamed_field_expression(
        expression: &Arc<RwLock<Expression>>,
        woog_struct: &Arc<RwLock<StructExpression>>,
        subtype: &Arc<RwLock<UnnamedFieldExpression>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<FieldExpression>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(FieldExpression {
            expression: expression.read().unwrap().id(),
            woog_struct: woog_struct.read().unwrap().id,
            subtype: FieldExpressionEnum::UnnamedFieldExpression(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_field_expression(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-struct-impl-nav-forward-to-expression"}}}
    /// Navigate to [`Expression`] across R38(1-*)
    pub fn r38_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.expression).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-struct-impl-nav-forward-to-woog_struct"}}}
    /// Navigate to [`StructExpression`] across R26(1-*)
    pub fn r26_struct_expression<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<StructExpression>>> {
        vec![store.exhume_struct_expression(&self.woog_struct).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_expression-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
