// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"type_cast-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use crate::lu_dog::types::value_type::ValueType;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-struct-documentation"}}}
/// Typecast Operator Expression
///
/// This is the `as` operator.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct TypeCast {
    pub id: Uuid,
    /// R68: [`TypeCast`] '' [`Expression`]
    pub lhs: Uuid,
    /// R69: [`TypeCast`] '' [`ValueType`]
    pub ty: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-implementation"}}}
impl TypeCast {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-struct-impl-new"}}}
    /// Inter a new 'Type Cast' in the store, and return it's `id`.
    pub fn new(
        lhs: &Arc<RwLock<Expression>>,
        ty: &Arc<RwLock<ValueType>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<TypeCast>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(TypeCast {
            id,
            lhs: lhs.read().unwrap().id(),
            ty: ty.read().unwrap().id(),
        }));
        store.inter_type_cast(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-struct-impl-nav-forward-to-lhs"}}}
    /// Navigate to [`Expression`] across R68(1-*)
    pub fn r68_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.lhs).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-struct-impl-nav-forward-to-ty"}}}
    /// Navigate to [`ValueType`] across R69(1-*)
    pub fn r69_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.ty).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"type_cast-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
