// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"x_future-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_future-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::value_type::ValueType;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_future-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct XFuture {
    pub id: Uuid,
    /// R2: [`XFuture`] 'contains' [`ValueType`]
    pub x_value: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_future-implementation"}}}
impl XFuture {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_future-struct-impl-new"}}}
    /// Inter a new 'Future' in the store, and return it's `id`.
    pub fn new(x_value: &Arc<RwLock<ValueType>>, store: &mut LuDogStore) -> Arc<RwLock<XFuture>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(XFuture {
            id,
            x_value: x_value.read().unwrap().id(),
        }));
        store.inter_x_future(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_future-struct-impl-nav-forward-to-x_value"}}}
    /// Navigate to [`ValueType`] across R2(1-*)
    pub fn r2_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.x_value).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_future-impl-nav-subtype-to-supertype-value_type"}}}
    // Navigate to [`ValueType`] across R1(isa)
    pub fn r1_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
