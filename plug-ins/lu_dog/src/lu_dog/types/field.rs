// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"field-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::field_access_target::FieldAccessTarget;
use crate::lu_dog::types::value_type::ValueType;
use crate::lu_dog::types::woog_struct::WoogStruct;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-struct-documentation"}}}
/// A Field in a data structure
///
/// A field has a name, and a type.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Field {
    pub id: Uuid,
    pub name: String,
    /// R7: [`Field`] 'comprises a' [`WoogStruct`]
    pub x_model: Uuid,
    /// R5: [`Field`] 'has a' [`ValueType`]
    pub ty: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-implementation"}}}
impl Field {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-struct-impl-new"}}}
    /// Inter a new 'Field' in the store, and return it's `id`.
    pub fn new(
        name: String,
        x_model: &Arc<RwLock<WoogStruct>>,
        ty: &Arc<RwLock<ValueType>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Field>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Field {
            id,
            name,
            x_model: x_model.read().unwrap().id,
            ty: ty.read().unwrap().id(),
        }));
        store.inter_field(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-struct-impl-nav-forward-to-x_model"}}}
    /// Navigate to [`WoogStruct`] across R7(1-*)
    pub fn r7_woog_struct<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<WoogStruct>>> {
        vec![store.exhume_woog_struct(&self.x_model).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-struct-impl-nav-forward-to-ty"}}}
    /// Navigate to [`ValueType`] across R5(1-*)
    pub fn r5_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.ty).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field-impl-nav-subtype-to-supertype-field_access_target"}}}
    // Navigate to [`FieldAccessTarget`] across R67(isa)
    pub fn r67_field_access_target<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<FieldAccessTarget>>> {
        vec![store.exhume_field_access_target(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
