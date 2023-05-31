// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"field_access_target-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-use-statements"}}}
use std::sync::{Arc, RwLock};

use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::field::Field;
use crate::lu_dog::types::field_access::FieldAccess;
use crate::lu_dog::types::function::Function;
use serde::{Deserialize, Serialize};
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-enum-documentation"}}}
/// The target of a field access.
///
/// It may be either a [`Field`] or a [`Function`].
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-enum-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum FieldAccessTarget {
    Field(Uuid),
    Function(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-implementation"}}}
impl FieldAccessTarget {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-new-impl"}}}
    /// Create a new instance of FieldAccessTarget::Field
    pub fn new_field(field: &Arc<RwLock<Field>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        if let Some(field) = store.exhume_field_access_target(&field.read().unwrap().id) {
            field
        } else {
            let new = Arc::new(RwLock::new(Self::Field(field.read().unwrap().id)));
            store.inter_field_access_target(new.clone());
            new
        }
    }

    /// Create a new instance of FieldAccessTarget::Function
    pub fn new_function(
        function: &Arc<RwLock<Function>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        if let Some(function) = store.exhume_field_access_target(&function.read().unwrap().id) {
            function
        } else {
            let new = Arc::new(RwLock::new(Self::Function(function.read().unwrap().id)));
            store.inter_field_access_target(new.clone());
            new
        }
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            FieldAccessTarget::Field(id) => *id,
            FieldAccessTarget::Function(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-struct-impl-nav-backward-1_M-to-field_access"}}}
    /// Navigate to [`FieldAccess`] across R65(1-M)
    pub fn r65_field_access<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<FieldAccess>>> {
        store
            .iter_field_access()
            .filter(|field_access| field_access.read().unwrap().field == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
