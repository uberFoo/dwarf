// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"field_access_target-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-use-statements"}}}
use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::enum_field::EnumField;
use crate::lu_dog::types::field::Field;
use crate::lu_dog::types::field_access::FieldAccess;
use crate::lu_dog::types::function::Function;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-enum-documentation"}}}
/// The target of a field access.
///
/// It may be either a [`Field`] or a [`Function`].
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum FieldAccessTarget {
    EnumField(Uuid),
    Field(Uuid),
    Function(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-implementation"}}}
impl FieldAccessTarget {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-new-impl"}}}
    /// Create a new instance of FieldAccessTarget::EnumField
    pub fn new_enum_field(
        enum_field: &Arc<RwLock<EnumField>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = enum_field.read().unwrap().id;
        if let Some(enum_field) = store.exhume_field_access_target(&id) {
            enum_field
        } else {
            let new = Arc::new(RwLock::new(Self::EnumField(id)));
            store.inter_field_access_target(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of FieldAccessTarget::Field
    pub fn new_field(field: &Arc<RwLock<Field>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = field.read().unwrap().id;
        if let Some(field) = store.exhume_field_access_target(&id) {
            field
        } else {
            let new = Arc::new(RwLock::new(Self::Field(id)));
            store.inter_field_access_target(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of FieldAccessTarget::Function
    pub fn new_function(
        function: &Arc<RwLock<Function>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = function.read().unwrap().id;
        if let Some(function) = store.exhume_field_access_target(&id) {
            function
        } else {
            let new = Arc::new(RwLock::new(Self::Function(id)));
            store.inter_field_access_target(new.clone());
            new
        }
    } // wtf?

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"field_access_target-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::EnumField(id) => *id,
            Self::Field(id) => *id,
            Self::Function(id) => *id,
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
