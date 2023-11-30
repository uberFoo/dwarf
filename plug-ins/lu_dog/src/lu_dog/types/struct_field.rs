// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"struct_field-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_field-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::enum_field::EnumField;
use crate::lu_dog::types::enum_field::EnumFieldEnum;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_field-struct-documentation"}}}
/// A field that is a structure.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_field-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct StructField {
    pub id: Uuid,
    pub name: String,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_field-implementation"}}}
impl StructField {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_field-struct-impl-new"}}}
    /// Inter a new 'Struct Field' in the store, and return it's `id`.
    pub fn new(name: String, store: &mut LuDogStore) -> Arc<RwLock<StructField>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(StructField { id, name }));
        store.inter_struct_field(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_field-impl-nav-subtype-to-supertype-enum_field"}}}
    // Navigate to [`EnumField`] across R85(isa)
    pub fn r85_enum_field<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<EnumField>>> {
        vec![store
            .iter_enum_field()
            .find(|enum_field| {
                if let EnumFieldEnum::StructField(id) = enum_field.read().unwrap().subtype {
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
