// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"z_object_store-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::implementation_block::ImplementationBlock;
use crate::lu_dog::types::object_wrapper::ObjectWrapper;
use crate::lu_dog::types::value_type::ValueType;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-struct-documentation"}}}
/// A generated ObjectStore
///
/// This is the backing store for the structs.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ZObjectStore {
    pub domain: String,
    pub id: Uuid,
    pub name: String,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-implementation"}}}
impl ZObjectStore {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-struct-impl-new"}}}
    /// Inter a new 'Object Store' in the store, and return it's `id`.
    pub fn new(domain: String, name: String, store: &mut LuDogStore) -> Arc<RwLock<ZObjectStore>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(ZObjectStore { domain, id, name }));
        store.inter_z_object_store(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-struct-impl-nav-backward-one-bi-cond-to-implementation_block"}}}
    /// Navigate to [`ImplementationBlock`] across R83(1c-1c)
    pub fn r83c_implementation_block<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<ImplementationBlock>>> {
        let implementation_block = store
            .iter_implementation_block()
            .find(|implementation_block| {
                implementation_block.read().unwrap().object_store == Some(self.id)
            });
        match implementation_block {
            Some(ref implementation_block) => vec![implementation_block.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-struct-impl-nav-backward-assoc-many-to-object_wrapper"}}}
    /// Navigate to [`ObjectWrapper`] across R78(1-M)
    pub fn r78_object_wrapper<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<ObjectWrapper>>> {
        store
            .iter_object_wrapper()
            .filter(|object_wrapper| object_wrapper.read().unwrap().z_store == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"z_object_store-impl-nav-subtype-to-supertype-value_type"}}}
    // Navigate to [`ValueType`] across R1(isa)
    pub fn r1_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
