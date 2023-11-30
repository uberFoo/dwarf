// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"implementation_block-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::enumeration::Enumeration;
use crate::lu_dog::types::function::Function;
use crate::lu_dog::types::item::Item;
use crate::lu_dog::types::item::ItemEnum;
use crate::lu_dog::types::woog_struct::WoogStruct;
use crate::lu_dog::types::z_object_store::ZObjectStore;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-struct-documentation"}}}
/// An Implementation Block
///
/// Inside this block functions are defined on a [`ModellType`].
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ImplementationBlock {
    pub id: Uuid,
    /// R8: [`ImplementationBlock`] 'adds functions to a' [`WoogStruct`]
    pub model_type: Option<Uuid>,
    /// R83: [`ImplementationBlock`] 'may refer to an' [`ZObjectStore`]
    pub object_store: Option<Uuid>,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-implementation"}}}
impl ImplementationBlock {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-struct-impl-new"}}}
    /// Inter a new 'Implementation Block' in the store, and return it's `id`.
    pub fn new(
        model_type: Option<&Arc<RwLock<WoogStruct>>>,
        object_store: Option<&Arc<RwLock<ZObjectStore>>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<ImplementationBlock>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(ImplementationBlock {
            id,
            model_type: model_type.map(|woog_struct| woog_struct.read().unwrap().id),
            object_store: object_store.map(|z_object_store| z_object_store.read().unwrap().id),
        }));
        store.inter_implementation_block(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-struct-impl-nav-forward-cond-to-model_type"}}}
    /// Navigate to [`WoogStruct`] across R8(1-*c)
    pub fn r8_woog_struct<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<WoogStruct>>> {
        match self.model_type {
            Some(ref model_type) => vec![store.exhume_woog_struct(&model_type).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-struct-impl-nav-forward-cond-to-object_store"}}}
    /// Navigate to [`ZObjectStore`] across R83(1-*c)
    pub fn r83_z_object_store<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<ZObjectStore>>> {
        match self.object_store {
            Some(ref object_store) => vec![store.exhume_z_object_store(&object_store).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-struct-impl-nav-backward-one-bi-cond-to-enumeration"}}}
    /// Navigate to [`Enumeration`] across R84(1c-1c)
    pub fn r84c_enumeration<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Enumeration>>> {
        let enumeration = store
            .iter_enumeration()
            .find(|enumeration| enumeration.read().unwrap().implementation == Some(self.id));
        match enumeration {
            Some(ref enumeration) => vec![enumeration.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-struct-impl-nav-backward-1_Mc-to-function"}}}
    /// Navigate to [`Function`] across R9(1-Mc)
    pub fn r9_function<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Function>>> {
        store
            .iter_function()
            .filter(|function| function.read().unwrap().impl_block == Some(self.id))
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"implementation_block-impl-nav-subtype-to-supertype-item"}}}
    // Navigate to [`Item`] across R6(isa)
    pub fn r6_item<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Item>>> {
        vec![store
            .iter_item()
            .find(|item| {
                if let ItemEnum::ImplementationBlock(id) = item.read().unwrap().subtype {
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
