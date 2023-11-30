// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"struct_generic-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::woog_struct::WoogStruct;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-struct-documentation"}}}
/// A generic type applied to the struct.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct StructGeneric {
    pub id: Uuid,
    pub name: String,
    /// R101: [`StructGeneric`] '' [`StructGeneric`]
    pub next: Option<Uuid>,
    /// R100: [`StructGeneric`] 'applies to a' [`WoogStruct`]
    pub woog_struct: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-implementation"}}}
impl StructGeneric {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-struct-impl-new"}}}
    /// Inter a new 'Struct Generic' in the store, and return it's `id`.
    pub fn new(
        name: String,
        next: Option<&Arc<RwLock<StructGeneric>>>,
        woog_struct: &Arc<RwLock<WoogStruct>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<StructGeneric>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(StructGeneric {
            id,
            name,
            next: next.map(|struct_generic| struct_generic.read().unwrap().id),
            woog_struct: woog_struct.read().unwrap().id,
        }));
        store.inter_struct_generic(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-struct-impl-nav-forward-cond-to-next"}}}
    /// Navigate to [`StructGeneric`] across R101(1-*c)
    pub fn r101_struct_generic<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<StructGeneric>>> {
        match self.next {
            Some(ref next) => vec![store.exhume_struct_generic(&next).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-struct-impl-nav-forward-to-woog_struct"}}}
    /// Navigate to [`WoogStruct`] across R100(1-*)
    pub fn r100_woog_struct<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<WoogStruct>>> {
        vec![store.exhume_woog_struct(&self.woog_struct).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-struct-impl-nav-backward-one-to-woog_struct"}}}
    /// Navigate to [`WoogStruct`] across R102(1-1)
    pub fn r102_woog_struct<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<WoogStruct>>> {
        vec![store
            .iter_woog_struct()
            .find(|woog_struct| woog_struct.read().unwrap().first_generic == Some(self.id))
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"struct_generic-struct-impl-nav-backward-one-bi-cond-to-struct_generic"}}}
    /// Navigate to [`StructGeneric`] across R101(1c-1c)
    pub fn r101c_struct_generic<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<StructGeneric>>> {
        let struct_generic = store
            .iter_struct_generic()
            .find(|struct_generic| struct_generic.read().unwrap().next == Some(self.id));
        match struct_generic {
            Some(ref struct_generic) => vec![struct_generic.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
