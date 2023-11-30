// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"generic-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::value_type::ValueType;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-struct-documentation"}}}
/// This is a generic “type”.
///
/// It’s really a placeholder in the extruder/compiler. We’ll use it as a type declaration
/// , and then define a new type for each use.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Generic {
    pub id: Uuid,
    pub name: String,
    /// R3: [`Generic`] 'next' [`Generic`]
    pub next: Option<Uuid>,
    /// R99: [`Generic`] 'has an inner' [`ValueType`]
    pub ty: Option<Uuid>,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-implementation"}}}
impl Generic {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-struct-impl-new"}}}
    /// Inter a new 'Generic' in the store, and return it's `id`.
    pub fn new(
        name: String,
        next: Option<&Arc<RwLock<Generic>>>,
        ty: Option<&Arc<RwLock<ValueType>>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Generic>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Generic {
            id,
            name,
            next: next.map(|generic| generic.read().unwrap().id),
            ty: ty.map(|value_type| value_type.read().unwrap().id()),
        }));
        store.inter_generic(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-struct-impl-nav-forward-cond-to-next"}}}
    /// Navigate to [`Generic`] across R3(1-*c)
    pub fn r3_generic<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Generic>>> {
        match self.next {
            Some(ref next) => vec![store.exhume_generic(&next).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-struct-impl-nav-forward-cond-to-ty"}}}
    /// Navigate to [`ValueType`] across R99(1-*c)
    pub fn r99_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        match self.ty {
            Some(ref ty) => vec![store.exhume_value_type(&ty).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-struct-impl-nav-backward-one-bi-cond-to-generic"}}}
    /// Navigate to [`Generic`] across R3(1c-1c)
    pub fn r3c_generic<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Generic>>> {
        let generic = store
            .iter_generic()
            .find(|generic| generic.read().unwrap().next == Some(self.id));
        match generic {
            Some(ref generic) => vec![generic.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"generic-impl-nav-subtype-to-supertype-value_type"}}}
    // Navigate to [`ValueType`] across R1(isa)
    pub fn r1_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
