// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"attribute-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"attribute-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use tracy_client::span;
use uuid::Uuid;

use crate::sarzak::types::object::Object;
use crate::sarzak::types::ty::Ty;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"attribute-struct-documentation"}}}
/// An `Attribute` represents a single value. Each value must have a
/// [`Type`], which constrains the values of data that may be assigned to
/// an `Attribute`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"attribute-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Attribute {
    pub id: Uuid,
    pub name: String,
    /// R1: [`Attribute`] 'lives in an' [`Object`]
    pub obj_id: Uuid,
    /// R2: [`Attribute`] 'has a' [`Ty`]
    pub ty: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"attribute-implementation"}}}
impl Attribute {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"attribute-struct-impl-new"}}}
    /// Inter a new 'Attribute' in the store, and return it's `id`.
    pub fn new(
        name: String,
        obj_id: &Arc<RwLock<Object>>,
        ty: &Arc<RwLock<Ty>>,
        store: &mut SarzakStore,
    ) -> Arc<RwLock<Attribute>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Attribute {
            id,
            name,
            obj_id: obj_id.read().unwrap().id,
            ty: ty.read().unwrap().id(),
        }));
        store.inter_attribute(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"attribute-struct-impl-nav-forward-to-obj_id"}}}
    /// Navigate to [`Object`] across R1(1-*)
    pub fn r1_object<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Object>>> {
        span!("r1_object");
        vec![store.exhume_object(&self.obj_id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"attribute-struct-impl-nav-forward-to-ty"}}}
    /// Navigate to [`Ty`] across R2(1-*)
    pub fn r2_ty<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Ty>>> {
        span!("r2_ty");
        vec![store.exhume_ty(&self.ty).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
