// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"x_box-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_box-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use tracy_client::span;
use uuid::Uuid;

use crate::merlin::types::anchor::Anchor;
use crate::sarzak::types::object::Object;
use serde::{Deserialize, Serialize};

use crate::merlin::store::ObjectStore as MerlinStore;
use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_box-struct-documentation"}}}
/// More than a box
///
/// This is the primary method of drawing an Object on the screen. I'm sure it'll be used for
///  State's as well.
///
/// It's a rectangle with parameters.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_box-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct XBox {
    pub height: i64,
    pub id: Uuid,
    pub width: i64,
    pub x: i64,
    pub y: i64,
    /// R1: [`XBox`] 'renders an' [`Object`]
    pub object: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_box-implementation"}}}
impl XBox {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_box-struct-impl-new"}}}
    /// Inter a new 'Box' in the store, and return it's `id`.
    pub fn new(
        height: i64,
        width: i64,
        x: i64,
        y: i64,
        object: &Object,
        store: &mut MerlinStore,
    ) -> Arc<RwLock<XBox>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(XBox {
            height,
            id,
            width,
            x,
            y,
            object: object.id,
        }));
        store.inter_x_box(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_box-struct-impl-nav-forward-to-object"}}}
    /// Navigate to [`Object`] across R1(1-*)
    pub fn r1_object<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<std::sync::Arc<std::sync::RwLock<Object>>> {
        span!("r1_object");
        vec![store.exhume_object(&self.object).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_box-struct-impl-nav-backward-assoc-many-to-anchor"}}}
    /// Navigate to [`Anchor`] across R3(1-M)
    pub fn r3_anchor<'a>(&'a self, store: &'a MerlinStore) -> Vec<Arc<RwLock<Anchor>>> {
        span!("r3_anchor");
        store
            .iter_anchor()
            .filter(|anchor| anchor.read().unwrap().x_box == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
