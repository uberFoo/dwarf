// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"edge-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"edge-use-statements"}}}
use crate::merlin::store::ObjectStore as MerlinStore;
use crate::merlin::types::anchor::Anchor;
use crate::merlin::types::bottom::BOTTOM;
use crate::merlin::types::left::LEFT;
use crate::merlin::types::right::RIGHT;
use crate::merlin::types::top::TOP;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use tracy_client::span;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"edge-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Edge {
    Bottom(Uuid),
    Left(Uuid),
    Right(Uuid),
    Top(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"edge-implementation"}}}
impl Edge {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"edge-new-impl"}}}
    /// Create a new instance of Edge::Bottom
    pub fn new_bottom(store: &MerlinStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_edge(&BOTTOM).unwrap()
    }

    /// Create a new instance of Edge::Left
    pub fn new_left(store: &MerlinStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_edge(&LEFT).unwrap()
    }

    /// Create a new instance of Edge::Right
    pub fn new_right(store: &MerlinStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_edge(&RIGHT).unwrap()
    }

    /// Create a new instance of Edge::Top
    pub fn new_top(store: &MerlinStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_edge(&TOP).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"edge-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Bottom(id) => *id,
            Self::Left(id) => *id,
            Self::Right(id) => *id,
            Self::Top(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"edge-struct-impl-nav-backward-1_M-to-anchor"}}}
    /// Navigate to [`Anchor`] across R9(1-M)
    pub fn r9_anchor<'a>(&'a self, store: &'a MerlinStore) -> Vec<Arc<RwLock<Anchor>>> {
        span!("r9_anchor");
        store
            .iter_anchor()
            .filter(|anchor| anchor.read().unwrap().edge == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
