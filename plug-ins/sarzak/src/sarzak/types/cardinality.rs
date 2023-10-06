// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"cardinality-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-use-statements"}}}
use crate::sarzak::store::ObjectStore as SarzakStore;
use crate::sarzak::types::associative_referent::AssociativeReferent;
use crate::sarzak::types::associative_referrer::AssociativeReferrer;
use crate::sarzak::types::many::MANY;
use crate::sarzak::types::one::ONE;
use crate::sarzak::types::referent::Referent;
use crate::sarzak::types::referrer::Referrer;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use tracy_client::span;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Cardinality {
    Many(Uuid),
    One(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-implementation"}}}
impl Cardinality {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-new-impl"}}}
    /// Create a new instance of Cardinality::Many
    pub fn new_many(store: &SarzakStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_cardinality(&MANY).unwrap()
    }

    /// Create a new instance of Cardinality::One
    pub fn new_one(store: &SarzakStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_cardinality(&ONE).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Many(id) => *id,
            Self::One(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-struct-impl-nav-backward-1_M-to-associative_referent"}}}
    /// Navigate to [`AssociativeReferent`] across R88(1-M)
    pub fn r88_associative_referent<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<Arc<RwLock<AssociativeReferent>>> {
        span!("r88_associative_referent");
        store
            .iter_associative_referent()
            .filter(|associative_referent| {
                associative_referent.read().unwrap().cardinality == self.id()
            })
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-struct-impl-nav-backward-1_M-to-associative_referrer"}}}
    /// Navigate to [`AssociativeReferrer`] across R89(1-M)
    pub fn r89_associative_referrer<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<Arc<RwLock<AssociativeReferrer>>> {
        span!("r89_associative_referrer");
        store
            .iter_associative_referrer()
            .filter(|associative_referrer| {
                associative_referrer.read().unwrap().cardinality == self.id()
            })
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-struct-impl-nav-backward-1_M-to-referent"}}}
    /// Navigate to [`Referent`] across R8(1-M)
    pub fn r8_referent<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Referent>>> {
        span!("r8_referent");
        store
            .iter_referent()
            .filter(|referent| referent.read().unwrap().cardinality == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"cardinality-struct-impl-nav-backward-1_M-to-referrer"}}}
    /// Navigate to [`Referrer`] across R9(1-M)
    pub fn r9_referrer<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Referrer>>> {
        span!("r9_referrer");
        store
            .iter_referrer()
            .filter(|referrer| referrer.read().unwrap().cardinality == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
