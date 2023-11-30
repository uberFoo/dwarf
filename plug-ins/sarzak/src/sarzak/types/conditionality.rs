// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"conditionality-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-use-statements"}}}
use crate::sarzak::store::ObjectStore as SarzakStore;
use crate::sarzak::types::associative_referent::AssociativeReferent;
use crate::sarzak::types::conditional::CONDITIONAL;
use crate::sarzak::types::referent::Referent;
use crate::sarzak::types::referrer::Referrer;
use crate::sarzak::types::unconditional::UNCONDITIONAL;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Conditionality {
    Conditional(Uuid),
    Unconditional(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-implementation"}}}
impl Conditionality {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-new-impl"}}}
    /// Create a new instance of Conditionality::Conditional
    pub fn new_conditional(store: &SarzakStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_conditionality(&CONDITIONAL).unwrap()
    }

    /// Create a new instance of Conditionality::Unconditional
    pub fn new_unconditional(store: &SarzakStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_conditionality(&UNCONDITIONAL).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Conditional(id) => *id,
            Self::Unconditional(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-struct-impl-nav-backward-1_M-to-associative_referent"}}}
    /// Navigate to [`AssociativeReferent`] across R77(1-M)
    pub fn r77_associative_referent<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<Arc<RwLock<AssociativeReferent>>> {
        store
            .iter_associative_referent()
            .filter(|associative_referent| {
                associative_referent.read().unwrap().conditionality == self.id()
            })
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-struct-impl-nav-backward-1_M-to-referent"}}}
    /// Navigate to [`Referent`] across R12(1-M)
    pub fn r12_referent<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Referent>>> {
        store
            .iter_referent()
            .filter(|referent| referent.read().unwrap().conditionality == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"conditionality-struct-impl-nav-backward-1_M-to-referrer"}}}
    /// Navigate to [`Referrer`] across R11(1-M)
    pub fn r11_referrer<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Referrer>>> {
        store
            .iter_referrer()
            .filter(|referrer| referrer.read().unwrap().conditionality == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
