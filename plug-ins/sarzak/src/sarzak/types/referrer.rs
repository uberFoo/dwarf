// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"referrer-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use tracy_client::span;
use uuid::Uuid;

use crate::sarzak::types::binary::Binary;
use crate::sarzak::types::cardinality::Cardinality;
use crate::sarzak::types::conditionality::Conditionality;
use crate::sarzak::types::object::Object;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-struct-documentation"}}}
/// This is the side of a binary relationship that is doing the pointing, thus it contains the
///  referential attribute. It is connected to the “from” side of a binary relationship.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Referrer {
    pub description: String,
    pub id: Uuid,
    pub referential_attribute: String,
    /// R9: [`Referrer`] 'has' [`Cardinality`]
    pub cardinality: Uuid,
    /// R11: [`Referrer`] 'has' [`Conditionality`]
    pub conditionality: Uuid,
    /// R17: [`Referrer`] 'is an instance of an' [`Object`]
    pub obj_id: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-implementation"}}}
impl Referrer {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-struct-impl-new"}}}
    /// Inter a new 'Referrer' in the store, and return it's `id`.
    pub fn new(
        description: String,
        referential_attribute: String,
        cardinality: &Arc<RwLock<Cardinality>>,
        conditionality: &Arc<RwLock<Conditionality>>,
        obj_id: &Arc<RwLock<Object>>,
        store: &mut SarzakStore,
    ) -> Arc<RwLock<Referrer>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Referrer {
            description,
            id,
            referential_attribute,
            cardinality: cardinality.read().unwrap().id(),
            conditionality: conditionality.read().unwrap().id(),
            obj_id: obj_id.read().unwrap().id,
        }));
        store.inter_referrer(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-struct-impl-nav-forward-to-cardinality"}}}
    /// Navigate to [`Cardinality`] across R9(1-*)
    pub fn r9_cardinality<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Cardinality>>> {
        span!("r9_cardinality");
        vec![store.exhume_cardinality(&self.cardinality).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-struct-impl-nav-forward-to-conditionality"}}}
    /// Navigate to [`Conditionality`] across R11(1-*)
    pub fn r11_conditionality<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<Arc<RwLock<Conditionality>>> {
        span!("r11_conditionality");
        vec![store.exhume_conditionality(&self.conditionality).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-struct-impl-nav-forward-to-obj_id"}}}
    /// Navigate to [`Object`] across R17(1-*)
    pub fn r17_object<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Object>>> {
        span!("r17_object");
        vec![store.exhume_object(&self.obj_id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"referrer-struct-impl-nav-backward-one-to-binary"}}}
    /// Navigate to [`Binary`] across R6(1-1)
    pub fn r6_binary<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Binary>>> {
        span!("r6_binary");
        vec![store
            .iter_binary()
            .find(|binary| binary.read().unwrap().from == self.id)
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
