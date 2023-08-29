// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"associative_referrer-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-use-statements"}}}
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;

use crate::sarzak::types::associative::Associative;
use crate::sarzak::types::cardinality::Cardinality;
use crate::sarzak::types::object::Object;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-struct-documentation"}}}
/// Associative Object
///
/// This is used in an [`Associative`] relationship to point to the Associative object itself
/// . It's the box with the line pointing at another line.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct AssociativeReferrer {
    pub id: Uuid,
    /// R89: [`AssociativeReferrer`] 'has' [`Cardinality`]
    pub cardinality: Uuid,
    /// R26: [`AssociativeReferrer`] 'is also an' [`Object`]
    pub obj_id: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-implementation"}}}
impl AssociativeReferrer {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-struct-impl-new"}}}
    /// Inter a new 'Associative Referrer' in the store, and return it's `id`.
    pub fn new(
        cardinality: &Rc<RefCell<Cardinality>>,
        obj_id: &Rc<RefCell<Object>>,
        store: &mut SarzakStore,
    ) -> Rc<RefCell<AssociativeReferrer>> {
        let id = Uuid::new_v4();
        let new = Rc::new(RefCell::new(AssociativeReferrer {
            id,
            cardinality: cardinality.borrow().id(),
            obj_id: obj_id.borrow().id,
        }));
        store.inter_associative_referrer(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-struct-impl-nav-forward-to-cardinality"}}}
    /// Navigate to [`Cardinality`] across R89(1-*)
    pub fn r89_cardinality<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Cardinality>>> {
        span!("r89_cardinality");
        vec![store.exhume_cardinality(&self.cardinality).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-struct-impl-nav-forward-to-obj_id"}}}
    /// Navigate to [`Object`] across R26(1-*)
    pub fn r26_object<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Object>>> {
        span!("r26_object");
        vec![store.exhume_object(&self.obj_id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"associative_referrer-struct-impl-nav-backward-one-to-associative"}}}
    /// Navigate to [`Associative`] across R21(1-1)
    pub fn r21_associative<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Associative>>> {
        span!("r21_associative");
        vec![store
            .iter_associative()
            .find(|associative| associative.borrow().from == self.id)
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
