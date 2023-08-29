// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"object-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-use-statements"}}}
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;

use crate::sarzak::types::associative_referent::AssociativeReferent;
use crate::sarzak::types::associative_referrer::AssociativeReferrer;
use crate::sarzak::types::attribute::Attribute;
use crate::sarzak::types::event::Event;
use crate::sarzak::types::referent::Referent;
use crate::sarzak::types::referrer::Referrer;
use crate::sarzak::types::state::State;
use crate::sarzak::types::subtype::Subtype;
use crate::sarzak::types::supertype::Supertype;
use crate::sarzak::types::ty::Ty;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-documentation"}}}
/// An `Object` is a collection of related data. By creating `Object`s, and
/// connecting them with `Relationships` we build a powerful abstraction.
///
/// `Object`s contain [Attribute]s that represent the data that the
/// `Object`encapsulates. All `Object`s have an attribute called `id`, which
/// is a unique identifier for each class of `Object`. The `id` attribute is a
/// version 5 UUID.
///
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Object {
    pub description: String,
    pub id: Uuid,
    pub key_letters: String,
    pub name: String,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-implementation"}}}
impl Object {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-new"}}}
    /// Inter a new 'Object' in the store, and return it's `id`.
    pub fn new(
        description: String,
        key_letters: String,
        name: String,
        store: &mut SarzakStore,
    ) -> Rc<RefCell<Object>> {
        let id = Uuid::new_v4();
        let new = Rc::new(RefCell::new(Object {
            description,
            id,
            key_letters,
            name,
        }));
        store.inter_object(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-associative_referent"}}}
    /// Navigate to [`AssociativeReferent`] across R25(1-M)
    pub fn r25_associative_referent<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<Rc<RefCell<AssociativeReferent>>> {
        span!("r25_associative_referent");
        store
            .iter_associative_referent()
            .filter(|associative_referent| associative_referent.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-associative_referrer"}}}
    /// Navigate to [`AssociativeReferrer`] across R26(1-M)
    pub fn r26_associative_referrer<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<Rc<RefCell<AssociativeReferrer>>> {
        span!("r26_associative_referrer");
        store
            .iter_associative_referrer()
            .filter(|associative_referrer| associative_referrer.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-attribute"}}}
    /// Navigate to [`Attribute`] across R1(1-M)
    pub fn r1_attribute<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Attribute>>> {
        span!("r1_attribute");
        store
            .iter_attribute()
            .filter(|attribute| attribute.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-event"}}}
    /// Navigate to [`Event`] across R19(1-M)
    pub fn r19_event<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Event>>> {
        span!("r19_event");
        store
            .iter_event()
            .filter(|event| event.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-referent"}}}
    /// Navigate to [`Referent`] across R16(1-M)
    pub fn r16_referent<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Referent>>> {
        span!("r16_referent");
        store
            .iter_referent()
            .filter(|referent| referent.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-referrer"}}}
    /// Navigate to [`Referrer`] across R17(1-M)
    pub fn r17_referrer<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Referrer>>> {
        span!("r17_referrer");
        store
            .iter_referrer()
            .filter(|referrer| referrer.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-state"}}}
    /// Navigate to [`State`] across R18(1-M)
    pub fn r18_state<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<State>>> {
        span!("r18_state");
        store
            .iter_state()
            .filter(|state| state.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-subtype"}}}
    /// Navigate to [`Subtype`] across R15(1-M)
    pub fn r15_subtype<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Subtype>>> {
        span!("r15_subtype");
        store
            .iter_subtype()
            .filter(|subtype| subtype.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-struct-impl-nav-backward-1_M-to-supertype"}}}
    /// Navigate to [`Supertype`] across R14(1-M)
    pub fn r14_supertype<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Supertype>>> {
        span!("r14_supertype");
        store
            .iter_supertype()
            .filter(|supertype| supertype.borrow().obj_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"object-impl-nav-subtype-to-supertype-ty"}}}
    // Navigate to [`Ty`] across R3(isa)
    pub fn r3_ty<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Ty>>> {
        span!("r3_ty");
        vec![store.exhume_ty(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
