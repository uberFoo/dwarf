// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"acknowledged_event-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"acknowledged_event-use-statements"}}}
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;

use crate::sarzak::types::event::Event;
use crate::sarzak::types::state::State;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"acknowledged_event-struct-documentation"}}}
/// An Event that Does Something
///
/// An acknowledged event is an event that a [`State`] knows how to handle.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"acknowledged_event-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct AcknowledgedEvent {
    pub id: Uuid,
    /// R20: [`Event`] '🚧 Comments are out of order — see sarzak#14.' [`Event`]
    pub event_id: Uuid,
    /// R20: [`State`] '🚧 Comments are out of order — see sarzak#14.' [`State`]
    pub state_id: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"acknowledged_event-implementation"}}}
impl AcknowledgedEvent {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"acknowledged_event-struct-impl-new"}}}
    /// Inter a new 'Acknowledged Event' in the store, and return it's `id`.
    pub fn new(
        event_id: &Rc<RefCell<Event>>,
        state_id: &Rc<RefCell<State>>,
        store: &mut SarzakStore,
    ) -> Rc<RefCell<AcknowledgedEvent>> {
        let id = Uuid::new_v4();
        let new = Rc::new(RefCell::new(AcknowledgedEvent {
            id,
            event_id: event_id.borrow().id,
            state_id: state_id.borrow().id,
        }));
        store.inter_acknowledged_event(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"acknowledged_event-struct-impl-nav-forward-assoc-to-event_id"}}}
    /// Navigate to [`Event`] across R20(1-*)
    pub fn r20_event<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Event>>> {
        span!("r20_event");
        vec![store.exhume_event(&self.event_id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"acknowledged_event-struct-impl-nav-forward-assoc-to-state_id"}}}
    /// Navigate to [`State`] across R20(1-*)
    pub fn r20_state<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<State>>> {
        span!("r20_state");
        vec![store.exhume_state(&self.state_id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
