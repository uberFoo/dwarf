// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"event-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"event-use-statements"}}}
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;

use crate::sarzak::types::acknowledged_event::AcknowledgedEvent;
use crate::sarzak::types::object::Object;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"event-struct-documentation"}}}
/// An event is sent to an object, and processed by the current state. Assuming it accepts the
///  event. Otherwise it’s dropped on the floor.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"event-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Event {
    pub id: Uuid,
    pub name: String,
    /// R19: [`Event`] 'triggers state transitions on' [`Object`]
    pub obj_id: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"event-implementation"}}}
impl Event {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"event-struct-impl-new"}}}
    /// Inter a new 'Event' in the store, and return it's `id`.
    pub fn new(
        name: String,
        obj_id: &Rc<RefCell<Object>>,
        store: &mut SarzakStore,
    ) -> Rc<RefCell<Event>> {
        let id = Uuid::new_v4();
        let new = Rc::new(RefCell::new(Event {
            id,
            name,
            obj_id: obj_id.borrow().id,
        }));
        store.inter_event(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"event-struct-impl-nav-forward-to-obj_id"}}}
    /// Navigate to [`Object`] across R19(1-*)
    pub fn r19_object<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Object>>> {
        span!("r19_object");
        vec![store.exhume_object(&self.obj_id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"event-struct-impl-nav-backward-assoc-many-to-acknowledged_event"}}}
    /// Navigate to [`AcknowledgedEvent`] across R20(1-M)
    pub fn r20_acknowledged_event<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<Rc<RefCell<AcknowledgedEvent>>> {
        span!("r20_acknowledged_event");
        store
            .iter_acknowledged_event()
            .filter(|acknowledged_event| acknowledged_event.borrow().event_id == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
