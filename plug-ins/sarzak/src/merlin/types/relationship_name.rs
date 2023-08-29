// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"relationship_name-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship_name-use-statements"}}}
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;

use crate::merlin::types::bisection::Bisection;
use crate::merlin::types::line::Line;
use serde::{Deserialize, Serialize};

use crate::merlin::store::ObjectStore as MerlinStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship_name-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct RelationshipName {
    pub id: Uuid,
    pub text: String,
    pub x: i64,
    pub y: i64,
    /// R11: [`RelationshipName`] 'is derived from' [`Line`]
    pub line: Uuid,
    /// R15: [`RelationshipName`] 'is anchored by' [`Bisection`]
    pub origin: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship_name-implementation"}}}
impl RelationshipName {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship_name-struct-impl-new"}}}
    /// Inter a new 'Relationship Name' in the store, and return it's `id`.
    pub fn new(
        text: String,
        x: i64,
        y: i64,
        line: &Rc<RefCell<Line>>,
        origin: &Rc<RefCell<Bisection>>,
        store: &mut MerlinStore,
    ) -> Rc<RefCell<RelationshipName>> {
        let id = Uuid::new_v4();
        let new = Rc::new(RefCell::new(RelationshipName {
            id,
            text,
            x,
            y,
            line: line.borrow().id,
            origin: origin.borrow().id,
        }));
        store.inter_relationship_name(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship_name-struct-impl-nav-forward-to-line"}}}
    /// Navigate to [`Line`] across R11(1-*)
    pub fn r11_line<'a>(&'a self, store: &'a MerlinStore) -> Vec<Rc<RefCell<Line>>> {
        span!("r11_line");
        vec![store.exhume_line(&self.line).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship_name-struct-impl-nav-forward-to-origin"}}}
    /// Navigate to [`Bisection`] across R15(1-*)
    pub fn r15_bisection<'a>(&'a self, store: &'a MerlinStore) -> Vec<Rc<RefCell<Bisection>>> {
        span!("r15_bisection");
        vec![store.exhume_bisection(&self.origin).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
