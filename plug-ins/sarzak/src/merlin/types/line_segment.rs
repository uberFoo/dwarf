// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"line_segment-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-use-statements"}}}
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;

use crate::merlin::types::bisection::Bisection;
use crate::merlin::types::line::Line;
use crate::merlin::types::line_segment_point::LineSegmentPoint;
use serde::{Deserialize, Serialize};

use crate::merlin::store::ObjectStore as MerlinStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-struct-documentation"}}}
/// Part of a Line
///
/// A line segment is in fact a straight line between two points. It is used to compose a (poly
/// ) [`Line`].
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct LineSegment {
    pub id: Uuid,
    /// R4: [`LineSegment`] 'composes' [`Line`]
    pub line: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-implementation"}}}
impl LineSegment {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-struct-impl-new"}}}
    /// Inter a new 'Line Segment' in the store, and return it's `id`.
    pub fn new(line: &Rc<RefCell<Line>>, store: &mut MerlinStore) -> Rc<RefCell<LineSegment>> {
        let id = Uuid::new_v4();
        let new = Rc::new(RefCell::new(LineSegment {
            id,
            line: line.borrow().id,
        }));
        store.inter_line_segment(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-struct-impl-nav-forward-to-line"}}}
    /// Navigate to [`Line`] across R4(1-*)
    pub fn r4_line<'a>(&'a self, store: &'a MerlinStore) -> Vec<Rc<RefCell<Line>>> {
        span!("r4_line");
        vec![store.exhume_line(&self.line).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-struct-impl-nav-backward-cond-to-bisection"}}}
    /// Navigate to [`Bisection`] across R14(1-1c)
    pub fn r14c_bisection<'a>(&'a self, store: &'a MerlinStore) -> Vec<Rc<RefCell<Bisection>>> {
        span!("r14_bisection");
        let bisection = store
            .iter_bisection()
            .find(|bisection| bisection.borrow().segment == self.id);
        match bisection {
            Some(ref bisection) => vec![bisection.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line_segment-struct-impl-nav-backward-assoc-many-to-line_segment_point"}}}
    /// Navigate to [`LineSegmentPoint`] across R5(1-M)
    pub fn r5_line_segment_point<'a>(
        &'a self,
        store: &'a MerlinStore,
    ) -> Vec<Rc<RefCell<LineSegmentPoint>>> {
        span!("r5_line_segment_point");
        store
            .iter_line_segment_point()
            .filter(|line_segment_point| line_segment_point.borrow().segment == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
