// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"line-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::merlin::types::anchor::Anchor;
use crate::merlin::types::glyph::Glyph;
use crate::merlin::types::line_segment::LineSegment;
use crate::merlin::types::relationship_name::RelationshipName;
use crate::merlin::types::relationship_phrase::RelationshipPhrase;
use crate::sarzak::types::relationship::Relationship;
use serde::{Deserialize, Serialize};

use crate::merlin::store::ObjectStore as MerlinStore;
use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-documentation"}}}
/// Relationship Line
///
/// A line is how we represent a relationship. A line is composed of many [`Line Segment`]-
/// s.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Line {
    pub id: Uuid,
    /// R2: [`Line`] 'represents a' [`Relationship`]
    pub relationship: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-implementation"}}}
impl Line {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-impl-new"}}}
    /// Inter a new 'Line' in the store, and return it's `id`.
    pub fn new(relationship: &Relationship, store: &mut MerlinStore) -> Arc<RwLock<Line>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Line {
            id,
            relationship: relationship.id(),
        }));
        store.inter_line(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-impl-nav-forward-to-relationship"}}}
    /// Navigate to [`Relationship`] across R2(1-*)
    pub fn r2_relationship<'a>(
        &'a self,
        store: &'a SarzakStore,
    ) -> Vec<std::sync::Arc<std::sync::RwLock<Relationship>>> {
        vec![store.exhume_relationship(&self.relationship).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-impl-nav-backward-cond-to-glyph"}}}
    /// Navigate to [`Glyph`] across R16(1-1c)
    pub fn r16c_glyph<'a>(&'a self, store: &'a MerlinStore) -> Vec<Arc<RwLock<Glyph>>> {
        let glyph = store
            .iter_glyph()
            .find(|glyph| glyph.read().unwrap().line == self.id);
        match glyph {
            Some(ref glyph) => vec![glyph.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-impl-nav-backward-one-to-line_segment"}}}
    /// Navigate to [`LineSegment`] across R4(1-1)
    pub fn r4_line_segment<'a>(&'a self, store: &'a MerlinStore) -> Vec<Arc<RwLock<LineSegment>>> {
        vec![store
            .iter_line_segment()
            .find(|line_segment| line_segment.read().unwrap().line == self.id)
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-impl-nav-backward-one-to-relationship_name"}}}
    /// Navigate to [`RelationshipName`] across R11(1-1)
    pub fn r11_relationship_name<'a>(
        &'a self,
        store: &'a MerlinStore,
    ) -> Vec<Arc<RwLock<RelationshipName>>> {
        vec![store
            .iter_relationship_name()
            .find(|relationship_name| relationship_name.read().unwrap().line == self.id)
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-impl-nav-backward-1_M-to-relationship_phrase"}}}
    /// Navigate to [`RelationshipPhrase`] across R12(1-M)
    pub fn r12_relationship_phrase<'a>(
        &'a self,
        store: &'a MerlinStore,
    ) -> Vec<Arc<RwLock<RelationshipPhrase>>> {
        store
            .iter_relationship_phrase()
            .filter(|relationship_phrase| relationship_phrase.read().unwrap().line == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"line-struct-impl-nav-backward-assoc-many-to-anchor"}}}
    /// Navigate to [`Anchor`] across R3(1-M)
    pub fn r3_anchor<'a>(&'a self, store: &'a MerlinStore) -> Vec<Arc<RwLock<Anchor>>> {
        store
            .iter_anchor()
            .filter(|anchor| anchor.read().unwrap().line == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
