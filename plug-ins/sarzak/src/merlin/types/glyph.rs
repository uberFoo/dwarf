// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"glyph-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use tracy_client::span;
use uuid::Uuid;

use crate::merlin::types::anchor::Anchor;
use crate::merlin::types::line::Line;
use crate::merlin::types::many::MANY;
use crate::merlin::types::one::ONE;
use crate::merlin::types::sub::SUB;
use crate::merlin::types::x_super::X_SUPER;
use serde::{Deserialize, Serialize};

use crate::merlin::store::ObjectStore as MerlinStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-hybrid-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Glyph {
    pub subtype: GlyphEnum,
    pub id: Uuid,
    /// R16: [`Glyph`] 'is determined by' [`Line`]
    pub line: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-hybrid-enum-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum GlyphEnum {
    Many(Uuid),
    One(Uuid),
    Sub(Uuid),
    XSuper(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-implementation"}}}
impl Glyph {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-struct-impl-new_many"}}}
    /// Inter a new Glyph in the store, and return it's `id`.
    pub fn new_many(line: &Arc<RwLock<Line>>, store: &mut MerlinStore) -> Arc<RwLock<Glyph>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Glyph {
            line: line.read().unwrap().id,
            subtype: GlyphEnum::Many(MANY),
            id,
        }));
        store.inter_glyph(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-struct-impl-new_one"}}}
    /// Inter a new Glyph in the store, and return it's `id`.
    pub fn new_one(line: &Arc<RwLock<Line>>, store: &mut MerlinStore) -> Arc<RwLock<Glyph>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Glyph {
            line: line.read().unwrap().id,
            subtype: GlyphEnum::One(ONE),
            id,
        }));
        store.inter_glyph(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-struct-impl-new_sub"}}}
    /// Inter a new Glyph in the store, and return it's `id`.
    pub fn new_sub(line: &Arc<RwLock<Line>>, store: &mut MerlinStore) -> Arc<RwLock<Glyph>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Glyph {
            line: line.read().unwrap().id,
            subtype: GlyphEnum::Sub(SUB),
            id,
        }));
        store.inter_glyph(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-struct-impl-new_x_super"}}}
    /// Inter a new Glyph in the store, and return it's `id`.
    pub fn new_x_super(line: &Arc<RwLock<Line>>, store: &mut MerlinStore) -> Arc<RwLock<Glyph>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Glyph {
            line: line.read().unwrap().id,
            subtype: GlyphEnum::XSuper(X_SUPER),
            id,
        }));
        store.inter_glyph(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-struct-impl-nav-forward-to-line"}}}
    /// Navigate to [`Line`] across R16(1-*)
    pub fn r16_line<'a>(&'a self, store: &'a MerlinStore) -> Vec<Arc<RwLock<Line>>> {
        span!("r16_line");
        vec![store.exhume_line(&self.line).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"glyph-struct-impl-nav-backward-1_M-to-anchor"}}}
    /// Navigate to [`Anchor`] across R10(1-M)
    pub fn r10_anchor<'a>(&'a self, store: &'a MerlinStore) -> Vec<Arc<RwLock<Anchor>>> {
        span!("r10_anchor");
        store
            .iter_anchor()
            .filter(|anchor| anchor.read().unwrap().glyph == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
