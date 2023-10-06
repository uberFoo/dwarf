// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"binary-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use tracy_client::span;
use uuid::Uuid;

use crate::sarzak::types::referent::Referent;
use crate::sarzak::types::referrer::Referrer;
use crate::sarzak::types::relationship::Relationship;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-struct-documentation"}}}
/// A `Binary` relationship, as it’s name implies, is a relationship between
/// two objects. It consists of two parts, the `Dependent` end of the
/// relationship and the `Independent` end.
///
/// The former is so named because it has the job of formalizing the
/// relationship. It stores a pointer to the independent object as an attribute.
///
/// The latter is aware of the relationship, but it does not store any
/// information about the relationship. That said, there are means of
/// traversing the relationship from the `Independent` object.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Binary {
    pub id: Uuid,
    pub number: i64,
    /// R6: [`Binary`] 'is formalized by' [`Referrer`]
    pub from: Uuid,
    /// R5: [`Binary`] 'loops in the' [`Referent`]
    pub to: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-implementation"}}}
impl Binary {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-struct-impl-new"}}}
    /// Inter a new 'Binary' in the store, and return it's `id`.
    pub fn new(
        number: i64,
        from: &Arc<RwLock<Referrer>>,
        to: &Arc<RwLock<Referent>>,
        store: &mut SarzakStore,
    ) -> Arc<RwLock<Binary>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Binary {
            id,
            number,
            from: from.read().unwrap().id,
            to: to.read().unwrap().id,
        }));
        store.inter_binary(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-struct-impl-nav-forward-to-from"}}}
    /// Navigate to [`Referrer`] across R6(1-*)
    pub fn r6_referrer<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Referrer>>> {
        span!("r6_referrer");
        vec![store.exhume_referrer(&self.from).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-struct-impl-nav-forward-to-to"}}}
    /// Navigate to [`Referent`] across R5(1-*)
    pub fn r5_referent<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Referent>>> {
        span!("r5_referent");
        vec![store.exhume_referent(&self.to).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"binary-impl-nav-subtype-to-supertype-relationship"}}}
    // Navigate to [`Relationship`] across R4(isa)
    pub fn r4_relationship<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Relationship>>> {
        span!("r4_relationship");
        vec![store.exhume_relationship(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
