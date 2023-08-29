// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"relationship-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship-use-statements"}}}
use crate::sarzak::store::ObjectStore as SarzakStore;
use crate::sarzak::types::associative::Associative;
use crate::sarzak::types::binary::Binary;
use crate::sarzak::types::isa::Isa;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship-enum-documentation"}}}
/// A `Relationship` indicates that a set of objects are connected to each other in some manner
/// . Typically it is a _real world_ relationship. In the
/// case of this model it is strictly an abstraction.
///
/// There are three types of `Relationship`: [`Isa`], [`Binary`], and [`Associative`]. Thus
///  `Relationship` is itself the *supertype* in an [`Isa`] relationship. It is a partitioning
///  *supertype-subtype* relationship, rather one of inheritance. As such, it’s  perfectly
///  suited to a rust `enum`! 😃
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Relationship {
    Associative(Uuid),
    Binary(Uuid),
    Isa(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship-implementation"}}}
impl Relationship {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship-new-impl"}}}
    /// Create a new instance of Relationship::Associative
    pub fn new_associative(
        associative: &Rc<RefCell<Associative>>,
        store: &mut SarzakStore,
    ) -> Rc<RefCell<Self>> {
        let id = associative.borrow().id;
        if let Some(associative) = store.exhume_relationship(&id) {
            associative
        } else {
            let new = Rc::new(RefCell::new(Self::Associative(id)));
            store.inter_relationship(new.clone());
            new
        }
    }

    /// Create a new instance of Relationship::Binary
    pub fn new_binary(binary: &Rc<RefCell<Binary>>, store: &mut SarzakStore) -> Rc<RefCell<Self>> {
        let id = binary.borrow().id;
        if let Some(binary) = store.exhume_relationship(&id) {
            binary
        } else {
            let new = Rc::new(RefCell::new(Self::Binary(id)));
            store.inter_relationship(new.clone());
            new
        }
    }

    /// Create a new instance of Relationship::Isa
    pub fn new_isa(isa: &Rc<RefCell<Isa>>, store: &mut SarzakStore) -> Rc<RefCell<Self>> {
        let id = isa.borrow().id;
        if let Some(isa) = store.exhume_relationship(&id) {
            isa
        } else {
            let new = Rc::new(RefCell::new(Self::Isa(id)));
            store.inter_relationship(new.clone());
            new
        }
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"relationship-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Associative(id) => *id,
            Self::Binary(id) => *id,
            Self::Isa(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
