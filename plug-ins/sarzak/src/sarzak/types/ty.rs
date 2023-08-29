// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"ty-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"ty-use-statements"}}}
use crate::sarzak::store::ObjectStore as SarzakStore;
use crate::sarzak::types::attribute::Attribute;
use crate::sarzak::types::boolean::BOOLEAN;
use crate::sarzak::types::external::External;
use crate::sarzak::types::float::FLOAT;
use crate::sarzak::types::integer::INTEGER;
use crate::sarzak::types::object::Object;
use crate::sarzak::types::s_string::S_STRING;
use crate::sarzak::types::s_uuid::S_UUID;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::rc::Rc;
use tracy_client::span;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"ty-enum-documentation"}}}
/// The type of a value
///
/// There are several values available: [Integer], [Boolean], [Float], [String], and [UUID]
/// .
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"ty-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Ty {
    Boolean(Uuid),
    External(Uuid),
    Float(Uuid),
    Integer(Uuid),
    Object(Uuid),
    SString(Uuid),
    SUuid(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"ty-implementation"}}}
impl Ty {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"ty-new-impl"}}}
    /// Create a new instance of Ty::Boolean
    pub fn new_boolean(store: &SarzakStore) -> Rc<RefCell<Self>> {
        // This is already in the store.
        store.exhume_ty(&BOOLEAN).unwrap()
    }

    /// Create a new instance of Ty::External
    pub fn new_external(
        external: &Rc<RefCell<External>>,
        store: &mut SarzakStore,
    ) -> Rc<RefCell<Self>> {
        let id = external.borrow().id;
        if let Some(external) = store.exhume_ty(&id) {
            external
        } else {
            let new = Rc::new(RefCell::new(Self::External(id)));
            store.inter_ty(new.clone());
            new
        }
    }

    /// Create a new instance of Ty::Float
    pub fn new_float(store: &SarzakStore) -> Rc<RefCell<Self>> {
        // This is already in the store.
        store.exhume_ty(&FLOAT).unwrap()
    }

    /// Create a new instance of Ty::Integer
    pub fn new_integer(store: &SarzakStore) -> Rc<RefCell<Self>> {
        // This is already in the store.
        store.exhume_ty(&INTEGER).unwrap()
    }

    /// Create a new instance of Ty::Object
    pub fn new_object(object: &Rc<RefCell<Object>>, store: &mut SarzakStore) -> Rc<RefCell<Self>> {
        let id = object.borrow().id;
        if let Some(object) = store.exhume_ty(&id) {
            object
        } else {
            let new = Rc::new(RefCell::new(Self::Object(id)));
            store.inter_ty(new.clone());
            new
        }
    }

    /// Create a new instance of Ty::SString
    pub fn new_s_string(store: &SarzakStore) -> Rc<RefCell<Self>> {
        // This is already in the store.
        store.exhume_ty(&S_STRING).unwrap()
    }

    /// Create a new instance of Ty::SUuid
    pub fn new_s_uuid(store: &SarzakStore) -> Rc<RefCell<Self>> {
        // This is already in the store.
        store.exhume_ty(&S_UUID).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"ty-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Boolean(id) => *id,
            Self::External(id) => *id,
            Self::Float(id) => *id,
            Self::Integer(id) => *id,
            Self::Object(id) => *id,
            Self::SString(id) => *id,
            Self::SUuid(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"ty-struct-impl-nav-backward-one-to-attribute"}}}
    /// Navigate to [`Attribute`] across R2(1-1)
    pub fn r2_attribute<'a>(&'a self, store: &'a SarzakStore) -> Vec<Rc<RefCell<Attribute>>> {
        span!("r2_attribute");
        vec![store
            .iter_attribute()
            .find(|attribute| attribute.borrow().ty == self.id())
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
