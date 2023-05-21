// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"value_type-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-use-statements"}}}
use std::sync::{Arc, RwLock};

use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::empty::EMPTY;
use crate::lu_dog::types::error::Error;
use crate::lu_dog::types::field::Field;
use crate::lu_dog::types::function::Function;
use crate::lu_dog::types::import::Import;
use crate::lu_dog::types::list::List;
use crate::lu_dog::types::range::RANGE;
use crate::lu_dog::types::reference::Reference;
use crate::lu_dog::types::span::Span;
use crate::lu_dog::types::unknown::UNKNOWN;
use crate::lu_dog::types::woog_option::WoogOption;
use crate::lu_dog::types::woog_struct::WoogStruct;
use crate::lu_dog::types::x_value::XValue;
use crate::lu_dog::types::z_object_store::ZObjectStore;
use sarzak::sarzak::types::ty::Ty;
use serde::{Deserialize, Serialize};
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-enum-documentation"}}}
/// Value Type
///
/// This is the main type abstraction used in Lu Dog. We mostly rely on what is available in
///  Sarzak, with two additions: ...
///
/// Two? I know that I need an Option<>. I'm not so sure about a & though. Everything from the
///  store is going to be by UUID, so all of my references are really "pointers" underneath.
///  I want them to be typed in the code though.
///
/// So how will the code work? We could store the type next to the pointer: (type, uuid). Huh
/// . This is the eventual output domain. How does that affect my thinking?
///
/// This should end up looking like woog, but simpler. Woog was for generating rust. I want
///  to generate dwarf. Dwarf needs to be typed? If so, when are they resolved to uuid's eventually
/// ?
///
/// Option for now. We'll see later...
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-enum-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum ValueType {
    Empty(Uuid),
    Error(Uuid),
    Function(Uuid),
    Import(Uuid),
    List(Uuid),
    ZObjectStore(Uuid),
    WoogOption(Uuid),
    Range(Uuid),
    Reference(Uuid),
    WoogStruct(Uuid),
    Ty(Uuid),
    Unknown(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-implementation"}}}
impl ValueType {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-new-impl"}}}
    /// Create a new instance of ValueType::Empty
    pub fn new_empty(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&EMPTY).unwrap()
    }

    /// Create a new instance of ValueType::Error
    pub fn new_error(error: &Arc<RwLock<Error>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        if let Some(error) = store.exhume_value_type(&error.read().unwrap().id()) {
            error
        } else {
            let new = Arc::new(RwLock::new(Self::Error(error.read().unwrap().id())));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::Function
    pub fn new_function(
        function: &Arc<RwLock<Function>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        if let Some(function) = store.exhume_value_type(&function.read().unwrap().id) {
            function
        } else {
            let new = Arc::new(RwLock::new(Self::Function(function.read().unwrap().id)));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::Import
    pub fn new_import(import: &Arc<RwLock<Import>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        if let Some(import) = store.exhume_value_type(&import.read().unwrap().id) {
            import
        } else {
            let new = Arc::new(RwLock::new(Self::Import(import.read().unwrap().id)));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::List
    pub fn new_list(list: &Arc<RwLock<List>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        if let Some(list) = store.exhume_value_type(&list.read().unwrap().id) {
            list
        } else {
            let new = Arc::new(RwLock::new(Self::List(list.read().unwrap().id)));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::ZObjectStore
    pub fn new_z_object_store(
        z_object_store: &Arc<RwLock<ZObjectStore>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        if let Some(z_object_store) = store.exhume_value_type(&z_object_store.read().unwrap().id) {
            z_object_store
        } else {
            let new = Arc::new(RwLock::new(Self::ZObjectStore(
                z_object_store.read().unwrap().id,
            )));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::WoogOption
    pub fn new_woog_option(
        woog_option: &Arc<RwLock<WoogOption>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        if let Some(woog_option) = store.exhume_value_type(&woog_option.read().unwrap().id) {
            woog_option
        } else {
            let new = Arc::new(RwLock::new(Self::WoogOption(
                woog_option.read().unwrap().id,
            )));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::Range
    pub fn new_range(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&RANGE).unwrap()
    }

    /// Create a new instance of ValueType::Reference
    pub fn new_reference(
        reference: &Arc<RwLock<Reference>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        if let Some(reference) = store.exhume_value_type(&reference.read().unwrap().id) {
            reference
        } else {
            let new = Arc::new(RwLock::new(Self::Reference(reference.read().unwrap().id)));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::WoogStruct
    pub fn new_woog_struct(
        woog_struct: &Arc<RwLock<WoogStruct>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        if let Some(woog_struct) = store.exhume_value_type(&woog_struct.read().unwrap().id) {
            woog_struct
        } else {
            let new = Arc::new(RwLock::new(Self::WoogStruct(
                woog_struct.read().unwrap().id,
            )));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::Ty
    pub fn new_ty(ty: &Arc<RwLock<Ty>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        if let Some(ty) = store.exhume_value_type(&ty.read().unwrap().id()) {
            ty
        } else {
            let new = Arc::new(RwLock::new(Self::Ty(ty.read().unwrap().id())));
            store.inter_value_type(new.clone());
            new
        }
    }

    /// Create a new instance of ValueType::Unknown
    pub fn new_unknown(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&UNKNOWN).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            ValueType::Empty(id) => *id,
            ValueType::Error(id) => *id,
            ValueType::Function(id) => *id,
            ValueType::Import(id) => *id,
            ValueType::List(id) => *id,
            ValueType::ZObjectStore(id) => *id,
            ValueType::WoogOption(id) => *id,
            ValueType::Range(id) => *id,
            ValueType::Reference(id) => *id,
            ValueType::WoogStruct(id) => *id,
            ValueType::Ty(id) => *id,
            ValueType::Unknown(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-field"}}}
    /// Navigate to [`Field`] across R5(1-M)
    pub fn r5_field<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Field>>> {
        store
            .iter_field()
            .filter(|field| field.read().unwrap().ty == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-function"}}}
    /// Navigate to [`Function`] across R10(1-M)
    pub fn r10_function<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Function>>> {
        store
            .iter_function()
            .filter(|function| function.read().unwrap().return_type == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-list"}}}
    /// Navigate to [`List`] across R36(1-M)
    pub fn r36_list<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<List>>> {
        store
            .iter_list()
            .filter(|list| list.read().unwrap().ty == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-woog_option"}}}
    /// Navigate to [`WoogOption`] across R2(1-M)
    pub fn r2_woog_option<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<WoogOption>>> {
        store
            .iter_woog_option()
            .filter(|woog_option| woog_option.read().unwrap().ty == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-reference"}}}
    /// Navigate to [`Reference`] across R35(1-M)
    pub fn r35_reference<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Reference>>> {
        store
            .iter_reference()
            .filter(|reference| reference.read().unwrap().ty == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_Mc-to-span"}}}
    /// Navigate to [`Span`] across R62(1-Mc)
    pub fn r62_span<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Span>>> {
        store
            .iter_span()
            .filter_map(|span| {
                if span.read().unwrap().ty == Some(self.id()) {
                    Some(span)
                } else {
                    None
                }
            })
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-x_value"}}}
    /// Navigate to [`XValue`] across R24(1-M)
    pub fn r24_x_value<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XValue>>> {
        store
            .iter_x_value()
            .filter(|x_value| x_value.read().unwrap().ty == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
