// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"value_type-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-use-statements"}}}
use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::char::CHAR;
use crate::lu_dog::types::empty::EMPTY;
use crate::lu_dog::types::enumeration::Enumeration;
use crate::lu_dog::types::field::Field;
use crate::lu_dog::types::function::Function;
use crate::lu_dog::types::generic::Generic;
use crate::lu_dog::types::import::Import;
use crate::lu_dog::types::lambda::Lambda;
use crate::lu_dog::types::lambda_parameter::LambdaParameter;
use crate::lu_dog::types::list::List;
use crate::lu_dog::types::parameter::Parameter;
use crate::lu_dog::types::range::RANGE;
use crate::lu_dog::types::span::Span;
use crate::lu_dog::types::task::TASK;
use crate::lu_dog::types::tuple_field::TupleField;
use crate::lu_dog::types::type_cast::TypeCast;
use crate::lu_dog::types::unknown::UNKNOWN;
use crate::lu_dog::types::woog_struct::WoogStruct;
use crate::lu_dog::types::x_future::XFuture;
use crate::lu_dog::types::x_plugin::XPlugin;
use crate::lu_dog::types::x_value::XValue;
use crate::lu_dog::types::z_object_store::ZObjectStore;
use dwarf::sarzak::types::ty::Ty;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
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
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum ValueType {
    Char(Uuid),
    Empty(Uuid),
    Enumeration(Uuid),
    Function(Uuid),
    XFuture(Uuid),
    Generic(Uuid),
    Import(Uuid),
    Lambda(Uuid),
    List(Uuid),
    ZObjectStore(Uuid),
    XPlugin(Uuid),
    Range(Uuid),
    WoogStruct(Uuid),
    Task(Uuid),
    Ty(Uuid),
    Unknown(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-implementation"}}}
impl ValueType {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-new-impl"}}}
    /// Create a new instance of ValueType::Char
    pub fn new_char(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&CHAR).unwrap()
    }

    /// Create a new instance of ValueType::Empty
    pub fn new_empty(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&EMPTY).unwrap()
    }

    /// Create a new instance of ValueType::Enumeration
    pub fn new_enumeration(
        enumeration: &Arc<RwLock<Enumeration>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = enumeration.read().unwrap().id;
        if let Some(enumeration) = store.exhume_value_type(&id) {
            enumeration
        } else {
            let new = Arc::new(RwLock::new(Self::Enumeration(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::Function
    pub fn new_function(
        function: &Arc<RwLock<Function>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = function.read().unwrap().id;
        if let Some(function) = store.exhume_value_type(&id) {
            function
        } else {
            let new = Arc::new(RwLock::new(Self::Function(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::XFuture
    pub fn new_x_future(
        x_future: &Arc<RwLock<XFuture>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = x_future.read().unwrap().id;
        if let Some(x_future) = store.exhume_value_type(&id) {
            x_future
        } else {
            let new = Arc::new(RwLock::new(Self::XFuture(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::Generic
    pub fn new_generic(
        generic: &Arc<RwLock<Generic>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = generic.read().unwrap().id;
        if let Some(generic) = store.exhume_value_type(&id) {
            generic
        } else {
            let new = Arc::new(RwLock::new(Self::Generic(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::Import
    pub fn new_import(import: &Arc<RwLock<Import>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = import.read().unwrap().id;
        if let Some(import) = store.exhume_value_type(&id) {
            import
        } else {
            let new = Arc::new(RwLock::new(Self::Import(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::Lambda
    pub fn new_lambda(lambda: &Arc<RwLock<Lambda>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = lambda.read().unwrap().id;
        if let Some(lambda) = store.exhume_value_type(&id) {
            lambda
        } else {
            let new = Arc::new(RwLock::new(Self::Lambda(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::List
    pub fn new_list(list: &Arc<RwLock<List>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = list.read().unwrap().id;
        if let Some(list) = store.exhume_value_type(&id) {
            list
        } else {
            let new = Arc::new(RwLock::new(Self::List(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::ZObjectStore
    pub fn new_z_object_store(
        z_object_store: &Arc<RwLock<ZObjectStore>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = z_object_store.read().unwrap().id;
        if let Some(z_object_store) = store.exhume_value_type(&id) {
            z_object_store
        } else {
            let new = Arc::new(RwLock::new(Self::ZObjectStore(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::XPlugin
    pub fn new_x_plugin(
        x_plugin: &Arc<RwLock<XPlugin>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = x_plugin.read().unwrap().id;
        if let Some(x_plugin) = store.exhume_value_type(&id) {
            x_plugin
        } else {
            let new = Arc::new(RwLock::new(Self::XPlugin(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::Range
    pub fn new_range(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&RANGE).unwrap()
    }

    /// Create a new instance of ValueType::WoogStruct
    pub fn new_woog_struct(
        woog_struct: &Arc<RwLock<WoogStruct>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = woog_struct.read().unwrap().id;
        if let Some(woog_struct) = store.exhume_value_type(&id) {
            woog_struct
        } else {
            let new = Arc::new(RwLock::new(Self::WoogStruct(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::Task
    pub fn new_task(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&TASK).unwrap()
    }

    /// Create a new instance of ValueType::Ty
    pub fn new_ty(ty: &Arc<RwLock<Ty>>, store: &mut LuDogStore) -> Arc<RwLock<Self>> {
        let id = ty.read().unwrap().id();
        if let Some(ty) = store.exhume_value_type(&id) {
            ty
        } else {
            let new = Arc::new(RwLock::new(Self::Ty(id)));
            store.inter_value_type(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of ValueType::Unknown
    pub fn new_unknown(store: &LuDogStore) -> Arc<RwLock<Self>> {
        // This is already in the store.
        store.exhume_value_type(&UNKNOWN).unwrap()
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Char(id) => *id,
            Self::Empty(id) => *id,
            Self::Enumeration(id) => *id,
            Self::Function(id) => *id,
            Self::XFuture(id) => *id,
            Self::Generic(id) => *id,
            Self::Import(id) => *id,
            Self::Lambda(id) => *id,
            Self::List(id) => *id,
            Self::ZObjectStore(id) => *id,
            Self::XPlugin(id) => *id,
            Self::Range(id) => *id,
            Self::WoogStruct(id) => *id,
            Self::Task(id) => *id,
            Self::Ty(id) => *id,
            Self::Unknown(id) => *id,
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
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-x_future"}}}
    /// Navigate to [`XFuture`] across R2(1-M)
    pub fn r2_x_future<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XFuture>>> {
        store
            .iter_x_future()
            .filter(|x_future| x_future.read().unwrap().x_value == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_Mc-to-generic"}}}
    /// Navigate to [`Generic`] across R99(1-Mc)
    pub fn r99_generic<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Generic>>> {
        store
            .iter_generic()
            .filter(|generic| generic.read().unwrap().ty == Some(self.id()))
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-lambda"}}}
    /// Navigate to [`Lambda`] across R74(1-M)
    pub fn r74_lambda<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Lambda>>> {
        store
            .iter_lambda()
            .filter(|lambda| lambda.read().unwrap().return_type == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_Mc-to-lambda_parameter"}}}
    /// Navigate to [`LambdaParameter`] across R77(1-Mc)
    pub fn r77_lambda_parameter<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<LambdaParameter>>> {
        store
            .iter_lambda_parameter()
            .filter(|lambda_parameter| lambda_parameter.read().unwrap().ty == Some(self.id()))
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
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-parameter"}}}
    /// Navigate to [`Parameter`] across R79(1-M)
    pub fn r79_parameter<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Parameter>>> {
        store
            .iter_parameter()
            .filter(|parameter| parameter.read().unwrap().ty == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_Mc-to-span"}}}
    /// Navigate to [`Span`] across R62(1-Mc)
    pub fn r62_span<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Span>>> {
        store
            .iter_span()
            .filter(|span| span.read().unwrap().ty == Some(self.id()))
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-tuple_field"}}}
    /// Navigate to [`TupleField`] across R86(1-M)
    pub fn r86_tuple_field<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<TupleField>>> {
        store
            .iter_tuple_field()
            .filter(|tuple_field| tuple_field.read().unwrap().ty == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"value_type-struct-impl-nav-backward-1_M-to-type_cast"}}}
    /// Navigate to [`TypeCast`] across R69(1-M)
    pub fn r69_type_cast<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<TypeCast>>> {
        store
            .iter_type_cast()
            .filter(|type_cast| type_cast.read().unwrap().ty == self.id())
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
