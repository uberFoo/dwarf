// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"data_structure-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"data_structure-use-statements"}}}
use crate::lu_dog::store::ObjectStore as LuDogStore;
use crate::lu_dog::types::enumeration::Enumeration;
use crate::lu_dog::types::struct_expression::StructExpression;
use crate::lu_dog::types::woog_struct::WoogStruct;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"data_structure-enum-definition"}}}
#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum DataStructure {
    Enumeration(Uuid),
    WoogStruct(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"data_structure-implementation"}}}
impl DataStructure {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"data_structure-new-impl"}}}
    /// Create a new instance of DataStructure::Enumeration
    pub fn new_enumeration(
        enumeration: &Arc<RwLock<Enumeration>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = enumeration.read().unwrap().id;
        if let Some(enumeration) = store.exhume_data_structure(&id) {
            enumeration
        } else {
            let new = Arc::new(RwLock::new(Self::Enumeration(id)));
            store.inter_data_structure(new.clone());
            new
        }
    } // wtf?

    /// Create a new instance of DataStructure::WoogStruct
    pub fn new_woog_struct(
        woog_struct: &Arc<RwLock<WoogStruct>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Self>> {
        let id = woog_struct.read().unwrap().id;
        if let Some(woog_struct) = store.exhume_data_structure(&id) {
            woog_struct
        } else {
            let new = Arc::new(RwLock::new(Self::WoogStruct(id)));
            store.inter_data_structure(new.clone());
            new
        }
    } // wtf?

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"data_structure-get-id-impl"}}}
    pub fn id(&self) -> Uuid {
        match self {
            Self::Enumeration(id) => *id,
            Self::WoogStruct(id) => *id,
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"data_structure-struct-impl-nav-backward-1_M-to-struct_expression"}}}
    /// Navigate to [`StructExpression`] across R39(1-M)
    pub fn r39_struct_expression<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<StructExpression>>> {
        store
            .iter_struct_expression()
            .filter(|struct_expression| struct_expression.read().unwrap().data == self.id())
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
