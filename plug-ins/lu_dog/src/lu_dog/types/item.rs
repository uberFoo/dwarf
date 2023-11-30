// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"item-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::dwarf_source_file::DwarfSourceFile;
use crate::lu_dog::types::enumeration::Enumeration;
use crate::lu_dog::types::function::Function;
use crate::lu_dog::types::implementation_block::ImplementationBlock;
use crate::lu_dog::types::import::Import;
use crate::lu_dog::types::woog_struct::WoogStruct;
use crate::lu_dog::types::x_macro::XMacro;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-hybrid-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Item {
    pub subtype: ItemEnum,
    pub id: Uuid,
    /// R25: [`Item`] '' [`DwarfSourceFile`]
    pub source: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-hybrid-enum-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum ItemEnum {
    Enumeration(Uuid),
    Function(Uuid),
    ImplementationBlock(Uuid),
    Import(Uuid),
    XMacro(Uuid),
    WoogStruct(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-implementation"}}}
impl Item {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-struct-impl-new_enumeration"}}}
    /// Inter a new Item in the store, and return it's `id`.
    pub fn new_enumeration(
        source: &Arc<RwLock<DwarfSourceFile>>,
        subtype: &Arc<RwLock<Enumeration>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Item>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Item {
            source: source.read().unwrap().id,
            subtype: ItemEnum::Enumeration(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_item(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-struct-impl-new_function"}}}
    /// Inter a new Item in the store, and return it's `id`.
    pub fn new_function(
        source: &Arc<RwLock<DwarfSourceFile>>,
        subtype: &Arc<RwLock<Function>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Item>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Item {
            source: source.read().unwrap().id,
            subtype: ItemEnum::Function(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_item(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-struct-impl-new_implementation_block"}}}
    /// Inter a new Item in the store, and return it's `id`.
    pub fn new_implementation_block(
        source: &Arc<RwLock<DwarfSourceFile>>,
        subtype: &Arc<RwLock<ImplementationBlock>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Item>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Item {
            source: source.read().unwrap().id,
            subtype: ItemEnum::ImplementationBlock(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_item(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-struct-impl-new_import"}}}
    /// Inter a new Item in the store, and return it's `id`.
    pub fn new_import(
        source: &Arc<RwLock<DwarfSourceFile>>,
        subtype: &Arc<RwLock<Import>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Item>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Item {
            source: source.read().unwrap().id,
            subtype: ItemEnum::Import(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_item(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-struct-impl-new_x_macro"}}}
    /// Inter a new Item in the store, and return it's `id`.
    pub fn new_x_macro(
        source: &Arc<RwLock<DwarfSourceFile>>,
        subtype: &Arc<RwLock<XMacro>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Item>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Item {
            source: source.read().unwrap().id,
            subtype: ItemEnum::XMacro(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_item(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-struct-impl-new_woog_struct"}}}
    /// Inter a new Item in the store, and return it's `id`.
    pub fn new_woog_struct(
        source: &Arc<RwLock<DwarfSourceFile>>,
        subtype: &Arc<RwLock<WoogStruct>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Item>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Item {
            source: source.read().unwrap().id,
            subtype: ItemEnum::WoogStruct(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_item(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"item-struct-impl-nav-forward-to-source"}}}
    /// Navigate to [`DwarfSourceFile`] across R25(1-*)
    pub fn r25_dwarf_source_file<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<DwarfSourceFile>>> {
        vec![store.exhume_dwarf_source_file(&self.source).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
