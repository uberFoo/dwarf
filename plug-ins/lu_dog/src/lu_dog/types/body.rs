// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"body-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::block::Block;
use crate::lu_dog::types::external_implementation::ExternalImplementation;
use crate::lu_dog::types::function::Function;
use crate::lu_dog::types::lambda::Lambda;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-hybrid-documentation"}}}
/// The function body. Generally contains statements, but may point to some other implementation
/// .
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-hybrid-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Body {
    pub subtype: BodyEnum,
    pub a_sink: bool,
    pub id: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-hybrid-enum-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum BodyEnum {
    Block(Uuid),
    ExternalImplementation(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-implementation"}}}
impl Body {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-struct-impl-new_block"}}}
    /// Inter a new Body in the store, and return it's `id`.
    pub fn new_block(
        a_sink: bool,
        subtype: &Arc<RwLock<Block>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Body>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Body {
            a_sink: a_sink,
            subtype: BodyEnum::Block(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_body(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-struct-impl-new_external_implementation"}}}
    /// Inter a new Body in the store, and return it's `id`.
    pub fn new_external_implementation(
        a_sink: bool,
        subtype: &Arc<RwLock<ExternalImplementation>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Body>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Body {
            a_sink: a_sink,
            subtype: BodyEnum::ExternalImplementation(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_body(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-struct-impl-nav-backward-cond-to-function"}}}
    /// Navigate to [`Function`] across R19(1-1c)
    pub fn r19c_function<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Function>>> {
        let function = store
            .iter_function()
            .find(|function| function.read().unwrap().body == self.id);
        match function {
            Some(ref function) => vec![function.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"body-struct-impl-nav-backward-one-bi-cond-to-lambda"}}}
    /// Navigate to [`Lambda`] across R73(1c-1c)
    pub fn r73c_lambda<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Lambda>>> {
        let lambda = store
            .iter_lambda()
            .find(|lambda| lambda.read().unwrap().body == Some(self.id));
        match lambda {
            Some(ref lambda) => vec![lambda.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
