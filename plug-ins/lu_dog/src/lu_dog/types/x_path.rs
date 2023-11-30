// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"x_path-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use crate::lu_dog::types::path_element::PathElement;
use crate::lu_dog::types::struct_expression::StructExpression;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-struct-documentation"}}}
/// This is a path to a local variable, or an item. It is made up of scopes, separated by `
/// ::`.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct XPath {
    pub id: Uuid,
    pub unique: Uuid,
    /// R97: [`XPath`] 'first element' [`PathElement`]
    pub first: Option<Uuid>,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-implementation"}}}
impl XPath {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-struct-impl-new"}}}
    /// Inter a new 'Path' in the store, and return it's `id`.
    pub fn new(
        unique: Uuid,
        first: Option<&Arc<RwLock<PathElement>>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<XPath>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(XPath {
            id,
            unique,
            first: first.map(|path_element| path_element.read().unwrap().id),
        }));
        store.inter_x_path(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-struct-impl-nav-forward-cond-to-first"}}}
    /// Navigate to [`PathElement`] across R97(1-*c)
    pub fn r97_path_element<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<PathElement>>> {
        match self.first {
            Some(ref first) => vec![store.exhume_path_element(&first).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-struct-impl-nav-backward-1_M-to-path_element"}}}
    /// Navigate to [`PathElement`] across R90(1-M)
    pub fn r90_path_element<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<PathElement>>> {
        store
            .iter_path_element()
            .filter(|path_element| path_element.read().unwrap().x_path == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-struct-impl-nav-backward-1_M-to-struct_expression"}}}
    /// Navigate to [`StructExpression`] across R96(1-M)
    pub fn r96_struct_expression<'a>(
        &'a self,
        store: &'a LuDogStore,
    ) -> Vec<Arc<RwLock<StructExpression>>> {
        store
            .iter_struct_expression()
            .filter(|struct_expression| struct_expression.read().unwrap().x_path == self.id)
            .collect()
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"x_path-impl-nav-subtype-to-supertype-expression"}}}
    // Navigate to [`Expression`] across R15(isa)
    pub fn r15_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
