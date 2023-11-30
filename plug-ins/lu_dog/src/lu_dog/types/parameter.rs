// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"parameter-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::function::Function;
use crate::lu_dog::types::value_type::ValueType;
use crate::lu_dog::types::variable::Variable;
use crate::lu_dog::types::variable::VariableEnum;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-documentation"}}}
/// A Parameter to a Function
///
/// From inside the function it's a parameter, from outside it's an argument. No idea why I
///  wrote that — just looking for content...  I mean, what else do you say about a parameter
/// ?
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Parameter {
    pub id: Uuid,
    pub position: i64,
    /// R13: [`Parameter`] 'is available to a' [`Function`]
    pub function: Uuid,
    /// R14: [`Parameter`] 'follows' [`Parameter`]
    pub next: Option<Uuid>,
    /// R79: [`Parameter`] 'requires a' [`ValueType`]
    pub ty: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-implementation"}}}
impl Parameter {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-impl-new"}}}
    /// Inter a new 'Parameter' in the store, and return it's `id`.
    pub fn new(
        position: i64,
        function: &Arc<RwLock<Function>>,
        next: Option<&Arc<RwLock<Parameter>>>,
        ty: &Arc<RwLock<ValueType>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Parameter>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Parameter {
            id,
            position,
            function: function.read().unwrap().id,
            next: next.map(|parameter| parameter.read().unwrap().id),
            ty: ty.read().unwrap().id(),
        }));
        store.inter_parameter(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-impl-nav-forward-to-function"}}}
    /// Navigate to [`Function`] across R13(1-*)
    pub fn r13_function<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Function>>> {
        vec![store.exhume_function(&self.function).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-impl-nav-forward-cond-to-next"}}}
    /// Navigate to [`Parameter`] across R14(1-*c)
    pub fn r14_parameter<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Parameter>>> {
        match self.next {
            Some(ref next) => vec![store.exhume_parameter(&next).unwrap()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-impl-nav-forward-to-ty"}}}
    /// Navigate to [`ValueType`] across R79(1-*)
    pub fn r79_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.ty).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-impl-nav-backward-one-bi-cond-to-function"}}}
    /// Navigate to [`Function`] across R82(1c-1c)
    pub fn r82c_function<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Function>>> {
        let function = store
            .iter_function()
            .find(|function| function.read().unwrap().first_param == Some(self.id));
        match function {
            Some(ref function) => vec![function.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-struct-impl-nav-backward-one-bi-cond-to-parameter"}}}
    /// Navigate to [`Parameter`] across R14(1c-1c)
    pub fn r14c_parameter<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Parameter>>> {
        let parameter = store
            .iter_parameter()
            .find(|parameter| parameter.read().unwrap().next == Some(self.id));
        match parameter {
            Some(ref parameter) => vec![parameter.clone()],
            None => Vec::new(),
        }
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"parameter-impl-nav-subtype-to-supertype-variable"}}}
    // Navigate to [`Variable`] across R12(isa)
    pub fn r12_variable<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Variable>>> {
        vec![store
            .iter_variable()
            .find(|variable| {
                if let VariableEnum::Parameter(id) = variable.read().unwrap().subtype {
                    id == self.id
                } else {
                    false
                }
            })
            .unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
