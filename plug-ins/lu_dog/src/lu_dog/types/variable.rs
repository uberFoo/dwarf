// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"variable-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::lambda_parameter::LambdaParameter;
use crate::lu_dog::types::local_variable::LocalVariable;
use crate::lu_dog::types::parameter::Parameter;
use crate::lu_dog::types::x_value::XValue;
use crate::lu_dog::types::x_value::XValueEnum;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-hybrid-documentation"}}}
/// A Variable
///
/// A variable in a function. It may be either a local variable or a parameter.
///
/// A variable has a name, and and indirectly, via [`Value`], a type.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-hybrid-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Variable {
    pub subtype: VariableEnum,
    pub id: Uuid,
    pub name: String,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-hybrid-enum-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum VariableEnum {
    LambdaParameter(Uuid),
    LocalVariable(Uuid),
    Parameter(Uuid),
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-implementation"}}}
impl Variable {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-struct-impl-new_lambda_parameter"}}}
    /// Inter a new Variable in the store, and return it's `id`.
    pub fn new_lambda_parameter(
        name: String,
        subtype: &Arc<RwLock<LambdaParameter>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Variable>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Variable {
            name: name,
            subtype: VariableEnum::LambdaParameter(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_variable(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-struct-impl-new_local_variable"}}}
    /// Inter a new Variable in the store, and return it's `id`.
    pub fn new_local_variable(
        name: String,
        subtype: &Arc<RwLock<LocalVariable>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Variable>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Variable {
            name: name,
            subtype: VariableEnum::LocalVariable(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_variable(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-struct-impl-new_parameter"}}}
    /// Inter a new Variable in the store, and return it's `id`.
    pub fn new_parameter(
        name: String,
        subtype: &Arc<RwLock<Parameter>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Variable>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Variable {
            name: name,
            subtype: VariableEnum::Parameter(subtype.read().unwrap().id), // b
            id,
        }));
        store.inter_variable(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"variable-impl-nav-subtype-to-supertype-x_value"}}}
    // Navigate to [`XValue`] across R11(isa)
    pub fn r11_x_value<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XValue>>> {
        vec![store
            .iter_x_value()
            .find(|x_value| {
                if let XValueEnum::Variable(id) = x_value.read().unwrap().subtype {
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
