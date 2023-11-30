// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"tuple_field-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"tuple_field-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::enum_field::EnumField;
use crate::lu_dog::types::enum_field::EnumFieldEnum;
use crate::lu_dog::types::value_type::ValueType;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"tuple_field-struct-documentation"}}}
/// A field that is a tuple.
///
/// Currently in this implementation we are cheating, as we don’t yet actually have tuples
/// . So this is limited to a single item.
///
/// Note the `hack` attribute. What’s happening is that during generic substitution?, expansion
/// ?, whatever. During that we are cloning the enum, and it’s fields. This is to create a
///  new type. When we do this we don’t want the store optimizing away a duplicate Tuple Field
/// .
///
/// I deb thee hack because I think the right thing to do is something else, I’m just not
///  sure what it is yet.
///
/// I renamed it to `xyzzy`, because I think `hack` does magic in the compiler.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"tuple_field-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct TupleField {
    pub id: Uuid,
    pub xyzzy: Uuid,
    /// R86: [`TupleField`] 'must have a type' [`ValueType`]
    pub ty: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"tuple_field-implementation"}}}
impl TupleField {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"tuple_field-struct-impl-new"}}}
    /// Inter a new 'Tuple Field' in the store, and return it's `id`.
    pub fn new(
        xyzzy: Uuid,
        ty: &Arc<RwLock<ValueType>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<TupleField>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(TupleField {
            id,
            xyzzy,
            ty: ty.read().unwrap().id(),
        }));
        store.inter_tuple_field(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"tuple_field-struct-impl-nav-forward-to-ty"}}}
    /// Navigate to [`ValueType`] across R86(1-*)
    pub fn r86_value_type<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<ValueType>>> {
        vec![store.exhume_value_type(&self.ty).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"tuple_field-impl-nav-subtype-to-supertype-enum_field"}}}
    // Navigate to [`EnumField`] across R85(isa)
    pub fn r85_enum_field<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<EnumField>>> {
        vec![store
            .iter_enum_field()
            .find(|enum_field| {
                if let EnumFieldEnum::TupleField(id) = enum_field.read().unwrap().subtype {
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
