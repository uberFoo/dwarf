// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"external-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"external-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::sarzak::types::ty::Ty;
use serde::{Deserialize, Serialize};

use crate::sarzak::store::ObjectStore as SarzakStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"external-struct-documentation"}}}
/// External Type
///
/// This may literally be anything. It's used during code generation to generate variables names
///  and type names for things that are outside of a modeled domain. For example, a timer would
///  be an external type. The specifics of how it is used is up to the model compiler.
///
/// In grace, the `name` attribute is used during code generation to create variable names by
///  converting it to `snake_case`. When used as a type, it is converted to `UpperCamelCase`
/// .
///
/// We use `path` as the path is a `use` statement.
///
/// I'm updating this while trying to use it, so this description is going to be rather incoherent
///  until things settle down.
///
/// The way I'm using this, and hopefully the way that will always accommodate, is as a singleton
///  within a particular function scope. Maybe it's a system-wide singleton? I dunno. But it's
///  a singleton.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"external-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct External {
    pub ctor: String,
    pub id: Uuid,
    pub name: String,
    pub x_path: String,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"external-implementation"}}}
impl External {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"external-struct-impl-new"}}}
    /// Inter a new 'External' in the store, and return it's `id`.
    pub fn new(
        ctor: String,
        name: String,
        x_path: String,
        store: &mut SarzakStore,
    ) -> Arc<RwLock<External>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(External {
            ctor,
            id,
            name,
            x_path,
        }));
        store.inter_external(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"external-impl-nav-subtype-to-supertype-ty"}}}
    // Navigate to [`Ty`] across R3(isa)
    pub fn r3_ty<'a>(&'a self, store: &'a SarzakStore) -> Vec<Arc<RwLock<Ty>>> {
        vec![store.exhume_ty(&self.id).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
