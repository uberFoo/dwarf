// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"pattern-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-use-statements"}}}
use std::sync::Arc;
use std::sync::RwLock;
use uuid::Uuid;

use crate::lu_dog::types::expression::Expression;
use crate::lu_dog::types::x_match::XMatch;
use serde::{Deserialize, Serialize};

use crate::lu_dog::store::ObjectStore as LuDogStore;
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-struct-documentation"}}}
/// The pattern is a specification for extracting data from a type. It’s sort of a reverse
///  impression of what you are looking for. If the shape of the impression matches the scrutinee
/// , then they “fit” and the pattern’s lvalues will be populated with data from the scrutinee
/// .
///
/// There are a bunch of diffirent kinds of patterns. Literal, ident, struct, tuple, etc. Modeling
///  this will take a lot of room and time.
///
/// Doing this I’m going to cheat a bit and store the code that does matching as a string
///  on this object during compilation. During runtime the string will be evaluated (either as
///  dwrf, or perhasps using a small VM. Or maybe use the built-in VM. It should be able to handle
///  all that we need. This way, I don’t have to model all the bits because they are encoded
///  in the code attribute.
///
/// So I guess that means I’ll be writing assembly code...
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-struct-definition"}}}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Pattern {
    pub id: Uuid,
    /// R92: [`Pattern`] 'executes' [`Expression`]
    pub expression: Uuid,
    /// R87: [`Expression`] '🚧 Comments are out of order — see sarzak#14.' [`Expression`]
    pub match_expr: Uuid,
    /// R87: [`XMatch`] '🚧 Comments are out of order — see sarzak#14.' [`XMatch`]
    pub x_match: Uuid,
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-implementation"}}}
impl Pattern {
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-struct-impl-new"}}}
    /// Inter a new 'Pattern' in the store, and return it's `id`.
    pub fn new(
        expression: &Arc<RwLock<Expression>>,
        match_expr: &Arc<RwLock<Expression>>,
        x_match: &Arc<RwLock<XMatch>>,
        store: &mut LuDogStore,
    ) -> Arc<RwLock<Pattern>> {
        let id = Uuid::new_v4();
        let new = Arc::new(RwLock::new(Pattern {
            id,
            expression: expression.read().unwrap().id(),
            match_expr: match_expr.read().unwrap().id(),
            x_match: x_match.read().unwrap().id,
        }));
        store.inter_pattern(new.clone());
        new
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-struct-impl-nav-forward-to-expression"}}}
    /// Navigate to [`Expression`] across R92(1-*)
    pub fn r92_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.expression).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-struct-impl-nav-forward-assoc-to-match_expr"}}}
    /// Navigate to [`Expression`] across R87(1-*)
    pub fn r87_expression<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<Expression>>> {
        vec![store.exhume_expression(&self.match_expr).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"pattern-struct-impl-nav-forward-assoc-to-x_match"}}}
    /// Navigate to [`XMatch`] across R87(1-*)
    pub fn r87_x_match<'a>(&'a self, store: &'a LuDogStore) -> Vec<Arc<RwLock<XMatch>>> {
        vec![store.exhume_x_match(&self.x_match).unwrap()]
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
