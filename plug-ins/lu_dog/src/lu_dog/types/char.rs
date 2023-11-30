// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"char-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"char-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"char-const-documentation"}}}
/// A char
///
/// I char is a single printable UNICODE character. It may contain multiple bytes.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"char-const-definition"}}}
pub const CHAR: Uuid = uuid!["9fe96a25-33bd-55cf-b227-fea54afbcc70"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Char;

impl Char {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        CHAR
    }
}

impl Default for Char {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
