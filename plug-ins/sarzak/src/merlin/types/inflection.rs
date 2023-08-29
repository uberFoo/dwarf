// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"inflection-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"inflection-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"inflection-const-documentation"}}}
/// Inflection Point
///
/// This is a point on the line that is used to split it into two segments. It’s called inflection
///  because it’s at this point that the segments may point in different directions.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"inflection-const-definition"}}}
pub const INFLECTION: Uuid = uuid!["15e9aa5c-b0ae-5ca9-8402-d2197984ef98"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Inflection;

impl Inflection {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        INFLECTION
    }
}

impl Default for Inflection {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
