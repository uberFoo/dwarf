// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"task-struct-definition-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"task-use-statements"}}}
use serde::{Deserialize, Serialize};
use uuid::{uuid, Uuid};
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"task-const-documentation"}}}
/// A type to contain a task.
///
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"task-const-definition"}}}
pub const TASK: Uuid = uuid!["e6491493-a305-56aa-9d82-985393df6874"];

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Task;

impl Task {
    pub fn new() -> Self {
        Self {}
    }

    pub fn id(&self) -> Uuid {
        TASK
    }
}

impl Default for Task {
    fn default() -> Self {
        Self::new()
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
