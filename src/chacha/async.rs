#![cfg(feature = "async")]
//! An executor with task priorities.

use std::collections::VecDeque;
use std::future::Future;
use std::thread;

use async_executor::{Executor, Task};
use futures_lite::{future, prelude::*};

use crate::{chacha::error::ChaChaError, RefType, Value};

/// An executor with task priorities.
///
/// Tasks with lower priorities only get polled when there are no tasks with higher priorities.
#[derive(Debug)]
pub struct ChaChaExecutor<'a> {
    ex: Executor<'a>,
    running: bool,
    queue: VecDeque<async_task::Task<Result<RefType<Value>, ChaChaError>>>,
}

impl<'a> ChaChaExecutor<'a> {
    /// Creates a new executor.
    pub const fn new() -> ChaChaExecutor<'a> {
        ChaChaExecutor {
            ex: Executor::new(),
            running: true,
            queue: VecDeque::new(),
        }
    }
    /// Spawns a task with the given priority.
    pub fn spawn(
        &mut self,
        future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    ) {
        let task = self.ex.spawn(future);
        log::debug!(target: "async", "spawn: {:?}\n{:?}", self.ex, task);
        self.queue.push_back(task);
    }

    pub fn run(&mut self) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "run: {:?}", self.queue);
        future::block_on(self.ex.run(self.queue.pop_front().unwrap()))
    }
}
