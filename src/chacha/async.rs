#![cfg(feature = "async")]
//! An executor with task priorities.

use std::collections::VecDeque;
use std::future::Future;
use std::sync::Arc;

use async_executor::{Executor, Task};
use crossbeam::channel::{Receiver, Sender};
use futures_lite::{future, prelude::*};

use crate::{chacha::error::ChaChaError, new_ref, NewRef, RefType, Value};

/// An executor with task priorities.
///
/// Tasks with lower priorities only get polled when there are no tasks with higher priorities.
#[derive(Clone, Debug)]
pub struct ChaChaExecutor<'a> {
    ex: Arc<Executor<'a>>,
    send: Sender<RefType<Value>>,
    recv: Receiver<RefType<Value>>,
}

impl<'a> ChaChaExecutor<'a> {
    /// Creates a new executor.
    pub fn new() -> ChaChaExecutor<'a> {
        let (send, recv) = crossbeam::channel::unbounded();
        ChaChaExecutor {
            ex: Arc::new(Executor::new()),
            send,
            recv,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ex.is_empty()
    }

    pub fn park_value(&self, value: RefType<Value>) {
        self.send.send(value).unwrap();
    }

    pub fn spawn(
        &self,
        future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    ) -> Task<Result<RefType<Value>, ChaChaError>> {
        let task = self.ex.spawn(future);
        log::debug!(target: "async", "spawn executor: {:?}\nspawn task: {:?}", self.ex, task);
        task
    }

    pub fn block_on(
        &self,
        task: Task<Result<RefType<Value>, ChaChaError>>,
    ) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "block_on: {:?}", self.ex);
        future::block_on(async { task.await })
        // let result = future::block_on(async { self.ex.run(async { task.await }).await });
        // result
    }

    pub fn tick(&self) {
        log::debug!(target: "async", "tick: {:?}", self.ex);
        future::block_on(async { self.ex.tick().await });
    }

    pub fn try_tick(&self) -> bool {
        log::debug!(target: "async", "tick: {:?}", self.ex);
        self.ex.try_tick()
    }
}
