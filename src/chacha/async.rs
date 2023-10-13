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
    ex: Vec<Arc<Executor<'a>>>,
    send: Sender<RefType<Value>>,
    _recv: Receiver<RefType<Value>>,
}

impl<'a> ChaChaExecutor<'a> {
    /// Creates a new executor.
    pub fn new() -> ChaChaExecutor<'a> {
        let (send, _recv) = crossbeam::channel::unbounded();
        ChaChaExecutor {
            ex: vec![Arc::new(Executor::new())],
            send,
            _recv,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ex.last().unwrap().is_empty()
    }

    pub fn park_value(&self, value: RefType<Value>) {
        self.send.send(value).unwrap();
    }

    pub fn spawn(
        &mut self,
        future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    ) -> Task<Result<RefType<Value>, ChaChaError>> {
        let task = self.ex.last().unwrap().spawn(future);
        log::debug!(target: "async", "spawn executor: {:?}\n\tspawn task: {:?}", self.ex, task);
        task
    }

    pub async fn run(&self) {
        log::debug!(target: "async", "run: {:?}", self.ex);
        while !self.ex.last().unwrap().is_empty() {
            // for _ in 0..10 {
            self.ex.last().unwrap().try_tick();
            // }

            // dbg!("yield");
            future::yield_now().await;
        }
    }

    pub fn block_on(
        &self,
        task: Task<Result<RefType<Value>, ChaChaError>>,
    ) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "block_on: {:?}", self.ex);
        future::block_on(async { task.await })
    }

    pub async fn resolve_task(
        &self,
        task: Task<Result<RefType<Value>, ChaChaError>>,
    ) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "resolve_task: {:?}", self.ex);
        self.ex.last().unwrap().run(task).await
    }

    pub fn tick(&self) {
        log::debug!(target: "async", "tick: {:?}", self.ex);
        future::block_on(async { self.ex.last().unwrap().tick().await });
    }

    pub fn try_tick(&self) -> bool {
        log::debug!(target: "async", "tick: {:?}", self.ex);
        self.ex.last().unwrap().try_tick()
    }
}
