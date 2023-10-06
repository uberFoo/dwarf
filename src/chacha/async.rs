#![cfg(feature = "async")]
//! An executor with task priorities.

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
    queue: Vec<async_task::Task<Result<RefType<Value>, ChaChaError>>>,
}

impl<'a> ChaChaExecutor<'a> {
    /// Creates a new executor.
    pub const fn new() -> ChaChaExecutor<'a> {
        ChaChaExecutor {
            ex: Executor::new(),
            running: true,
            queue: Vec::new(),
        }
    }
    /// Spawns a task with the given priority.
    pub fn spawn(
        &mut self,
        future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    ) {
        // ) -> () {
        // self.ex.spawn(future).detach();
        let foo = self.ex.spawn(future);
        log::debug!(target: "async", "mother {:?}\n{:?}", self.ex, foo);
        self.queue.push(foo);
    }

    pub fn block_on(&mut self) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "block_on: {:?}", self.queue);
        future::block_on(self.ex.run(self.queue.pop().unwrap()))
    }

    pub async fn halt(&mut self) {
        log::debug!(target: "async", "halt: {}", self.running);
        self.running = false;
        log::debug!(target: "async", "halt: {}", self.running);
    }

    /// Runs the executor forever.
    pub async fn run(&self) {
        while self.running {
            log::debug!(target: "async", "running fucktard: {}", self.running);
            // for i in 0..200 {
            log::debug!(target: "async", "fucker0 {:?}", self.ex);
            let t0 = self.ex.tick();

            log::debug!(target: "async", "fucker1 {:?}", self.ex);
            // println!("tick {i}");
            t0.await;
            // future::block_on(t0);
            log::debug!(target: "async", "fucker2 {:?}", self.ex);
            // }

            // Yield every now and then.
            // future::yield_now().await;
        }
    }
}

// fn main() {
//     static EX: ChaChaExecutor<'_> = ChaChaExecutor::new();

//     // Spawn a thread running the executor forever.
//     thread::spawn(|| future::block_on(EX.run()));

//     let mut tasks = Vec::new();

//     for _ in 0..200 {
//         // Spawn a task with this priority.
//         tasks.push(EX.spawn(async move {
//             println!("a");
//             future::yield_now().await;
//             println!("b");
//         }));
//     }

//     for task in tasks {
//         future::block_on(task);
//     }
// }
