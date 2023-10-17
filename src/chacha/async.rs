#![cfg(feature = "async")]
//! An executor with task priorities.

use std::future::Future;
use std::sync::Arc;
use std::thread;
use std::{collections::VecDeque, marker::PhantomData};

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
    _recv: Receiver<RefType<Value>>,
}

impl<'a> ChaChaExecutor<'a> {
    /// Creates a new executor.
    pub fn new() -> ChaChaExecutor<'a> {
        let (send, _recv) = crossbeam::channel::unbounded();
        ChaChaExecutor {
            ex: Arc::new(Executor::new()),
            send,
            _recv,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ex.is_empty()
    }

    pub fn park_value(&self, value: RefType<Value>) {
        self.send.send(value).unwrap();
    }

    pub fn spawn(
        &mut self,
        future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    ) -> Task<Result<RefType<Value>, ChaChaError>> {
        let task = self.ex.spawn(future);
        log::debug!(target: "async", "spawn executor: {:?}\n\tspawn task: {:?}", self.ex, task);
        task
    }

    pub async fn run(&mut self) {
        log::debug!(target: "async", "run: {:?}", self.ex);

        while !self.ex.initialized() {}

        while !self.ex.is_empty() {
            self.ex.tick().await;
            future::yield_now().await;
            if !self.ex.running() {
                break;
            }
        }

        log::debug!(target: "async", "run done: {:?}", self.ex);
        self.ex.shutdown();
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
        self.ex.run(task).await
    }

    pub fn tick(&self) {
        log::debug!(target: "async", "tick: {:?}", self.ex);
        future::block_on(async { self.ex.tick().await });
    }

    pub fn try_tick(&self) -> bool {
        log::debug!(target: "async", "try_tick: {:?}", self.ex);
        self.ex.try_tick()
    }
}

/// A thread abstraction for this Rube Goldbergian creation
///
/// The thread may not be what most think of as a thread. To understand it, we
/// need to understand what's happening in the interpreter. We always start with
/// a single thread (traditional) that parses and type checks the program. It
/// then begins execution, traditionally at main. We continue running like this
/// until the first async block is encountered. It is at this point that we need
/// a thread.
///
/// As we are executing async dwarf code there are two points to which we need pay
/// particular attention. One is, as mentioned above, an async block. And the
/// other is an async function call. At each of these points in the interpreter
/// we need to create a future and enqueue it upon an executor so that we may
/// receive a task in return. These tasks are tied to a particular executor. The
/// executor may be ticked to run a task that is ready.
///
/// Note that the executor is run by `tick`ing it. This will poll a single task
/// that is ready. There is code in smol that uses randomization to reorder a
/// list, but I'm not sure if that's part of the scheduler. The point being that
/// we do not have control over what task is run next, in the event that several
/// are ready. I saw this with one executor. Throwing all the tasks in gets
/// things done, but all at the same time.
///
/// It's also worth reiterating that I'm wedging async in -- most of the code is
/// sync.
///
/// Two things are missing. Sequencing of tasks, and
///
/// Maybe only one thing is missing. And that's the conundrum I've been avoiding
/// mentioning, and that's `await`.
///
/// No, I think I did the tiered executors because of the sequencing problem.
/// I need to figure out an example of that. Oh, it happens with spawn and
/// multiple threads-of-execution. No maybe not. Was thinking that tasks from
/// each *toe* were getting interleaved, but that shouldn't cause any problem.
///
/// Back to await. We can't just `block_on` when we hit an await expression
/// because that blocks and we are trying to do this with just one thread. And
/// even if we wanted `n` threads, what about the `n + 1`st blocking call? No,
/// we can't block. So we need a way to run a task and let it return pending,
/// and continue it's normal route through the executor. And we need to stop
/// subsequent execution of the current task until the awaited task is complete.
///
///
///
/// So the interpreter is running along, don't forget that we are executing in
/// a synchronously, but we are inside an async block _in dwarf_, so we have
/// a thread (one of these). And we hit an async function call.
pub(crate) struct ChaChaThread<'a> {
    _foo: PhantomData<&'a ()>,
}
