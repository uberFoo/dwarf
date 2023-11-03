#![cfg(feature = "async")]

use std::{
    future::Future,
    marker::PhantomData,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
    task::{Context, Poll, Waker},
    thread,
};

use async_executor::{Executor, Task};
use backtrace::Backtrace;
use crossbeam::channel::{Receiver, Sender};
use futures_lite::future;
use pin_project_lite::pin_project;

use crate::{chacha::error::ChaChaError, new_ref, NewRef, RefType, Value};

static mut TASK_COUNT: AtomicUsize = AtomicUsize::new(0);
static mut EXECUTOR_COUNT: AtomicUsize = AtomicUsize::new(0);

// pin_project! {
#[derive(Debug)]
pub struct ChaChaTask<'a> {
    // #[pin]
    inner: Option<Task<Result<RefType<Value>, ChaChaError>>>,
    // #[pin]
    executor: &'a ChaChaExecutor<'a>,
    started: AtomicBool,
    waker: Option<Waker>,
    id: usize,
}
// }

impl<'a> ChaChaTask<'a> {
    pub fn new(
        executor: &'a ChaChaExecutor<'a>,
        future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    ) -> ChaChaTask<'a> {
        let id = unsafe { TASK_COUNT.fetch_add(1, Ordering::SeqCst) };
        log::debug!(target: "async", "ChaChaTask::new: {id}");
        log::trace!(target: "async", "Executor: {:?}", executor);
        log::trace!(target: "async", "Thread: {:?}", thread::current().id());

        // spawn a task that spawns a task
        // let inner = executor.clone();
        let inner = executor.ex.clone();
        let future = async move {
            log::debug!(target: "async", "ChaChaTask::new: spawn inner task: {id}");
            log::trace!(target: "async", "Executor: {:?}", inner);
            log::trace!(target: "async", "Thread: {:?}", thread::current().id());
            let task = inner.spawn(future);
            task.await
        };

        Self {
            inner: Some(executor.spawn(future)),
            executor,
            started: AtomicBool::new(false),
            waker: None,
            id,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn detach(&mut self) {
        if let Some(task) = self.inner.take() {
            task.detach();
        }
    }
}

impl<'a> ChaChaTask<'a> {
    pub fn start(&self) {
        if !self.started.load(Ordering::SeqCst) {
            log::debug!(target: "async", "ChaChaTask::start: {}", self.id);
            self.started.store(true, Ordering::SeqCst);
            if let Some(waker) = self.waker.as_ref() {
                waker.clone().wake();
            }
        }
    }
}

impl<'a> Future for ChaChaTask<'a> {
    type Output = Result<RefType<Value>, ChaChaError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // let mut this = self.project();
        let this = std::pin::Pin::into_inner(self);

        if this.started.load(Ordering::SeqCst) {
            log::debug!(
                target: "async",
                "ChaChaTask::poll: ready: {}",
                this.id,);
            log::trace!(target: "async", "Executor: {:?}", this.executor);
            log::trace!(target: "async", "Thread: {:?}", thread::current().id());
            let task = this.inner.take().unwrap();
            // while !this.executor.ex.initialized() {}
            // Poll::Ready(future::block_on(this.executor.ex.run(task)))
            Poll::Ready(future::block_on(this.executor.resolve_task(task)))
        } else {
            log::debug!(
                target: "async",
                "ChaChaTask::poll: pending: {}",
                this.id,);
            log::trace!(target: "async", "Executor: {:?}", this.executor);
            log::trace!(target: "async", "Thread: {:?}", thread::current().id());
            // this.waker = &mut Some(cx.waker().clone());
            this.waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

#[derive(Clone, Debug)]
pub struct ChaChaExecutor<'a> {
    id: usize,
    pub ex: Arc<Executor<'a>>,
}

impl<'a> Drop for ChaChaExecutor<'a> {
    fn drop(&mut self) {
        log::debug!(target: "async", "ChaChaExecutor::drop: {:?}", self);
    }
}

impl<'a> ChaChaExecutor<'a> {
    /// Creates a new executor.
    pub fn new() -> ChaChaExecutor<'a> {
        let id = unsafe { EXECUTOR_COUNT.fetch_add(1, Ordering::SeqCst) };
        ChaChaExecutor {
            id,
            ex: Arc::new(Executor::new()),
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn is_empty(&self) -> bool {
        self.ex.is_empty()
    }

    // pub fn spawn_task(
    //     &mut self,
    //     future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    // ) -> ChaChaTask<'a> {
    //     // let task = self.ex.spawn(future);
    //     ChaChaTask::new(self, future)
    //     // log::debug!(target: "async", "spawn executor: {:?}\n\tspawn task: {:?}", self.ex, task);
    //     // task
    // }

    pub(super) fn spawn(
        &self,
        future: impl Future<Output = Result<RefType<Value>, ChaChaError>> + Send + 'a,
    ) -> Task<Result<RefType<Value>, ChaChaError>> {
        let task = self.ex.spawn(future);
        log::debug!(target: "async", "spawn executor: {:?}\n\tspawn task: {:?}", self, task);
        task
    }

    pub fn shutdown(&self) {
        self.ex.shutdown();
        log::debug!(target: "async", "shutdown: {:?}", self);
    }

    pub fn finished(&self) -> bool {
        self.ex.is_empty()
    }

    pub async fn run(&self) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "ChaChaExecutor::run: {:?}, thread: {:?}", self, thread::current().id());

        // dbg!("initializing", thread::current().id());
        while !self.ex.initialized() && self.ex.running() {}

        while self.ex.running() {
            // log::debug!(target: "async", "ChaChaExecutor::run loop: {:?}, thread: {:?}", self, thread::current().id());
            // dbg!("executing", thread::current().id());
            self.ex.tick().await;
            // log::debug!(target: "async", "ChaChaExecutor::run loop: {:?}, thread: {:?}", self, thread::current().id());
        }
        while !self.ex.is_empty() {
            // log::debug!(target: "async", "ChaChaExecutor::exiting: {:?}, thread: {:?}", self, thread::current().id());
            // dbg!("emptying", thread::current().id());
            if !self.ex.try_tick() {
                break;
            }
        }

        // dbg!("outta here", thread::current().id());

        log::debug!(target: "async", "run done: {:?}, thread: {:?}", self, thread::current().id());

        Ok(new_ref!(Value, Value::Empty))
    }

    pub fn block_on(
        &self,
        task: Task<Result<RefType<Value>, ChaChaError>>,
    ) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "block_on: {:?}", self);
        future::block_on(async { task.await })
    }

    pub async fn resolve_task(
        &self,
        task: Task<Result<RefType<Value>, ChaChaError>>,
    ) -> Result<RefType<Value>, ChaChaError> {
        log::debug!(target: "async", "resolve_task: {:?}", self);
        self.ex.run(task).await
    }

    pub fn tick(&self) {
        log::debug!(target: "async", "tick: {:?}", self);
        future::block_on(async { self.ex.tick().await });
    }

    pub fn try_tick(&self) -> bool {
        log::debug!(target: "async", "try_tick: {:?}", self.ex);
        self.ex.try_tick()
    }
}
