#![cfg(feature = "async")]

use std::{
    future::Future,
    marker::PhantomData,
    mem,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
    task::{Context, Poll, Waker},
    thread,
};

use async_condvar_fair::Condvar;
use async_executor::{Executor as SmolExecutor, Task as SmolTask};
use concurrent_queue::ConcurrentQueue;
use futures_lite::{future, prelude::*};
use once_cell::sync::OnceCell;
use parking_lot::Mutex;
use pin_project_lite::pin_project;
use slab::Slab;
// use smol::lock::Mutex;

use crate::{chacha::error::ChaChaError, new_ref, NewRef, RefType, Value, ValueResult};

#[derive(Debug)]
pub struct Executor<'a> {
    root: usize,
    workers: Slab<Worker<'a>>,
    threads: Vec<thread::JoinHandle<()>>,
    queue: ConcurrentQueue<usize>,
    shutdown: smol::lock::Mutex<bool>,
    waiter: Condvar,
}

impl<'a> Executor<'a> {
    pub fn at_index(index: usize) -> &'static mut Worker<'static> {
        log::debug!(target: "async", "Executor::at_index: {index}");
        let exec = unsafe { EXECUTOR.get_mut().unwrap() };
        log::trace!(target: "async", "Executor::at_index: {exec:?}");
        let executor = exec.workers.get_mut(index).unwrap();
        log::trace!(target: "async", "Executor::at_index: {executor:?}");
        executor
    }

    pub fn global() -> &'static mut Worker<'static> {
        let exec = unsafe { EXECUTOR.get_mut().unwrap() };
        exec.workers.get_mut(exec.root).unwrap()
    }

    pub fn new_worker() -> usize {
        let exec = unsafe { EXECUTOR.get_mut().unwrap() };
        let idx = exec.workers.insert(Worker::new());
        let worker = exec.workers.get_mut(idx).unwrap();
        worker.set_id(idx);
        idx
    }

    pub fn spawn(task: &Task<ValueResult>) {
        let exec = unsafe { EXECUTOR.get().unwrap() };
        task.start();
        exec.queue
            .push(task.worker.id().unwrap())
            .expect("failed to push task");
        exec.waiter.notify_one();
    }

    async fn run() {
        loop {
            let exec = unsafe { EXECUTOR.get().unwrap() };
            loop {
                let _ = match exec.queue.pop().ok() {
                    Some(idx) => {
                        if let Some(worker) = exec.workers.get(idx) {
                            log::debug!(target: "async", "Executor::run: worker found: {idx}");
                            log::trace!(target: "async", "Executor::run: worker: {worker:?}");
                            log::trace!(target: "async", "Executor::run: thread: {:?}", thread::current().id());
                            // while !worker.ex.initialized() {}
                            // while worker.try_tick() {}
                            match worker.try_tick() {
                                true => {
                                    log::debug!(target: "async", "Executor::run: worker ticked: {idx}");
                                    exec.queue.push(idx).expect("failed to push task");
                                }
                                false => {
                                    log::debug!(target: "async", "Executor::run: worker finished: {idx}");
                                    // exec.workers[idx].shutdown();
                                    // exec.workers.remove(idx);
                                }
                            }
                            log::debug!(target: "async", "Executor::run: worker finished: {idx}");
                        } else {
                            log::error!(target: "async", "Executor::run: worker not found: {idx}");
                        }
                    }
                    None => break,
                };
            }
            let shutdown = exec.shutdown.lock().await;
            let shutdown = exec.waiter.wait(shutdown).await;
            if *shutdown {
                break;
            }
        }
    }

    pub fn new(thread_count: usize) {
        let mut workers = Slab::with_capacity(thread_count);
        let worker = Worker::new();
        let root = workers.insert(worker);
        let worker = workers.get_mut(root).unwrap();
        worker.set_id(root);

        unsafe {
            EXECUTOR
                .set(Executor {
                    root,
                    workers,
                    threads: Vec::new(),
                    queue: ConcurrentQueue::unbounded(),
                    shutdown: smol::lock::Mutex::new(false),
                    waiter: Condvar::new(),
                })
                .unwrap();
        };

        let mut threads = Vec::new();
        for _ in 0..thread_count {
            // let worker = worker.clone();
            let handle = thread::spawn(move || {
                log::debug!(target: "async", "Executor::new: thread spawned: {:?}", thread::current().id());
                // let _ = future::block_on(worker.run());
                let _ = future::block_on(Executor::run());
                log::debug!(target: "async", "Executor::new thread exiting: {:?}", thread::current().id());
            });
            threads.push(handle);
        }

        let executor = unsafe { EXECUTOR.get_mut().unwrap() };
        executor.threads = threads;
    }

    pub fn remove_worker(index: usize) -> Worker<'static> {
        log::debug!(target: "async", "Executor::remove_worker: {index}");
        let exec = unsafe { EXECUTOR.get_mut().unwrap() };
        exec.workers[index].shutdown();
        let e = exec.workers.remove(index);
        e
    }

    pub fn shutdown() {
        log::debug!(target: "async", "Executor::shutdown");
        unsafe {
            if let Some(executor) = EXECUTOR.take() {
                let mut shutdown = future::block_on(executor.shutdown.lock());
                *shutdown = true;
                executor.waiter.notify_all();
                // for (_, mut worker) in executor.workers {
                //     worker.shutdown();
                // }

                for handle in executor.threads {
                    handle.join().unwrap();
                }
            }
        }
    }
}

static mut EXECUTOR: OnceCell<Executor> = OnceCell::new();
static mut TASK_COUNT: AtomicUsize = AtomicUsize::new(0);
static mut EXECUTOR_COUNT: AtomicUsize = AtomicUsize::new(0);

// pin_project! {
#[derive(Debug)]
pub struct Task<'a, T> {
    // #[pin]
    inner: Option<SmolTask<T>>,
    // #[pin]
    worker: Worker<'a>,
    started: AtomicBool,
    waker: Option<Waker>,
    id: usize,
}
// }

impl<'a, T> Task<'a, T> {
    pub fn new(worker: &'a Worker<'a>, future: impl Future<Output = T> + Send + 'a) -> Task<'a, T>
    where
        T: Send + 'a,
    {
        let id = unsafe { TASK_COUNT.fetch_add(1, Ordering::SeqCst) };
        log::debug!(target: "async", "ChaChaTask::new: {id}");
        log::trace!(target: "async", "ChaChaTask::new worker: {:?}", worker);
        log::trace!(target: "async", "ChaChaTask::new thread: {:?}", thread::current().id());

        // spawn a task that spawns a task
        let inner = worker.clone();
        // let inner = executor.ex.clone();
        let future = async move {
            log::debug!(target: "async", "ChaChaTask::new: spawn inner task: {id}");
            log::trace!(target: "async", "ChaChaTask::new inner: {:?}", inner);
            log::trace!(target: "async", "ChaChaTask::new thread: {:?}", thread::current().id());
            let task = inner.spawn(future);
            let result = task.await;
            // mem::forget(inner);
            result
        };

        Self {
            inner: Some(worker.spawn(future)),
            worker: worker.clone(),
            started: AtomicBool::new(false),
            waker: None,
            id,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

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

impl<'a, T> Future for Task<'a, T>
where
    T: std::fmt::Debug,
{
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        log::debug!(target: "async", "ChaCha::poll {}\n{:?}", self.id, thread::current().id());
        // let mut this = self.project();
        let this = std::pin::Pin::into_inner(self);

        if this.started.load(Ordering::SeqCst) {
            log::debug!(
                target: "async",
                "ChaChaTask::poll: ready: {}",
                this.id,);
            log::trace!(target: "async", "Task::poll worker:: {:?}", this.worker);
            log::trace!(target: "async", "Task::poll thread: {:?}", thread::current().id());
            let task = this.inner.take().unwrap();
            // while !this.executor.ex.initialized() {}
            // Poll::Ready(future::block_on(this.executor.ex.run(task)))
            Poll::Ready(future::block_on(this.worker.resolve_task(task)))
        } else {
            log::debug!(
                target: "async",
                "ChaChaTask::poll: pending: {}",
                this.id,);
            log::trace!(target: "async", "Task::poll worker:: {:?}", this.worker);
            log::trace!(target: "async", "Task::poll thread: {:?}", thread::current().id());
            // this.waker = &mut Some(cx.waker().clone());
            this.waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

#[derive(Clone, Debug)]
pub struct Worker<'a> {
    id: Option<usize>,
    pub ex: Arc<SmolExecutor<'a>>,
    shutdown: Arc<Mutex<bool>>,
    waiter: Arc<Condvar>,
}

// impl<'a> Drop for ChaChaExecutor<'a> {
//     fn drop(&mut self) {
//         log::debug!(target: "async", "ChaChaExecutor::drop: {:?}", self);
//     }
// }

impl<'a> Worker<'a> {
    /// Creates a new executor.
    pub fn new() -> Worker<'a> {
        // let id = unsafe { EXECUTOR_COUNT.fetch_add(1, Ordering::SeqCst) };
        Worker {
            id: None,
            ex: Arc::new(SmolExecutor::new()),
            shutdown: Arc::new(Mutex::new(false)),
            waiter: Arc::new(Condvar::new()),
        }
    }

    pub fn set_id(&mut self, id: usize) {
        self.id = Some(id);
    }

    pub fn id(&self) -> Option<usize> {
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

    pub(super) fn spawn<T>(&self, future: impl Future<Output = T> + Send + 'a) -> SmolTask<T>
    where
        T: Send + 'a,
    {
        let task = self.ex.spawn(future);
        self.waiter.notify_all();
        log::debug!(target: "async", "spawn executor: {:?}\n\tspawn task: {:?}", self, task);
        task
    }

    pub fn shutdown(&mut self) {
        let mut shutdown = self.shutdown.lock();
        *shutdown = true;
        self.waiter.notify_all();
        self.ex.shutdown();
        log::debug!(target: "async", "ChaChaExecutor: shutdown: {:?}", self);
    }

    pub fn finished(&self) -> bool {
        self.ex.is_empty()
    }

    // pub async fn run(&self) {
    //     log::debug!(target: "async", "ChaChaExecutor::run: {:?}, thread: {:?}", self, thread::current().id());

    //     // dbg!("initializing", thread::current().id());
    //     while !self.ex.initialized() && self.ex.running() {}

    //     // while self.ex.running() {
    //     // while !self.shutdown && self.ex.running() {
    //     //     log::debug!(target: "async", "ChaChaExecutor::run loop pre: {:?}, thread: {:?}", self, thread::current().id());
    //     //     // dbg!("executing", thread::current().id());
    //     //     self.ex.tick().await;
    //     //     log::debug!(target: "async", "ChaChaExecutor::run loop post: {:?}, thread: {:?}", self, thread::current().id());
    //     // }
    //     let mut running = true;
    //     while running {
    //         self.ex
    //             .tick()
    //             .or(async {
    //                 let shutdown = self.shutdown.lock();
    //                 // dbg!("fuck me", thread::current().id());
    //                 let shutdown = self.waiter.wait(shutdown).await;
    //                 // dbg!("in the ass", thread::current().id());
    //                 if *shutdown {
    //                     // dbg!("bye bye", thread::current().id());
    //                     running = false;
    //                 }
    //             })
    //             .await;
    //     }

    //     log::debug!(target: "async", "Escaped!");

    //     while !self.ex.is_empty() {
    //         // log::debug!(target: "async", "ChaChaExecutor::exiting: {:?}, thread: {:?}", self, thread::current().id());
    //         // dbg!("emptying", thread::current().id());
    //         if !self.ex.try_tick() {
    //             break;
    //         }
    //     }

    //     // dbg!("outta here", thread::current().id());

    //     log::debug!(target: "async", "run done: {:?}, thread: {:?}", self, thread::current().id());
    // }

    pub fn block_on<T>(&self, task: SmolTask<T>) -> T {
        log::debug!(target: "async", "block_on: {:?}", self);
        future::block_on(async { task.await })
    }

    pub async fn resolve_task<T>(&self, task: SmolTask<T>) -> T
    where
        T: std::fmt::Debug,
    {
        log::debug!(target: "async", "ChaChaExecutor: resolve_task: {self:?}");
        let result = self.ex.run(task).await;
        log::debug!(target: "async", "ChaChaExecutor: resolve_task: {self:?}, RESOLVED: {result:?}");
        result
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
