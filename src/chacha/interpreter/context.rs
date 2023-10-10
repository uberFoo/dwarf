use std::path::PathBuf;

use circular_queue::CircularQueue;
use crossbeam::channel::{Receiver, Sender};

#[cfg(feature = "async")]
use crate::chacha::r#async::ChaChaExecutor;

use crate::{
    interpreter::{DebuggerStatus, Memory, MemoryUpdateMessage},
    lu_dog::{Block, ObjectStore as LuDogStore},
    new_ref, s_read, s_write,
    sarzak::ObjectStore as SarzakStore,
    Dirty, ModelStore, NewRef, RefType, Value,
};

#[derive(Clone, Debug)]
pub struct ModelContext {
    lu_dog: RefType<LuDogStore>,
    sarzak: RefType<SarzakStore>,
    models: RefType<ModelStore>,
}

impl ModelContext {
    pub fn new(
        lu_dog: RefType<LuDogStore>,
        sarzak: RefType<SarzakStore>,
        models: RefType<ModelStore>,
    ) -> Self {
        Self {
            lu_dog,
            sarzak,
            models,
        }
    }

    pub fn lu_dog(&self) -> &RefType<LuDogStore> {
        &self.lu_dog
    }

    pub fn sarzak(&self) -> &RefType<SarzakStore> {
        &self.sarzak
    }

    pub fn models(&self) -> &RefType<ModelStore> {
        &self.models
    }
}

// #[derive(Clone)]
#[derive(Debug)]
pub struct Context<'a> {
    models: ModelContext,
    /// The prompt to display in the REPL
    prompt: String,
    /// The root block, used by the REPL
    block: RefType<Block>,
    memory: Memory,
    mem_update_recv: Receiver<MemoryUpdateMessage>,
    #[allow(dead_code)]
    std_out_send: Sender<String>,
    std_out_recv: Receiver<String>,
    debug_status_writer: Option<Sender<DebuggerStatus>>,
    // obj_file_path: Option<PathBuf>,
    timings: CircularQueue<f64>,
    expr_count: usize,
    func_calls: usize,
    args: Option<RefType<Value>>,
    dwarf_home: PathBuf,
    dirty: Vec<Dirty>,
    #[cfg(feature = "async")]
    executors: Vec<ChaChaExecutor<'a>>,
}

/// Save the lu_dog model when the context is dropped
///
/// NB: This doesn't work. The thread that started us apparently goes away
/// before we get a chance to run this to completion. That's my current
/// working hypothesis.
///
/// Shouldn't this work if we are joining the threads? Maybe I wasn't doing that?
/// Do I still need this?
/// I do if we want to save the model on exit.
// impl Drop for Context {
//     fn drop(&mut self) {
//         // s_read!(self.lu_dog)
//         //     .unwrap()
//         //     .persist_bincode(&self.obj_file_path)
//         //     .unwrap();
//     }
// }

impl<'a> Drop for Context<'a> {
    fn drop(&mut self) {
        log::debug!("dropping context");
    }
}

#[allow(clippy::too_many_arguments)]
impl<'a> Context<'a> {
    pub fn new(
        prompt: String,
        block: RefType<Block>,
        memory: Memory,
        lu_dog: RefType<LuDogStore>,
        sarzak: RefType<SarzakStore>,
        models: RefType<ModelStore>,
        mem_update_recv: Receiver<MemoryUpdateMessage>,
        std_out_send: Sender<String>,
        std_out_recv: Receiver<String>,
        debug_status_writer: Option<Sender<DebuggerStatus>>,
        timings: CircularQueue<f64>,
        expr_count: usize,
        func_calls: usize,
        args: Option<RefType<Value>>,
        dwarf_home: PathBuf,
        dirty: Vec<Dirty>,
    ) -> Self {
        Self {
            prompt,
            block,
            memory,
            models: ModelContext::new(lu_dog, sarzak, models),
            mem_update_recv,
            std_out_send,
            std_out_recv,
            debug_status_writer,
            timings,
            expr_count,
            func_calls,
            args,
            dwarf_home,
            dirty,
            #[cfg(feature = "async")]
            executors: vec![ChaChaExecutor::new()],
        }
    }

    pub fn from_context(&self) -> Self {
        Self {
            prompt: self.prompt.clone(),
            block: self.block.clone(),
            memory: self.memory.clone(),
            models: self.models.clone(),
            mem_update_recv: self.mem_update_recv.clone(),
            std_out_send: self.std_out_send.clone(),
            std_out_recv: self.std_out_recv.clone(),
            debug_status_writer: self.debug_status_writer.clone(),
            timings: self.timings.clone(),
            expr_count: 0,
            func_calls: 0,
            args: self.args.clone(),
            dwarf_home: self.dwarf_home.clone(),
            dirty: self.dirty.clone(),
            #[cfg(feature = "async")]
            executors: self.executors.clone(),
        }
    }

    #[cfg(feature = "async")]
    pub fn executor(&self) -> &ChaChaExecutor<'a> {
        &self.executors.last().unwrap()
    }

    #[cfg(feature = "async")]
    pub fn executors(&self) -> &[ChaChaExecutor<'a>] {
        &self.executors
    }

    pub fn dirty(&self) -> Vec<Dirty> {
        self.dirty.clone()
    }

    pub fn clear_dirty(&mut self) {
        self.dirty.clear();
    }

    pub fn set_dirty(&mut self, dirty: Vec<Dirty>) {
        self.dirty = dirty;
    }

    pub fn std_out_recv(&self) -> &Receiver<String> {
        &self.std_out_recv
    }

    pub fn debug_status_writer(&self) -> Option<&Sender<DebuggerStatus>> {
        self.debug_status_writer.as_ref()
    }

    pub fn set_debug_status_writer(&mut self, writer: Sender<DebuggerStatus>) {
        self.debug_status_writer = Some(writer);
    }

    pub fn add_args(&mut self, args: Vec<String>) {
        self.args = Some(new_ref!(Value, args.into()));
    }

    // pub fn register_model<P>(&self, model_name: String, model_path: P) -> Result<()>
    // where
    //     P: AsRef<Path>,
    // {
    //     let model =
    //         SarzakStore::load(model_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

    //     s_write!(self.models).insert(model_name, model);

    //     Ok(())
    // }

    pub fn register_memory_updates(&self) -> Receiver<MemoryUpdateMessage> {
        self.mem_update_recv.clone()
    }

    pub fn std_out_send(&self) -> &Sender<String> {
        &self.std_out_send
    }

    pub fn get_std_out(&self) -> Receiver<String> {
        self.std_out_recv.clone()
    }

    pub fn drain_std_out(&self) -> Vec<String> {
        let mut out = Vec::new();
        while let Ok(line) = self.std_out_recv.try_recv() {
            out.push(line);
        }
        out
    }

    pub fn get_args(&self) -> Option<RefType<Value>> {
        self.args.clone()
    }

    pub fn get_home(&self) -> &PathBuf {
        &self.dwarf_home
    }

    pub fn memory(&mut self) -> &mut Memory {
        &mut self.memory
    }

    pub fn prompt(&self) -> &str {
        &self.prompt
    }

    #[inline]
    pub fn increment_expression_count(&mut self, count: usize) {
        self.expr_count += count;
    }

    #[inline]
    pub fn increment_call_count(&mut self) {
        self.func_calls += 1;
    }

    #[inline]
    pub fn get_expression_count(&self) -> usize {
        self.expr_count
    }

    pub fn get_timings(&self) -> Vec<f64> {
        self.timings.iter().copied().collect()
    }

    #[inline]
    pub fn new_timing(&mut self, timing: f64) {
        self.timings.push(timing);
    }

    pub fn source(&self) -> String {
        let source = s_read!(self.models.lu_dog())
            .iter_dwarf_source_file()
            .next()
            .unwrap();
        let source = s_read!(source);
        source.source.clone()
    }

    pub fn lu_dog_heel(&self) -> &RefType<LuDogStore> {
        &self.models.lu_dog()
    }

    pub fn block(&self) -> &RefType<Block> {
        &self.block
    }

    pub fn sarzak_heel(&self) -> &RefType<SarzakStore> {
        &self.models.sarzak()
    }

    pub fn models(&self) -> &ModelContext {
        &self.models
    }
}
