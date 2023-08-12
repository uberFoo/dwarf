use circular_queue::CircularQueue;
use crossbeam::channel::{Receiver, Sender};

use crate::{
    interpreter::{DebuggerStatus, Memory, MemoryUpdateMessage},
    lu_dog::{Block, ObjectStore as LuDogStore},
    new_ref, s_read,
    sarzak::ObjectStore as SarzakStore,
    ModelStore, NewRef, RefType, Value,
};

// #[derive(Clone, Debug)]
pub struct Context {
    /// The prompt to display in the REPL
    prompt: String,
    /// The root block, used by the REPL
    block: RefType<Block>,
    memory: Memory,
    lu_dog: RefType<LuDogStore>,
    sarzak: RefType<SarzakStore>,
    models: RefType<ModelStore>,
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
impl Drop for Context {
    fn drop(&mut self) {
        // s_read!(self.lu_dog)
        //     .unwrap()
        //     .persist_bincode(&self.obj_file_path)
        //     .unwrap();
    }
}

#[allow(clippy::too_many_arguments)]
impl Context {
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
    ) -> Self {
        Self {
            prompt,
            block,
            memory,
            lu_dog,
            sarzak,
            models,
            mem_update_recv,
            std_out_send,
            std_out_recv,
            debug_status_writer,
            timings,
            expr_count,
            func_calls,
            args,
        }
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
        let source = s_read!(self.lu_dog)
            .iter_dwarf_source_file()
            .next()
            .unwrap();
        let source = s_read!(source);
        source.source.clone()
    }

    pub fn lu_dog_heel(&self) -> &RefType<LuDogStore> {
        &self.lu_dog
    }

    pub fn block(&self) -> &RefType<Block> {
        &self.block
    }

    pub fn sarzak_heel(&self) -> &RefType<SarzakStore> {
        &self.sarzak
    }

    pub fn models(&self) -> &RefType<ModelStore> {
        &self.models
    }

    // pub fn register_store_proxy(&mut self, name: String, proxy: impl ProxyType + 'static) {
    //     self.memory.insert_global(
    //         name.clone(),
    //         new_ref!(
    //             Value,
    //             Value::ProxyType(new_ref!(Box<dyn ProxyType>, Box::new(proxy)))
    //         ),
    //     );

    //     let mut lu_dog = s_write!(self.lu_dog);
    //     let local = LocalVariable::new(Uuid::new_v4(), &mut lu_dog);
    //     let var = Variable::new_local_variable(name.clone(), &local, &mut lu_dog);
    //     let import = Import::new(
    //         "So ugly".to_owned(),
    //         false,
    //         name,
    //         "path".to_owned(),
    //         None,
    //         &mut lu_dog,
    //     );

    //     let _value = XValue::new_variable(
    //         &self.block,
    //         &ValueType::new_import(&import, &mut lu_dog),
    //         &var,
    //         &mut lu_dog,
    //     );
    //     // {
    //     //     // Build the ASTs
    //     //     let local = LocalVariable::new(Uuid::new_v4(), &mut *s_write!(lu_dog));
    //     //     let var = Variable::new_local_variable(
    //     //         "MERLIN_STORE".to_owned(),
    //     //         local,
    //     //         &mut *s_write!(lu_dog),
    //     //     );

    //     //     let store = ZObjectStore::new("merlin".to_owned(), &mut *s_write!(lu_dog));
    //     //     let mut write = s_write!(lu_dog);
    //     //     let _value = LuDogValue::new_variable(
    //     //         block.clone(),
    //     //         ValueType::new_z_object_store(store, &mut write),
    //     //         var,
    //     //         &mut write,
    //     //     );
    //     // }
    // }
}
