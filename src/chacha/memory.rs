//! Memory management for the ChaCha interpreter.

use std::fmt;

use ansi_term::Colour;
use crossbeam::channel::{unbounded, Receiver, Sender};
use rustc_hash::FxHashMap as HashMap;

use crate::{bubba::Thonk, debug, function, interpreter::STEPPING, s_read, RefType, Value};

#[derive(Clone, Debug)]
pub struct Memory {
    thonks: HashMap<String, Thonk>,
    meta: HashMap<String, HashMap<String, RefType<Value>>>,
    global: HashMap<String, RefType<Value>>,
    frames: Vec<HashMap<String, RefType<Value>>>,
    sender: Sender<MemoryUpdateMessage>,
}

impl Memory {
    pub(crate) fn new() -> (Self, Receiver<MemoryUpdateMessage>) {
        let (sender, receiver) = unbounded();

        (
            Memory {
                thonks: HashMap::default(),
                meta: HashMap::default(),
                global: HashMap::default(),
                frames: vec![HashMap::default()],
                sender,
            },
            receiver,
        )
    }

    pub fn get_globals(&self) -> Vec<(&str, &RefType<Value>)> {
        self.global
            .iter()
            .map(|(name, value)| (name.as_str(), value))
            .collect()
    }

    pub fn get_frames(&self) -> Vec<Vec<(&str, &RefType<Value>)>> {
        self.frames
            .iter()
            .map(|frame| {
                frame
                    .iter()
                    .map(|(name, value)| (name.as_str(), value))
                    .collect()
            })
            .collect()
    }

    pub(crate) fn insert_thonk(&mut self, thonk: Thonk) {
        self.thonks.insert(thonk.get_name().to_owned(), thonk);
    }

    pub(crate) fn get_thonk(&self, name: &str) -> Option<&Thonk> {
        self.thonks.get(name)
    }

    pub(crate) fn push_frame(&mut self) {
        if *STEPPING.lock() {
            self.sender.send(MemoryUpdateMessage::PushFrame).unwrap();
        }
        self.frames.push(HashMap::default());
    }

    pub(crate) fn pop_frame(&mut self) {
        if *STEPPING.lock() {
            self.sender.send(MemoryUpdateMessage::PopFrame).unwrap();
        }
        self.frames.pop();
    }

    pub(crate) fn insert_meta_table(&mut self, table: String) {
        self.meta.insert(table, HashMap::default());
    }

    pub(crate) fn insert_meta(&mut self, table: &str, name: String, value: RefType<Value>) {
        let table = self.meta.get_mut(table).unwrap();
        table.insert(name, value);
    }

    pub(crate) fn get_meta(&self, table: &str, name: &str) -> Option<RefType<Value>> {
        if let Some(table) = self.meta.get(table) {
            table.get(name).cloned()
        } else {
            None
        }
    }

    pub(crate) fn insert(&mut self, name: String, value: RefType<Value>) {
        if *STEPPING.lock() {
            self.sender
                .send(MemoryUpdateMessage::AddLocal((name.clone(), value.clone())))
                .unwrap();
        }
        let frame = self.frames.last_mut().unwrap();
        frame.insert(name, value);
    }

    pub(crate) fn get(&self, name: &str) -> Option<RefType<Value>> {
        debug!("memory", "name: {name}");
        if name.contains("::") {
            let mut split = name.split("::");

            let name = split.next().unwrap();
            let table = split.next().unwrap();

            if let Some(value) = self.get_simple(table) {
                let value = s_read!(value);
                if let Value::Table(ref table) = *value {
                    table.get(name).cloned()
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            self.get_simple(name).cloned()
        }
    }

    pub(crate) fn get_simple(&self, name: &str) -> Option<&RefType<Value>> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        self.global.get(name)
    }
}

type MemoryCell = (String, RefType<Value>);

#[derive(Clone, Debug)]
pub enum MemoryUpdateMessage {
    AddGlobal(MemoryCell),
    AddMeta(MemoryCell),
    PushFrame,
    PopFrame,
    AddLocal(MemoryCell),
}

impl fmt::Display for MemoryUpdateMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemoryUpdateMessage::AddGlobal((name, value)) => {
                write!(f, "add global {}: {}", name, s_read!(value))
            }
            MemoryUpdateMessage::AddMeta((name, value)) => {
                write!(f, "add meta {}: {}", name, s_read!(value))
            }
            MemoryUpdateMessage::PushFrame => write!(f, "push frame"),
            MemoryUpdateMessage::PopFrame => write!(f, "pop frame"),
            MemoryUpdateMessage::AddLocal((name, value)) => {
                write!(f, "add local {}: {}", name, s_read!(value))
            }
        }
    }
}
