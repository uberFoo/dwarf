//! Memory management for the ChaCha interpreter.

use std::fmt;

use ansi_term::Colour;
use crossbeam::channel::{unbounded, Receiver, Sender};
use rustc_hash::FxHashMap as HashMap;

use crate::{
    chacha::vm::Thonk, debug, function, interpreter::STEPPING, new_ref, s_read, s_write, NewRef,
    RefType, Value,
};

#[derive(Clone, Debug)]
pub struct Memory {
    thonks: Vec<Thonk>,
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
                thonks: Vec::new(),
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

    #[allow(dead_code)]
    pub(crate) fn thonk_index<S: AsRef<str>>(&self, name: S) -> Option<usize> {
        self.thonks
            .iter()
            .enumerate()
            .find(|(_, thonk)| thonk._name == name.as_ref())
            .map(|(index, _)| index)
    }

    pub(crate) fn reserve_thonk_slot(&mut self) -> ThonkReservation {
        let slot = self.thonks.len();
        self.thonks.push(Thonk::new("placeholder".to_string()));
        ThonkReservation { slot }
    }

    pub(crate) fn insert_thonk(&mut self, thonk: Thonk, reservation: ThonkReservation) {
        self.thonks[reservation.slot] = thonk;
    }

    pub(crate) fn get_thonk(&self, index: usize) -> Option<&Thonk> {
        self.thonks.get(index)
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

    // 🚧 Document this -- I'm not sure what the :: business is about.
    pub(crate) fn insert_global(&mut self, name: String, value: RefType<Value>) {
        if name.contains("::") {
            let mut split = name.split("::");
            let table = split.next().unwrap();
            let name = split
                .next()
                .expect("name contained `::`, but no second element");

            if *STEPPING.lock() {
                self.sender
                    .send(MemoryUpdateMessage::AddGlobal((
                        name.to_owned(),
                        value.clone(),
                    )))
                    .unwrap();
            }

            if let Some(value) = self.global.get(table) {
                let mut write_value = s_write!(value);
                if let Value::Table(ref mut table) = *write_value {
                    table.insert(name.to_owned(), value.clone());
                } else {
                    unreachable!()
                }
            } else {
                let mut map = HashMap::default();
                map.insert(name.to_owned(), value);
                self.global
                    .insert(table.to_owned(), new_ref!(Value, Value::Table(map)));
            }
        } else {
            self.global.insert(name, value);
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

pub(crate) struct ThonkReservation {
    slot: usize,
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
