//! merlin Object Store
//!
//! The ObjectStore contains instances of objects in the domain.
//! The instances are stored in a hash map, keyed by the object's UUID.
//! This is used during code generation, and probably not useful elsewhere.
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"merlin-object-store-file"}}}
//!
//! # Contents:
//!
//! * [`Anchor`]
//! * [`Bisection`]
//! * [`XBox`]
//! * [`Edge`]
//! * [`Glyph`]
//! * [`Line`]
//! * [`LineSegment`]
//! * [`LineSegmentPoint`]
//! * [`Point`]
//! * [`RelationshipName`]
//! * [`RelationshipPhrase`]
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"merlin-object-store-definition"}}}
use std::cell::RefCell;
use std::rc::Rc;
use std::{
    fs,
    io::{self, prelude::*},
    path::Path,
};

use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::merlin::types::{
    Anchor, Bisection, Edge, Glyph, Line, LineSegment, LineSegmentPoint, Point, RelationshipName,
    RelationshipPhrase, XBox, BOTTOM, LEFT, RIGHT, TOP,
};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ObjectStore {
    anchor: Rc<RefCell<HashMap<Uuid, Rc<RefCell<Anchor>>>>>,
    bisection: Rc<RefCell<HashMap<Uuid, Rc<RefCell<Bisection>>>>>,
    x_box: Rc<RefCell<HashMap<Uuid, Rc<RefCell<XBox>>>>>,
    edge: Rc<RefCell<HashMap<Uuid, Rc<RefCell<Edge>>>>>,
    glyph: Rc<RefCell<HashMap<Uuid, Rc<RefCell<Glyph>>>>>,
    line: Rc<RefCell<HashMap<Uuid, Rc<RefCell<Line>>>>>,
    line_segment: Rc<RefCell<HashMap<Uuid, Rc<RefCell<LineSegment>>>>>,
    line_segment_point: Rc<RefCell<HashMap<Uuid, Rc<RefCell<LineSegmentPoint>>>>>,
    point: Rc<RefCell<HashMap<Uuid, Rc<RefCell<Point>>>>>,
    relationship_name: Rc<RefCell<HashMap<Uuid, Rc<RefCell<RelationshipName>>>>>,
    relationship_phrase: Rc<RefCell<HashMap<Uuid, Rc<RefCell<RelationshipPhrase>>>>>,
}

impl ObjectStore {
    pub fn new() -> Self {
        let mut store = Self {
            anchor: Rc::new(RefCell::new(HashMap::default())),
            bisection: Rc::new(RefCell::new(HashMap::default())),
            x_box: Rc::new(RefCell::new(HashMap::default())),
            edge: Rc::new(RefCell::new(HashMap::default())),
            glyph: Rc::new(RefCell::new(HashMap::default())),
            line: Rc::new(RefCell::new(HashMap::default())),
            line_segment: Rc::new(RefCell::new(HashMap::default())),
            line_segment_point: Rc::new(RefCell::new(HashMap::default())),
            point: Rc::new(RefCell::new(HashMap::default())),
            relationship_name: Rc::new(RefCell::new(HashMap::default())),
            relationship_phrase: Rc::new(RefCell::new(HashMap::default())),
        };

        // Initialize Singleton Subtypes
        // 💥 Look at how beautiful this generated code is for super/sub-type graphs!
        // I remember having a bit of a struggle making it work. It's recursive, with
        // a lot of special cases, and I think it calls other recursive functions...💥
        store.inter_edge(Rc::new(RefCell::new(Edge::Bottom(BOTTOM))));
        store.inter_edge(Rc::new(RefCell::new(Edge::Left(LEFT))));
        store.inter_edge(Rc::new(RefCell::new(Edge::Right(RIGHT))));
        store.inter_edge(Rc::new(RefCell::new(Edge::Top(TOP))));

        store
    }

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"merlin-object-store-methods"}}}
    /// Inter (insert) [`Anchor`] into the store.
    ///
    pub fn inter_anchor(&mut self, anchor: Rc<RefCell<Anchor>>) {
        let read = anchor.borrow();
        self.anchor.borrow_mut().insert(read.id, anchor.clone());
    }

    /// Exhume (get) [`Anchor`] from the store.
    ///
    pub fn exhume_anchor(&self, id: &Uuid) -> Option<Rc<RefCell<Anchor>>> {
        self.anchor.borrow().get(id).map(|anchor| anchor.clone())
    }

    /// Exorcise (remove) [`Anchor`] from the store.
    ///
    pub fn exorcise_anchor(&mut self, id: &Uuid) -> Option<Rc<RefCell<Anchor>>> {
        self.anchor
            .borrow_mut()
            .remove(id)
            .map(|anchor| anchor.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Anchor>`.
    ///
    pub fn iter_anchor(&self) -> impl Iterator<Item = Rc<RefCell<Anchor>>> + '_ {
        let values: Vec<Rc<RefCell<Anchor>>> = self
            .anchor
            .borrow()
            .values()
            .map(|anchor| anchor.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Bisection`] into the store.
    ///
    pub fn inter_bisection(&mut self, bisection: Rc<RefCell<Bisection>>) {
        let read = bisection.borrow();
        self.bisection
            .borrow_mut()
            .insert(read.id, bisection.clone());
    }

    /// Exhume (get) [`Bisection`] from the store.
    ///
    pub fn exhume_bisection(&self, id: &Uuid) -> Option<Rc<RefCell<Bisection>>> {
        self.bisection
            .borrow()
            .get(id)
            .map(|bisection| bisection.clone())
    }

    /// Exorcise (remove) [`Bisection`] from the store.
    ///
    pub fn exorcise_bisection(&mut self, id: &Uuid) -> Option<Rc<RefCell<Bisection>>> {
        self.bisection
            .borrow_mut()
            .remove(id)
            .map(|bisection| bisection.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Bisection>`.
    ///
    pub fn iter_bisection(&self) -> impl Iterator<Item = Rc<RefCell<Bisection>>> + '_ {
        let values: Vec<Rc<RefCell<Bisection>>> = self
            .bisection
            .borrow()
            .values()
            .map(|bisection| bisection.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`XBox`] into the store.
    ///
    pub fn inter_x_box(&mut self, x_box: Rc<RefCell<XBox>>) {
        let read = x_box.borrow();
        self.x_box.borrow_mut().insert(read.id, x_box.clone());
    }

    /// Exhume (get) [`XBox`] from the store.
    ///
    pub fn exhume_x_box(&self, id: &Uuid) -> Option<Rc<RefCell<XBox>>> {
        self.x_box.borrow().get(id).map(|x_box| x_box.clone())
    }

    /// Exorcise (remove) [`XBox`] from the store.
    ///
    pub fn exorcise_x_box(&mut self, id: &Uuid) -> Option<Rc<RefCell<XBox>>> {
        self.x_box
            .borrow_mut()
            .remove(id)
            .map(|x_box| x_box.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, XBox>`.
    ///
    pub fn iter_x_box(&self) -> impl Iterator<Item = Rc<RefCell<XBox>>> + '_ {
        let values: Vec<Rc<RefCell<XBox>>> = self
            .x_box
            .borrow()
            .values()
            .map(|x_box| x_box.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Edge`] into the store.
    ///
    pub fn inter_edge(&mut self, edge: Rc<RefCell<Edge>>) {
        let read = edge.borrow();
        self.edge.borrow_mut().insert(read.id(), edge.clone());
    }

    /// Exhume (get) [`Edge`] from the store.
    ///
    pub fn exhume_edge(&self, id: &Uuid) -> Option<Rc<RefCell<Edge>>> {
        self.edge.borrow().get(id).map(|edge| edge.clone())
    }

    /// Exorcise (remove) [`Edge`] from the store.
    ///
    pub fn exorcise_edge(&mut self, id: &Uuid) -> Option<Rc<RefCell<Edge>>> {
        self.edge.borrow_mut().remove(id).map(|edge| edge.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Edge>`.
    ///
    pub fn iter_edge(&self) -> impl Iterator<Item = Rc<RefCell<Edge>>> + '_ {
        let values: Vec<Rc<RefCell<Edge>>> = self
            .edge
            .borrow()
            .values()
            .map(|edge| edge.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Glyph`] into the store.
    ///
    pub fn inter_glyph(&mut self, glyph: Rc<RefCell<Glyph>>) {
        let read = glyph.borrow();
        self.glyph.borrow_mut().insert(read.id, glyph.clone());
    }

    /// Exhume (get) [`Glyph`] from the store.
    ///
    pub fn exhume_glyph(&self, id: &Uuid) -> Option<Rc<RefCell<Glyph>>> {
        self.glyph.borrow().get(id).map(|glyph| glyph.clone())
    }

    /// Exorcise (remove) [`Glyph`] from the store.
    ///
    pub fn exorcise_glyph(&mut self, id: &Uuid) -> Option<Rc<RefCell<Glyph>>> {
        self.glyph
            .borrow_mut()
            .remove(id)
            .map(|glyph| glyph.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Glyph>`.
    ///
    pub fn iter_glyph(&self) -> impl Iterator<Item = Rc<RefCell<Glyph>>> + '_ {
        let values: Vec<Rc<RefCell<Glyph>>> = self
            .glyph
            .borrow()
            .values()
            .map(|glyph| glyph.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Line`] into the store.
    ///
    pub fn inter_line(&mut self, line: Rc<RefCell<Line>>) {
        let read = line.borrow();
        self.line.borrow_mut().insert(read.id, line.clone());
    }

    /// Exhume (get) [`Line`] from the store.
    ///
    pub fn exhume_line(&self, id: &Uuid) -> Option<Rc<RefCell<Line>>> {
        self.line.borrow().get(id).map(|line| line.clone())
    }

    /// Exorcise (remove) [`Line`] from the store.
    ///
    pub fn exorcise_line(&mut self, id: &Uuid) -> Option<Rc<RefCell<Line>>> {
        self.line.borrow_mut().remove(id).map(|line| line.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Line>`.
    ///
    pub fn iter_line(&self) -> impl Iterator<Item = Rc<RefCell<Line>>> + '_ {
        let values: Vec<Rc<RefCell<Line>>> = self
            .line
            .borrow()
            .values()
            .map(|line| line.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`LineSegment`] into the store.
    ///
    pub fn inter_line_segment(&mut self, line_segment: Rc<RefCell<LineSegment>>) {
        let read = line_segment.borrow();
        self.line_segment
            .borrow_mut()
            .insert(read.id, line_segment.clone());
    }

    /// Exhume (get) [`LineSegment`] from the store.
    ///
    pub fn exhume_line_segment(&self, id: &Uuid) -> Option<Rc<RefCell<LineSegment>>> {
        self.line_segment
            .borrow()
            .get(id)
            .map(|line_segment| line_segment.clone())
    }

    /// Exorcise (remove) [`LineSegment`] from the store.
    ///
    pub fn exorcise_line_segment(&mut self, id: &Uuid) -> Option<Rc<RefCell<LineSegment>>> {
        self.line_segment
            .borrow_mut()
            .remove(id)
            .map(|line_segment| line_segment.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, LineSegment>`.
    ///
    pub fn iter_line_segment(&self) -> impl Iterator<Item = Rc<RefCell<LineSegment>>> + '_ {
        let values: Vec<Rc<RefCell<LineSegment>>> = self
            .line_segment
            .borrow()
            .values()
            .map(|line_segment| line_segment.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`LineSegmentPoint`] into the store.
    ///
    pub fn inter_line_segment_point(&mut self, line_segment_point: Rc<RefCell<LineSegmentPoint>>) {
        let read = line_segment_point.borrow();
        self.line_segment_point
            .borrow_mut()
            .insert(read.id, line_segment_point.clone());
    }

    /// Exhume (get) [`LineSegmentPoint`] from the store.
    ///
    pub fn exhume_line_segment_point(&self, id: &Uuid) -> Option<Rc<RefCell<LineSegmentPoint>>> {
        self.line_segment_point
            .borrow()
            .get(id)
            .map(|line_segment_point| line_segment_point.clone())
    }

    /// Exorcise (remove) [`LineSegmentPoint`] from the store.
    ///
    pub fn exorcise_line_segment_point(
        &mut self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<LineSegmentPoint>>> {
        self.line_segment_point
            .borrow_mut()
            .remove(id)
            .map(|line_segment_point| line_segment_point.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, LineSegmentPoint>`.
    ///
    pub fn iter_line_segment_point(
        &self,
    ) -> impl Iterator<Item = Rc<RefCell<LineSegmentPoint>>> + '_ {
        let values: Vec<Rc<RefCell<LineSegmentPoint>>> = self
            .line_segment_point
            .borrow()
            .values()
            .map(|line_segment_point| line_segment_point.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Point`] into the store.
    ///
    pub fn inter_point(&mut self, point: Rc<RefCell<Point>>) {
        let read = point.borrow();
        self.point.borrow_mut().insert(read.id, point.clone());
    }

    /// Exhume (get) [`Point`] from the store.
    ///
    pub fn exhume_point(&self, id: &Uuid) -> Option<Rc<RefCell<Point>>> {
        self.point.borrow().get(id).map(|point| point.clone())
    }

    /// Exorcise (remove) [`Point`] from the store.
    ///
    pub fn exorcise_point(&mut self, id: &Uuid) -> Option<Rc<RefCell<Point>>> {
        self.point
            .borrow_mut()
            .remove(id)
            .map(|point| point.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Point>`.
    ///
    pub fn iter_point(&self) -> impl Iterator<Item = Rc<RefCell<Point>>> + '_ {
        let values: Vec<Rc<RefCell<Point>>> = self
            .point
            .borrow()
            .values()
            .map(|point| point.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`RelationshipName`] into the store.
    ///
    pub fn inter_relationship_name(&mut self, relationship_name: Rc<RefCell<RelationshipName>>) {
        let read = relationship_name.borrow();
        self.relationship_name
            .borrow_mut()
            .insert(read.id, relationship_name.clone());
    }

    /// Exhume (get) [`RelationshipName`] from the store.
    ///
    pub fn exhume_relationship_name(&self, id: &Uuid) -> Option<Rc<RefCell<RelationshipName>>> {
        self.relationship_name
            .borrow()
            .get(id)
            .map(|relationship_name| relationship_name.clone())
    }

    /// Exorcise (remove) [`RelationshipName`] from the store.
    ///
    pub fn exorcise_relationship_name(
        &mut self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<RelationshipName>>> {
        self.relationship_name
            .borrow_mut()
            .remove(id)
            .map(|relationship_name| relationship_name.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, RelationshipName>`.
    ///
    pub fn iter_relationship_name(
        &self,
    ) -> impl Iterator<Item = Rc<RefCell<RelationshipName>>> + '_ {
        let values: Vec<Rc<RefCell<RelationshipName>>> = self
            .relationship_name
            .borrow()
            .values()
            .map(|relationship_name| relationship_name.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`RelationshipPhrase`] into the store.
    ///
    pub fn inter_relationship_phrase(
        &mut self,
        relationship_phrase: Rc<RefCell<RelationshipPhrase>>,
    ) {
        let read = relationship_phrase.borrow();
        self.relationship_phrase
            .borrow_mut()
            .insert(read.id, relationship_phrase.clone());
    }

    /// Exhume (get) [`RelationshipPhrase`] from the store.
    ///
    pub fn exhume_relationship_phrase(&self, id: &Uuid) -> Option<Rc<RefCell<RelationshipPhrase>>> {
        self.relationship_phrase
            .borrow()
            .get(id)
            .map(|relationship_phrase| relationship_phrase.clone())
    }

    /// Exorcise (remove) [`RelationshipPhrase`] from the store.
    ///
    pub fn exorcise_relationship_phrase(
        &mut self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<RelationshipPhrase>>> {
        self.relationship_phrase
            .borrow_mut()
            .remove(id)
            .map(|relationship_phrase| relationship_phrase.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, RelationshipPhrase>`.
    ///
    pub fn iter_relationship_phrase(
        &self,
    ) -> impl Iterator<Item = Rc<RefCell<RelationshipPhrase>>> + '_ {
        let values: Vec<Rc<RefCell<RelationshipPhrase>>> = self
            .relationship_phrase
            .borrow()
            .values()
            .map(|relationship_phrase| relationship_phrase.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"merlin-object-store-persistence"}}}
    /// Persist the store.
    ///
    /// The store is persisted as a a bincode file.
    pub fn persist_bincode<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let path = path.as_ref();
        let mut bin_file = fs::File::create(path)?;
        let encoded: Vec<u8> = bincode::serialize(&self).unwrap();
        bin_file.write_all(&encoded)?;
        Ok(())
    }

    /// Persist the store.
    ///
    /// The store is persisted as a directory of JSON files. The intention
    /// is that this directory can be checked into version control.
    /// In fact, I intend to add automagic git integration as an option.
    pub fn persist<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let path = path.as_ref();
        fs::create_dir_all(path)?;

        let path = path.join("merlin.json");
        fs::create_dir_all(&path)?;

        // Persist Anchor.
        {
            let path = path.join("anchor");
            fs::create_dir_all(&path)?;
            for anchor in self.anchor.borrow().values() {
                let path = path.join(format!("{}.json", anchor.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &anchor)?;
            }
        }

        // Persist Bisection.
        {
            let path = path.join("bisection");
            fs::create_dir_all(&path)?;
            for bisection in self.bisection.borrow().values() {
                let path = path.join(format!("{}.json", bisection.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &bisection)?;
            }
        }

        // Persist Box.
        {
            let path = path.join("x_box");
            fs::create_dir_all(&path)?;
            for x_box in self.x_box.borrow().values() {
                let path = path.join(format!("{}.json", x_box.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &x_box)?;
            }
        }

        // Persist Edge.
        {
            let path = path.join("edge");
            fs::create_dir_all(&path)?;
            for edge in self.edge.borrow().values() {
                let path = path.join(format!("{}.json", edge.borrow().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &edge)?;
            }
        }

        // Persist Glyph.
        {
            let path = path.join("glyph");
            fs::create_dir_all(&path)?;
            for glyph in self.glyph.borrow().values() {
                let path = path.join(format!("{}.json", glyph.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &glyph)?;
            }
        }

        // Persist Line.
        {
            let path = path.join("line");
            fs::create_dir_all(&path)?;
            for line in self.line.borrow().values() {
                let path = path.join(format!("{}.json", line.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &line)?;
            }
        }

        // Persist Line Segment.
        {
            let path = path.join("line_segment");
            fs::create_dir_all(&path)?;
            for line_segment in self.line_segment.borrow().values() {
                let path = path.join(format!("{}.json", line_segment.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &line_segment)?;
            }
        }

        // Persist Line Segment Point.
        {
            let path = path.join("line_segment_point");
            fs::create_dir_all(&path)?;
            for line_segment_point in self.line_segment_point.borrow().values() {
                let path = path.join(format!("{}.json", line_segment_point.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &line_segment_point)?;
            }
        }

        // Persist Point.
        {
            let path = path.join("point");
            fs::create_dir_all(&path)?;
            for point in self.point.borrow().values() {
                let path = path.join(format!("{}.json", point.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &point)?;
            }
        }

        // Persist Relationship Name.
        {
            let path = path.join("relationship_name");
            fs::create_dir_all(&path)?;
            for relationship_name in self.relationship_name.borrow().values() {
                let path = path.join(format!("{}.json", relationship_name.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &relationship_name)?;
            }
        }

        // Persist Relationship Phrase.
        {
            let path = path.join("relationship_phrase");
            fs::create_dir_all(&path)?;
            for relationship_phrase in self.relationship_phrase.borrow().values() {
                let path = path.join(format!("{}.json", relationship_phrase.borrow().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &relationship_phrase)?;
            }
        }

        Ok(())
    }

    /// Load the store.
    ///
    pub fn from_bincode(code: &[u8]) -> io::Result<Self> {
        Ok(bincode::deserialize(code).unwrap())
    }

    /// The store is as a bincode file.
    pub fn load_bincode<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let bin_file = fs::File::open(path)?;
        Ok(bincode::deserialize_from(bin_file).unwrap())
    }

    /// Load the store.
    ///
    /// The store is persisted as a directory of JSON files. The intention
    /// is that this directory can be checked into version control.
    /// In fact, I intend to add automagic git integration as an option.
    pub fn load<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let path = path.as_ref();
        let path = path.join("merlin.json");

        let store = Self::new();

        // Load Anchor.
        {
            let path = path.join("anchor");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let anchor: Rc<RefCell<Anchor>> = serde_json::from_reader(reader)?;
                store
                    .anchor
                    .borrow_mut()
                    .insert(anchor.borrow().id, anchor.clone());
            }
        }

        // Load Bisection.
        {
            let path = path.join("bisection");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let bisection: Rc<RefCell<Bisection>> = serde_json::from_reader(reader)?;
                store
                    .bisection
                    .borrow_mut()
                    .insert(bisection.borrow().id, bisection.clone());
            }
        }

        // Load Box.
        {
            let path = path.join("x_box");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let x_box: Rc<RefCell<XBox>> = serde_json::from_reader(reader)?;
                store
                    .x_box
                    .borrow_mut()
                    .insert(x_box.borrow().id, x_box.clone());
            }
        }

        // Load Edge.
        {
            let path = path.join("edge");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let edge: Rc<RefCell<Edge>> = serde_json::from_reader(reader)?;
                store
                    .edge
                    .borrow_mut()
                    .insert(edge.borrow().id(), edge.clone());
            }
        }

        // Load Glyph.
        {
            let path = path.join("glyph");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let glyph: Rc<RefCell<Glyph>> = serde_json::from_reader(reader)?;
                store
                    .glyph
                    .borrow_mut()
                    .insert(glyph.borrow().id, glyph.clone());
            }
        }

        // Load Line.
        {
            let path = path.join("line");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let line: Rc<RefCell<Line>> = serde_json::from_reader(reader)?;
                store
                    .line
                    .borrow_mut()
                    .insert(line.borrow().id, line.clone());
            }
        }

        // Load Line Segment.
        {
            let path = path.join("line_segment");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let line_segment: Rc<RefCell<LineSegment>> = serde_json::from_reader(reader)?;
                store
                    .line_segment
                    .borrow_mut()
                    .insert(line_segment.borrow().id, line_segment.clone());
            }
        }

        // Load Line Segment Point.
        {
            let path = path.join("line_segment_point");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let line_segment_point: Rc<RefCell<LineSegmentPoint>> =
                    serde_json::from_reader(reader)?;
                store
                    .line_segment_point
                    .borrow_mut()
                    .insert(line_segment_point.borrow().id, line_segment_point.clone());
            }
        }

        // Load Point.
        {
            let path = path.join("point");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let point: Rc<RefCell<Point>> = serde_json::from_reader(reader)?;
                store
                    .point
                    .borrow_mut()
                    .insert(point.borrow().id, point.clone());
            }
        }

        // Load Relationship Name.
        {
            let path = path.join("relationship_name");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let relationship_name: Rc<RefCell<RelationshipName>> =
                    serde_json::from_reader(reader)?;
                store
                    .relationship_name
                    .borrow_mut()
                    .insert(relationship_name.borrow().id, relationship_name.clone());
            }
        }

        // Load Relationship Phrase.
        {
            let path = path.join("relationship_phrase");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let relationship_phrase: Rc<RefCell<RelationshipPhrase>> =
                    serde_json::from_reader(reader)?;
                store
                    .relationship_phrase
                    .borrow_mut()
                    .insert(relationship_phrase.borrow().id, relationship_phrase.clone());
            }
        }

        Ok(store)
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
