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
use std::sync::Arc;
use std::sync::RwLock;
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
    anchor: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Anchor>>>>>,
    bisection: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Bisection>>>>>,
    x_box: Arc<RwLock<HashMap<Uuid, Arc<RwLock<XBox>>>>>,
    edge: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Edge>>>>>,
    glyph: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Glyph>>>>>,
    line: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Line>>>>>,
    line_segment: Arc<RwLock<HashMap<Uuid, Arc<RwLock<LineSegment>>>>>,
    line_segment_point: Arc<RwLock<HashMap<Uuid, Arc<RwLock<LineSegmentPoint>>>>>,
    point: Arc<RwLock<HashMap<Uuid, Arc<RwLock<Point>>>>>,
    relationship_name: Arc<RwLock<HashMap<Uuid, Arc<RwLock<RelationshipName>>>>>,
    relationship_phrase: Arc<RwLock<HashMap<Uuid, Arc<RwLock<RelationshipPhrase>>>>>,
}

impl ObjectStore {
    pub fn new() -> Self {
        let mut store = Self {
            anchor: Arc::new(RwLock::new(HashMap::default())),
            bisection: Arc::new(RwLock::new(HashMap::default())),
            x_box: Arc::new(RwLock::new(HashMap::default())),
            edge: Arc::new(RwLock::new(HashMap::default())),
            glyph: Arc::new(RwLock::new(HashMap::default())),
            line: Arc::new(RwLock::new(HashMap::default())),
            line_segment: Arc::new(RwLock::new(HashMap::default())),
            line_segment_point: Arc::new(RwLock::new(HashMap::default())),
            point: Arc::new(RwLock::new(HashMap::default())),
            relationship_name: Arc::new(RwLock::new(HashMap::default())),
            relationship_phrase: Arc::new(RwLock::new(HashMap::default())),
        };

        // Initialize Singleton Subtypes
        // 💥 Look at how beautiful this generated code is for super/sub-type graphs!
        // I remember having a bit of a struggle making it work. It's recursive, with
        // a lot of special cases, and I think it calls other recursive functions...💥
        store.inter_edge(Arc::new(RwLock::new(Edge::Bottom(BOTTOM))));
        store.inter_edge(Arc::new(RwLock::new(Edge::Left(LEFT))));
        store.inter_edge(Arc::new(RwLock::new(Edge::Right(RIGHT))));
        store.inter_edge(Arc::new(RwLock::new(Edge::Top(TOP))));

        store
    }

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"merlin-object-store-methods"}}}
    /// Inter (insert) [`Anchor`] into the store.
    ///
    pub fn inter_anchor(&mut self, anchor: Arc<RwLock<Anchor>>) {
        let read = anchor.read().unwrap();
        self.anchor.write().unwrap().insert(read.id, anchor.clone());
    }

    /// Exhume (get) [`Anchor`] from the store.
    ///
    pub fn exhume_anchor(&self, id: &Uuid) -> Option<Arc<RwLock<Anchor>>> {
        self.anchor
            .read()
            .unwrap()
            .get(id)
            .map(|anchor| anchor.clone())
    }

    /// Exorcise (remove) [`Anchor`] from the store.
    ///
    pub fn exorcise_anchor(&mut self, id: &Uuid) -> Option<Arc<RwLock<Anchor>>> {
        self.anchor
            .write()
            .unwrap()
            .remove(id)
            .map(|anchor| anchor.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Anchor>`.
    ///
    pub fn iter_anchor(&self) -> impl Iterator<Item = Arc<RwLock<Anchor>>> + '_ {
        let values: Vec<Arc<RwLock<Anchor>>> = self
            .anchor
            .read()
            .unwrap()
            .values()
            .map(|anchor| anchor.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Bisection`] into the store.
    ///
    pub fn inter_bisection(&mut self, bisection: Arc<RwLock<Bisection>>) {
        let read = bisection.read().unwrap();
        self.bisection
            .write()
            .unwrap()
            .insert(read.id, bisection.clone());
    }

    /// Exhume (get) [`Bisection`] from the store.
    ///
    pub fn exhume_bisection(&self, id: &Uuid) -> Option<Arc<RwLock<Bisection>>> {
        self.bisection
            .read()
            .unwrap()
            .get(id)
            .map(|bisection| bisection.clone())
    }

    /// Exorcise (remove) [`Bisection`] from the store.
    ///
    pub fn exorcise_bisection(&mut self, id: &Uuid) -> Option<Arc<RwLock<Bisection>>> {
        self.bisection
            .write()
            .unwrap()
            .remove(id)
            .map(|bisection| bisection.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Bisection>`.
    ///
    pub fn iter_bisection(&self) -> impl Iterator<Item = Arc<RwLock<Bisection>>> + '_ {
        let values: Vec<Arc<RwLock<Bisection>>> = self
            .bisection
            .read()
            .unwrap()
            .values()
            .map(|bisection| bisection.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`XBox`] into the store.
    ///
    pub fn inter_x_box(&mut self, x_box: Arc<RwLock<XBox>>) {
        let read = x_box.read().unwrap();
        self.x_box.write().unwrap().insert(read.id, x_box.clone());
    }

    /// Exhume (get) [`XBox`] from the store.
    ///
    pub fn exhume_x_box(&self, id: &Uuid) -> Option<Arc<RwLock<XBox>>> {
        self.x_box
            .read()
            .unwrap()
            .get(id)
            .map(|x_box| x_box.clone())
    }

    /// Exorcise (remove) [`XBox`] from the store.
    ///
    pub fn exorcise_x_box(&mut self, id: &Uuid) -> Option<Arc<RwLock<XBox>>> {
        self.x_box
            .write()
            .unwrap()
            .remove(id)
            .map(|x_box| x_box.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, XBox>`.
    ///
    pub fn iter_x_box(&self) -> impl Iterator<Item = Arc<RwLock<XBox>>> + '_ {
        let values: Vec<Arc<RwLock<XBox>>> = self
            .x_box
            .read()
            .unwrap()
            .values()
            .map(|x_box| x_box.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Edge`] into the store.
    ///
    pub fn inter_edge(&mut self, edge: Arc<RwLock<Edge>>) {
        let read = edge.read().unwrap();
        self.edge.write().unwrap().insert(read.id(), edge.clone());
    }

    /// Exhume (get) [`Edge`] from the store.
    ///
    pub fn exhume_edge(&self, id: &Uuid) -> Option<Arc<RwLock<Edge>>> {
        self.edge.read().unwrap().get(id).map(|edge| edge.clone())
    }

    /// Exorcise (remove) [`Edge`] from the store.
    ///
    pub fn exorcise_edge(&mut self, id: &Uuid) -> Option<Arc<RwLock<Edge>>> {
        self.edge
            .write()
            .unwrap()
            .remove(id)
            .map(|edge| edge.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Edge>`.
    ///
    pub fn iter_edge(&self) -> impl Iterator<Item = Arc<RwLock<Edge>>> + '_ {
        let values: Vec<Arc<RwLock<Edge>>> = self
            .edge
            .read()
            .unwrap()
            .values()
            .map(|edge| edge.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Glyph`] into the store.
    ///
    pub fn inter_glyph(&mut self, glyph: Arc<RwLock<Glyph>>) {
        let read = glyph.read().unwrap();
        self.glyph.write().unwrap().insert(read.id, glyph.clone());
    }

    /// Exhume (get) [`Glyph`] from the store.
    ///
    pub fn exhume_glyph(&self, id: &Uuid) -> Option<Arc<RwLock<Glyph>>> {
        self.glyph
            .read()
            .unwrap()
            .get(id)
            .map(|glyph| glyph.clone())
    }

    /// Exorcise (remove) [`Glyph`] from the store.
    ///
    pub fn exorcise_glyph(&mut self, id: &Uuid) -> Option<Arc<RwLock<Glyph>>> {
        self.glyph
            .write()
            .unwrap()
            .remove(id)
            .map(|glyph| glyph.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Glyph>`.
    ///
    pub fn iter_glyph(&self) -> impl Iterator<Item = Arc<RwLock<Glyph>>> + '_ {
        let values: Vec<Arc<RwLock<Glyph>>> = self
            .glyph
            .read()
            .unwrap()
            .values()
            .map(|glyph| glyph.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Line`] into the store.
    ///
    pub fn inter_line(&mut self, line: Arc<RwLock<Line>>) {
        let read = line.read().unwrap();
        self.line.write().unwrap().insert(read.id, line.clone());
    }

    /// Exhume (get) [`Line`] from the store.
    ///
    pub fn exhume_line(&self, id: &Uuid) -> Option<Arc<RwLock<Line>>> {
        self.line.read().unwrap().get(id).map(|line| line.clone())
    }

    /// Exorcise (remove) [`Line`] from the store.
    ///
    pub fn exorcise_line(&mut self, id: &Uuid) -> Option<Arc<RwLock<Line>>> {
        self.line
            .write()
            .unwrap()
            .remove(id)
            .map(|line| line.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Line>`.
    ///
    pub fn iter_line(&self) -> impl Iterator<Item = Arc<RwLock<Line>>> + '_ {
        let values: Vec<Arc<RwLock<Line>>> = self
            .line
            .read()
            .unwrap()
            .values()
            .map(|line| line.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`LineSegment`] into the store.
    ///
    pub fn inter_line_segment(&mut self, line_segment: Arc<RwLock<LineSegment>>) {
        let read = line_segment.read().unwrap();
        self.line_segment
            .write()
            .unwrap()
            .insert(read.id, line_segment.clone());
    }

    /// Exhume (get) [`LineSegment`] from the store.
    ///
    pub fn exhume_line_segment(&self, id: &Uuid) -> Option<Arc<RwLock<LineSegment>>> {
        self.line_segment
            .read()
            .unwrap()
            .get(id)
            .map(|line_segment| line_segment.clone())
    }

    /// Exorcise (remove) [`LineSegment`] from the store.
    ///
    pub fn exorcise_line_segment(&mut self, id: &Uuid) -> Option<Arc<RwLock<LineSegment>>> {
        self.line_segment
            .write()
            .unwrap()
            .remove(id)
            .map(|line_segment| line_segment.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, LineSegment>`.
    ///
    pub fn iter_line_segment(&self) -> impl Iterator<Item = Arc<RwLock<LineSegment>>> + '_ {
        let values: Vec<Arc<RwLock<LineSegment>>> = self
            .line_segment
            .read()
            .unwrap()
            .values()
            .map(|line_segment| line_segment.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`LineSegmentPoint`] into the store.
    ///
    pub fn inter_line_segment_point(&mut self, line_segment_point: Arc<RwLock<LineSegmentPoint>>) {
        let read = line_segment_point.read().unwrap();
        self.line_segment_point
            .write()
            .unwrap()
            .insert(read.id, line_segment_point.clone());
    }

    /// Exhume (get) [`LineSegmentPoint`] from the store.
    ///
    pub fn exhume_line_segment_point(&self, id: &Uuid) -> Option<Arc<RwLock<LineSegmentPoint>>> {
        self.line_segment_point
            .read()
            .unwrap()
            .get(id)
            .map(|line_segment_point| line_segment_point.clone())
    }

    /// Exorcise (remove) [`LineSegmentPoint`] from the store.
    ///
    pub fn exorcise_line_segment_point(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<LineSegmentPoint>>> {
        self.line_segment_point
            .write()
            .unwrap()
            .remove(id)
            .map(|line_segment_point| line_segment_point.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, LineSegmentPoint>`.
    ///
    pub fn iter_line_segment_point(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<LineSegmentPoint>>> + '_ {
        let values: Vec<Arc<RwLock<LineSegmentPoint>>> = self
            .line_segment_point
            .read()
            .unwrap()
            .values()
            .map(|line_segment_point| line_segment_point.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`Point`] into the store.
    ///
    pub fn inter_point(&mut self, point: Arc<RwLock<Point>>) {
        let read = point.read().unwrap();
        self.point.write().unwrap().insert(read.id, point.clone());
    }

    /// Exhume (get) [`Point`] from the store.
    ///
    pub fn exhume_point(&self, id: &Uuid) -> Option<Arc<RwLock<Point>>> {
        self.point
            .read()
            .unwrap()
            .get(id)
            .map(|point| point.clone())
    }

    /// Exorcise (remove) [`Point`] from the store.
    ///
    pub fn exorcise_point(&mut self, id: &Uuid) -> Option<Arc<RwLock<Point>>> {
        self.point
            .write()
            .unwrap()
            .remove(id)
            .map(|point| point.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Point>`.
    ///
    pub fn iter_point(&self) -> impl Iterator<Item = Arc<RwLock<Point>>> + '_ {
        let values: Vec<Arc<RwLock<Point>>> = self
            .point
            .read()
            .unwrap()
            .values()
            .map(|point| point.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Inter (insert) [`RelationshipName`] into the store.
    ///
    pub fn inter_relationship_name(&mut self, relationship_name: Arc<RwLock<RelationshipName>>) {
        let read = relationship_name.read().unwrap();
        self.relationship_name
            .write()
            .unwrap()
            .insert(read.id, relationship_name.clone());
    }

    /// Exhume (get) [`RelationshipName`] from the store.
    ///
    pub fn exhume_relationship_name(&self, id: &Uuid) -> Option<Arc<RwLock<RelationshipName>>> {
        self.relationship_name
            .read()
            .unwrap()
            .get(id)
            .map(|relationship_name| relationship_name.clone())
    }

    /// Exorcise (remove) [`RelationshipName`] from the store.
    ///
    pub fn exorcise_relationship_name(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<RelationshipName>>> {
        self.relationship_name
            .write()
            .unwrap()
            .remove(id)
            .map(|relationship_name| relationship_name.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, RelationshipName>`.
    ///
    pub fn iter_relationship_name(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<RelationshipName>>> + '_ {
        let values: Vec<Arc<RwLock<RelationshipName>>> = self
            .relationship_name
            .read()
            .unwrap()
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
        relationship_phrase: Arc<RwLock<RelationshipPhrase>>,
    ) {
        let read = relationship_phrase.read().unwrap();
        self.relationship_phrase
            .write()
            .unwrap()
            .insert(read.id, relationship_phrase.clone());
    }

    /// Exhume (get) [`RelationshipPhrase`] from the store.
    ///
    pub fn exhume_relationship_phrase(&self, id: &Uuid) -> Option<Arc<RwLock<RelationshipPhrase>>> {
        self.relationship_phrase
            .read()
            .unwrap()
            .get(id)
            .map(|relationship_phrase| relationship_phrase.clone())
    }

    /// Exorcise (remove) [`RelationshipPhrase`] from the store.
    ///
    pub fn exorcise_relationship_phrase(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<RelationshipPhrase>>> {
        self.relationship_phrase
            .write()
            .unwrap()
            .remove(id)
            .map(|relationship_phrase| relationship_phrase.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, RelationshipPhrase>`.
    ///
    pub fn iter_relationship_phrase(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<RelationshipPhrase>>> + '_ {
        let values: Vec<Arc<RwLock<RelationshipPhrase>>> = self
            .relationship_phrase
            .read()
            .unwrap()
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
            for anchor in self.anchor.read().unwrap().values() {
                let path = path.join(format!("{}.json", anchor.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &anchor)?;
            }
        }

        // Persist Bisection.
        {
            let path = path.join("bisection");
            fs::create_dir_all(&path)?;
            for bisection in self.bisection.read().unwrap().values() {
                let path = path.join(format!("{}.json", bisection.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &bisection)?;
            }
        }

        // Persist Box.
        {
            let path = path.join("x_box");
            fs::create_dir_all(&path)?;
            for x_box in self.x_box.read().unwrap().values() {
                let path = path.join(format!("{}.json", x_box.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &x_box)?;
            }
        }

        // Persist Edge.
        {
            let path = path.join("edge");
            fs::create_dir_all(&path)?;
            for edge in self.edge.read().unwrap().values() {
                let path = path.join(format!("{}.json", edge.read().unwrap().id()));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &edge)?;
            }
        }

        // Persist Glyph.
        {
            let path = path.join("glyph");
            fs::create_dir_all(&path)?;
            for glyph in self.glyph.read().unwrap().values() {
                let path = path.join(format!("{}.json", glyph.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &glyph)?;
            }
        }

        // Persist Line.
        {
            let path = path.join("line");
            fs::create_dir_all(&path)?;
            for line in self.line.read().unwrap().values() {
                let path = path.join(format!("{}.json", line.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &line)?;
            }
        }

        // Persist Line Segment.
        {
            let path = path.join("line_segment");
            fs::create_dir_all(&path)?;
            for line_segment in self.line_segment.read().unwrap().values() {
                let path = path.join(format!("{}.json", line_segment.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &line_segment)?;
            }
        }

        // Persist Line Segment Point.
        {
            let path = path.join("line_segment_point");
            fs::create_dir_all(&path)?;
            for line_segment_point in self.line_segment_point.read().unwrap().values() {
                let path = path.join(format!("{}.json", line_segment_point.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &line_segment_point)?;
            }
        }

        // Persist Point.
        {
            let path = path.join("point");
            fs::create_dir_all(&path)?;
            for point in self.point.read().unwrap().values() {
                let path = path.join(format!("{}.json", point.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &point)?;
            }
        }

        // Persist Relationship Name.
        {
            let path = path.join("relationship_name");
            fs::create_dir_all(&path)?;
            for relationship_name in self.relationship_name.read().unwrap().values() {
                let path = path.join(format!("{}.json", relationship_name.read().unwrap().id));
                let file = fs::File::create(path)?;
                let mut writer = io::BufWriter::new(file);
                serde_json::to_writer_pretty(&mut writer, &relationship_name)?;
            }
        }

        // Persist Relationship Phrase.
        {
            let path = path.join("relationship_phrase");
            fs::create_dir_all(&path)?;
            for relationship_phrase in self.relationship_phrase.read().unwrap().values() {
                let path = path.join(format!("{}.json", relationship_phrase.read().unwrap().id));
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
                let anchor: Arc<RwLock<Anchor>> = serde_json::from_reader(reader)?;
                store
                    .anchor
                    .write()
                    .unwrap()
                    .insert(anchor.read().unwrap().id, anchor.clone());
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
                let bisection: Arc<RwLock<Bisection>> = serde_json::from_reader(reader)?;
                store
                    .bisection
                    .write()
                    .unwrap()
                    .insert(bisection.read().unwrap().id, bisection.clone());
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
                let x_box: Arc<RwLock<XBox>> = serde_json::from_reader(reader)?;
                store
                    .x_box
                    .write()
                    .unwrap()
                    .insert(x_box.read().unwrap().id, x_box.clone());
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
                let edge: Arc<RwLock<Edge>> = serde_json::from_reader(reader)?;
                store
                    .edge
                    .write()
                    .unwrap()
                    .insert(edge.read().unwrap().id(), edge.clone());
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
                let glyph: Arc<RwLock<Glyph>> = serde_json::from_reader(reader)?;
                store
                    .glyph
                    .write()
                    .unwrap()
                    .insert(glyph.read().unwrap().id, glyph.clone());
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
                let line: Arc<RwLock<Line>> = serde_json::from_reader(reader)?;
                store
                    .line
                    .write()
                    .unwrap()
                    .insert(line.read().unwrap().id, line.clone());
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
                let line_segment: Arc<RwLock<LineSegment>> = serde_json::from_reader(reader)?;
                store
                    .line_segment
                    .write()
                    .unwrap()
                    .insert(line_segment.read().unwrap().id, line_segment.clone());
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
                let line_segment_point: Arc<RwLock<LineSegmentPoint>> =
                    serde_json::from_reader(reader)?;
                store.line_segment_point.write().unwrap().insert(
                    line_segment_point.read().unwrap().id,
                    line_segment_point.clone(),
                );
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
                let point: Arc<RwLock<Point>> = serde_json::from_reader(reader)?;
                store
                    .point
                    .write()
                    .unwrap()
                    .insert(point.read().unwrap().id, point.clone());
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
                let relationship_name: Arc<RwLock<RelationshipName>> =
                    serde_json::from_reader(reader)?;
                store.relationship_name.write().unwrap().insert(
                    relationship_name.read().unwrap().id,
                    relationship_name.clone(),
                );
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
                let relationship_phrase: Arc<RwLock<RelationshipPhrase>> =
                    serde_json::from_reader(reader)?;
                store.relationship_phrase.write().unwrap().insert(
                    relationship_phrase.read().unwrap().id,
                    relationship_phrase.clone(),
                );
            }
        }

        Ok(store)
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
