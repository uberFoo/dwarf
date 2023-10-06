//! sarzak Object Store
//!
//! The ObjectStore contains instances of objects in the domain.
//! The instances are stored in a hash map, keyed by the object's UUID.
//! This is used during code generation, and probably not useful elsewhere.
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"sarzak-object-store-file"}}}
//!
//! # Contents:
//!
//! * [`AcknowledgedEvent`]
//! * [`AnAssociativeReferent`]
//! * [`Associative`]
//! * [`AssociativeReferent`]
//! * [`AssociativeReferrer`]
//! * [`Attribute`]
//! * [`Binary`]
//! * [`Cardinality`]
//! * [`Conditionality`]
//! * [`Event`]
//! * [`External`]
//! * [`Isa`]
//! * [`Object`]
//! * [`Referent`]
//! * [`Referrer`]
//! * [`Relationship`]
//! * [`State`]
//! * [`Subtype`]
//! * [`Supertype`]
//! * [`Ty`]
// {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"sarzak-object-store-definition"}}}
use std::sync::Arc;
use std::sync::RwLock;
use std::{
    fs,
    io::{self, prelude::*},
    path::Path,
    time::SystemTime,
};

use heck::ToUpperCamelCase;
use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::sarzak::types::{
    AcknowledgedEvent, AnAssociativeReferent, Associative, AssociativeReferent,
    AssociativeReferrer, Attribute, Binary, Cardinality, Conditionality, Event, External, Isa,
    Object, Referent, Referrer, Relationship, State, Subtype, Supertype, Ty, BOOLEAN, CONDITIONAL,
    FLOAT, INTEGER, MANY, ONE, S_STRING, S_UUID, UNCONDITIONAL,
};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ObjectStore {
    acknowledged_event: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<AcknowledgedEvent>>, SystemTime)>>>,
    an_associative_referent:
        Arc<RwLock<HashMap<Uuid, (Arc<RwLock<AnAssociativeReferent>>, SystemTime)>>>,
    associative: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Associative>>, SystemTime)>>>,
    associative_referent:
        Arc<RwLock<HashMap<Uuid, (Arc<RwLock<AssociativeReferent>>, SystemTime)>>>,
    associative_referrer:
        Arc<RwLock<HashMap<Uuid, (Arc<RwLock<AssociativeReferrer>>, SystemTime)>>>,
    attribute: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Attribute>>, SystemTime)>>>,
    binary: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Binary>>, SystemTime)>>>,
    cardinality: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Cardinality>>, SystemTime)>>>,
    conditionality: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Conditionality>>, SystemTime)>>>,
    event: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Event>>, SystemTime)>>>,
    external: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<External>>, SystemTime)>>>,
    isa: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Isa>>, SystemTime)>>>,
    object: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Object>>, SystemTime)>>>,
    object_id_by_name: Arc<RwLock<HashMap<String, (Uuid, SystemTime)>>>,
    referent: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Referent>>, SystemTime)>>>,
    referrer: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Referrer>>, SystemTime)>>>,
    relationship: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Relationship>>, SystemTime)>>>,
    state: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<State>>, SystemTime)>>>,
    subtype: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Subtype>>, SystemTime)>>>,
    supertype: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Supertype>>, SystemTime)>>>,
    ty: Arc<RwLock<HashMap<Uuid, (Arc<RwLock<Ty>>, SystemTime)>>>,
}

impl ObjectStore {
    pub fn new() -> Self {
        let mut store = Self {
            acknowledged_event: Arc::new(RwLock::new(HashMap::default())),
            an_associative_referent: Arc::new(RwLock::new(HashMap::default())),
            associative: Arc::new(RwLock::new(HashMap::default())),
            associative_referent: Arc::new(RwLock::new(HashMap::default())),
            associative_referrer: Arc::new(RwLock::new(HashMap::default())),
            attribute: Arc::new(RwLock::new(HashMap::default())),
            binary: Arc::new(RwLock::new(HashMap::default())),
            cardinality: Arc::new(RwLock::new(HashMap::default())),
            conditionality: Arc::new(RwLock::new(HashMap::default())),
            event: Arc::new(RwLock::new(HashMap::default())),
            external: Arc::new(RwLock::new(HashMap::default())),
            isa: Arc::new(RwLock::new(HashMap::default())),
            object: Arc::new(RwLock::new(HashMap::default())),
            object_id_by_name: Arc::new(RwLock::new(HashMap::default())),
            referent: Arc::new(RwLock::new(HashMap::default())),
            referrer: Arc::new(RwLock::new(HashMap::default())),
            relationship: Arc::new(RwLock::new(HashMap::default())),
            state: Arc::new(RwLock::new(HashMap::default())),
            subtype: Arc::new(RwLock::new(HashMap::default())),
            supertype: Arc::new(RwLock::new(HashMap::default())),
            ty: Arc::new(RwLock::new(HashMap::default())),
        };

        // Initialize Singleton Subtypes
        // 💥 Look at how beautiful this generated code is for super/sub-type graphs!
        // I remember having a bit of a struggle making it work. It's recursive, with
        // a lot of special cases, and I think it calls other recursive functions...💥
        store.inter_cardinality(Arc::new(RwLock::new(Cardinality::Many(MANY))));
        store.inter_cardinality(Arc::new(RwLock::new(Cardinality::One(ONE))));
        store.inter_conditionality(Arc::new(RwLock::new(Conditionality::Conditional(
            CONDITIONAL,
        ))));
        store.inter_conditionality(Arc::new(RwLock::new(Conditionality::Unconditional(
            UNCONDITIONAL,
        ))));
        store.inter_ty(Arc::new(RwLock::new(Ty::Boolean(BOOLEAN))));
        store.inter_ty(Arc::new(RwLock::new(Ty::Float(FLOAT))));
        store.inter_ty(Arc::new(RwLock::new(Ty::Integer(INTEGER))));
        store.inter_ty(Arc::new(RwLock::new(Ty::SString(S_STRING))));
        store.inter_ty(Arc::new(RwLock::new(Ty::SUuid(S_UUID))));

        store
    }

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"sarzak-object-store-methods"}}}
    /// Inter (insert) [`AcknowledgedEvent`] into the store.
    ///
    pub fn inter_acknowledged_event(&mut self, acknowledged_event: Arc<RwLock<AcknowledgedEvent>>) {
        let read = acknowledged_event.read().unwrap();
        self.acknowledged_event
            .write()
            .unwrap()
            .insert(read.id, (acknowledged_event.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`AcknowledgedEvent`] from the store.
    ///
    pub fn exhume_acknowledged_event(&self, id: &Uuid) -> Option<Arc<RwLock<AcknowledgedEvent>>> {
        self.acknowledged_event
            .read()
            .unwrap()
            .get(id)
            .map(|acknowledged_event| acknowledged_event.0.clone())
    }

    /// Exorcise (remove) [`AcknowledgedEvent`] from the store.
    ///
    pub fn exorcise_acknowledged_event(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<AcknowledgedEvent>>> {
        self.acknowledged_event
            .write()
            .unwrap()
            .remove(id)
            .map(|acknowledged_event| acknowledged_event.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AcknowledgedEvent>`.
    ///
    pub fn iter_acknowledged_event(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<AcknowledgedEvent>>> + '_ {
        let values: Vec<Arc<RwLock<AcknowledgedEvent>>> = self
            .acknowledged_event
            .read()
            .unwrap()
            .values()
            .map(|acknowledged_event| acknowledged_event.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for AcknowledgedEvent.
    ///
    pub fn acknowledged_event_timestamp(
        &self,
        acknowledged_event: &AcknowledgedEvent,
    ) -> SystemTime {
        self.acknowledged_event
            .read()
            .unwrap()
            .get(&acknowledged_event.id)
            .map(|acknowledged_event| acknowledged_event.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`AnAssociativeReferent`] into the store.
    ///
    pub fn inter_an_associative_referent(
        &mut self,
        an_associative_referent: Arc<RwLock<AnAssociativeReferent>>,
    ) {
        let read = an_associative_referent.read().unwrap();
        self.an_associative_referent.write().unwrap().insert(
            read.id,
            (an_associative_referent.clone(), SystemTime::now()),
        );
    }

    /// Exhume (get) [`AnAssociativeReferent`] from the store.
    ///
    pub fn exhume_an_associative_referent(
        &self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<AnAssociativeReferent>>> {
        self.an_associative_referent
            .read()
            .unwrap()
            .get(id)
            .map(|an_associative_referent| an_associative_referent.0.clone())
    }

    /// Exorcise (remove) [`AnAssociativeReferent`] from the store.
    ///
    pub fn exorcise_an_associative_referent(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<AnAssociativeReferent>>> {
        self.an_associative_referent
            .write()
            .unwrap()
            .remove(id)
            .map(|an_associative_referent| an_associative_referent.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AnAssociativeReferent>`.
    ///
    pub fn iter_an_associative_referent(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<AnAssociativeReferent>>> + '_ {
        let values: Vec<Arc<RwLock<AnAssociativeReferent>>> = self
            .an_associative_referent
            .read()
            .unwrap()
            .values()
            .map(|an_associative_referent| an_associative_referent.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for AnAssociativeReferent.
    ///
    pub fn an_associative_referent_timestamp(
        &self,
        an_associative_referent: &AnAssociativeReferent,
    ) -> SystemTime {
        self.an_associative_referent
            .read()
            .unwrap()
            .get(&an_associative_referent.id)
            .map(|an_associative_referent| an_associative_referent.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Associative`] into the store.
    ///
    pub fn inter_associative(&mut self, associative: Arc<RwLock<Associative>>) {
        let read = associative.read().unwrap();
        self.associative
            .write()
            .unwrap()
            .insert(read.id, (associative.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Associative`] from the store.
    ///
    pub fn exhume_associative(&self, id: &Uuid) -> Option<Arc<RwLock<Associative>>> {
        self.associative
            .read()
            .unwrap()
            .get(id)
            .map(|associative| associative.0.clone())
    }

    /// Exorcise (remove) [`Associative`] from the store.
    ///
    pub fn exorcise_associative(&mut self, id: &Uuid) -> Option<Arc<RwLock<Associative>>> {
        self.associative
            .write()
            .unwrap()
            .remove(id)
            .map(|associative| associative.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Associative>`.
    ///
    pub fn iter_associative(&self) -> impl Iterator<Item = Arc<RwLock<Associative>>> + '_ {
        let values: Vec<Arc<RwLock<Associative>>> = self
            .associative
            .read()
            .unwrap()
            .values()
            .map(|associative| associative.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Associative.
    ///
    pub fn associative_timestamp(&self, associative: &Associative) -> SystemTime {
        self.associative
            .read()
            .unwrap()
            .get(&associative.id)
            .map(|associative| associative.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`AssociativeReferent`] into the store.
    ///
    pub fn inter_associative_referent(
        &mut self,
        associative_referent: Arc<RwLock<AssociativeReferent>>,
    ) {
        let read = associative_referent.read().unwrap();
        self.associative_referent
            .write()
            .unwrap()
            .insert(read.id, (associative_referent.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`AssociativeReferent`] from the store.
    ///
    pub fn exhume_associative_referent(
        &self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<AssociativeReferent>>> {
        self.associative_referent
            .read()
            .unwrap()
            .get(id)
            .map(|associative_referent| associative_referent.0.clone())
    }

    /// Exorcise (remove) [`AssociativeReferent`] from the store.
    ///
    pub fn exorcise_associative_referent(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<AssociativeReferent>>> {
        self.associative_referent
            .write()
            .unwrap()
            .remove(id)
            .map(|associative_referent| associative_referent.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AssociativeReferent>`.
    ///
    pub fn iter_associative_referent(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<AssociativeReferent>>> + '_ {
        let values: Vec<Arc<RwLock<AssociativeReferent>>> = self
            .associative_referent
            .read()
            .unwrap()
            .values()
            .map(|associative_referent| associative_referent.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for AssociativeReferent.
    ///
    pub fn associative_referent_timestamp(
        &self,
        associative_referent: &AssociativeReferent,
    ) -> SystemTime {
        self.associative_referent
            .read()
            .unwrap()
            .get(&associative_referent.id)
            .map(|associative_referent| associative_referent.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`AssociativeReferrer`] into the store.
    ///
    pub fn inter_associative_referrer(
        &mut self,
        associative_referrer: Arc<RwLock<AssociativeReferrer>>,
    ) {
        let read = associative_referrer.read().unwrap();
        self.associative_referrer
            .write()
            .unwrap()
            .insert(read.id, (associative_referrer.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`AssociativeReferrer`] from the store.
    ///
    pub fn exhume_associative_referrer(
        &self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<AssociativeReferrer>>> {
        self.associative_referrer
            .read()
            .unwrap()
            .get(id)
            .map(|associative_referrer| associative_referrer.0.clone())
    }

    /// Exorcise (remove) [`AssociativeReferrer`] from the store.
    ///
    pub fn exorcise_associative_referrer(
        &mut self,
        id: &Uuid,
    ) -> Option<Arc<RwLock<AssociativeReferrer>>> {
        self.associative_referrer
            .write()
            .unwrap()
            .remove(id)
            .map(|associative_referrer| associative_referrer.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AssociativeReferrer>`.
    ///
    pub fn iter_associative_referrer(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<AssociativeReferrer>>> + '_ {
        let values: Vec<Arc<RwLock<AssociativeReferrer>>> = self
            .associative_referrer
            .read()
            .unwrap()
            .values()
            .map(|associative_referrer| associative_referrer.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for AssociativeReferrer.
    ///
    pub fn associative_referrer_timestamp(
        &self,
        associative_referrer: &AssociativeReferrer,
    ) -> SystemTime {
        self.associative_referrer
            .read()
            .unwrap()
            .get(&associative_referrer.id)
            .map(|associative_referrer| associative_referrer.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Attribute`] into the store.
    ///
    pub fn inter_attribute(&mut self, attribute: Arc<RwLock<Attribute>>) {
        let read = attribute.read().unwrap();
        self.attribute
            .write()
            .unwrap()
            .insert(read.id, (attribute.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Attribute`] from the store.
    ///
    pub fn exhume_attribute(&self, id: &Uuid) -> Option<Arc<RwLock<Attribute>>> {
        self.attribute
            .read()
            .unwrap()
            .get(id)
            .map(|attribute| attribute.0.clone())
    }

    /// Exorcise (remove) [`Attribute`] from the store.
    ///
    pub fn exorcise_attribute(&mut self, id: &Uuid) -> Option<Arc<RwLock<Attribute>>> {
        self.attribute
            .write()
            .unwrap()
            .remove(id)
            .map(|attribute| attribute.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Attribute>`.
    ///
    pub fn iter_attribute(&self) -> impl Iterator<Item = Arc<RwLock<Attribute>>> + '_ {
        let values: Vec<Arc<RwLock<Attribute>>> = self
            .attribute
            .read()
            .unwrap()
            .values()
            .map(|attribute| attribute.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Attribute.
    ///
    pub fn attribute_timestamp(&self, attribute: &Attribute) -> SystemTime {
        self.attribute
            .read()
            .unwrap()
            .get(&attribute.id)
            .map(|attribute| attribute.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Binary`] into the store.
    ///
    pub fn inter_binary(&mut self, binary: Arc<RwLock<Binary>>) {
        let read = binary.read().unwrap();
        self.binary
            .write()
            .unwrap()
            .insert(read.id, (binary.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Binary`] from the store.
    ///
    pub fn exhume_binary(&self, id: &Uuid) -> Option<Arc<RwLock<Binary>>> {
        self.binary
            .read()
            .unwrap()
            .get(id)
            .map(|binary| binary.0.clone())
    }

    /// Exorcise (remove) [`Binary`] from the store.
    ///
    pub fn exorcise_binary(&mut self, id: &Uuid) -> Option<Arc<RwLock<Binary>>> {
        self.binary
            .write()
            .unwrap()
            .remove(id)
            .map(|binary| binary.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Binary>`.
    ///
    pub fn iter_binary(&self) -> impl Iterator<Item = Arc<RwLock<Binary>>> + '_ {
        let values: Vec<Arc<RwLock<Binary>>> = self
            .binary
            .read()
            .unwrap()
            .values()
            .map(|binary| binary.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Binary.
    ///
    pub fn binary_timestamp(&self, binary: &Binary) -> SystemTime {
        self.binary
            .read()
            .unwrap()
            .get(&binary.id)
            .map(|binary| binary.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Cardinality`] into the store.
    ///
    pub fn inter_cardinality(&mut self, cardinality: Arc<RwLock<Cardinality>>) {
        let read = cardinality.read().unwrap();
        self.cardinality
            .write()
            .unwrap()
            .insert(read.id(), (cardinality.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Cardinality`] from the store.
    ///
    pub fn exhume_cardinality(&self, id: &Uuid) -> Option<Arc<RwLock<Cardinality>>> {
        self.cardinality
            .read()
            .unwrap()
            .get(id)
            .map(|cardinality| cardinality.0.clone())
    }

    /// Exorcise (remove) [`Cardinality`] from the store.
    ///
    pub fn exorcise_cardinality(&mut self, id: &Uuid) -> Option<Arc<RwLock<Cardinality>>> {
        self.cardinality
            .write()
            .unwrap()
            .remove(id)
            .map(|cardinality| cardinality.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Cardinality>`.
    ///
    pub fn iter_cardinality(&self) -> impl Iterator<Item = Arc<RwLock<Cardinality>>> + '_ {
        let values: Vec<Arc<RwLock<Cardinality>>> = self
            .cardinality
            .read()
            .unwrap()
            .values()
            .map(|cardinality| cardinality.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Cardinality.
    ///
    pub fn cardinality_timestamp(&self, cardinality: &Cardinality) -> SystemTime {
        self.cardinality
            .read()
            .unwrap()
            .get(&cardinality.id())
            .map(|cardinality| cardinality.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Conditionality`] into the store.
    ///
    pub fn inter_conditionality(&mut self, conditionality: Arc<RwLock<Conditionality>>) {
        let read = conditionality.read().unwrap();
        self.conditionality
            .write()
            .unwrap()
            .insert(read.id(), (conditionality.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Conditionality`] from the store.
    ///
    pub fn exhume_conditionality(&self, id: &Uuid) -> Option<Arc<RwLock<Conditionality>>> {
        self.conditionality
            .read()
            .unwrap()
            .get(id)
            .map(|conditionality| conditionality.0.clone())
    }

    /// Exorcise (remove) [`Conditionality`] from the store.
    ///
    pub fn exorcise_conditionality(&mut self, id: &Uuid) -> Option<Arc<RwLock<Conditionality>>> {
        self.conditionality
            .write()
            .unwrap()
            .remove(id)
            .map(|conditionality| conditionality.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Conditionality>`.
    ///
    pub fn iter_conditionality(&self) -> impl Iterator<Item = Arc<RwLock<Conditionality>>> + '_ {
        let values: Vec<Arc<RwLock<Conditionality>>> = self
            .conditionality
            .read()
            .unwrap()
            .values()
            .map(|conditionality| conditionality.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Conditionality.
    ///
    pub fn conditionality_timestamp(&self, conditionality: &Conditionality) -> SystemTime {
        self.conditionality
            .read()
            .unwrap()
            .get(&conditionality.id())
            .map(|conditionality| conditionality.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Event`] into the store.
    ///
    pub fn inter_event(&mut self, event: Arc<RwLock<Event>>) {
        let read = event.read().unwrap();
        self.event
            .write()
            .unwrap()
            .insert(read.id, (event.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Event`] from the store.
    ///
    pub fn exhume_event(&self, id: &Uuid) -> Option<Arc<RwLock<Event>>> {
        self.event
            .read()
            .unwrap()
            .get(id)
            .map(|event| event.0.clone())
    }

    /// Exorcise (remove) [`Event`] from the store.
    ///
    pub fn exorcise_event(&mut self, id: &Uuid) -> Option<Arc<RwLock<Event>>> {
        self.event
            .write()
            .unwrap()
            .remove(id)
            .map(|event| event.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Event>`.
    ///
    pub fn iter_event(&self) -> impl Iterator<Item = Arc<RwLock<Event>>> + '_ {
        let values: Vec<Arc<RwLock<Event>>> = self
            .event
            .read()
            .unwrap()
            .values()
            .map(|event| event.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Event.
    ///
    pub fn event_timestamp(&self, event: &Event) -> SystemTime {
        self.event
            .read()
            .unwrap()
            .get(&event.id)
            .map(|event| event.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`External`] into the store.
    ///
    pub fn inter_external(&mut self, external: Arc<RwLock<External>>) {
        let read = external.read().unwrap();
        self.external
            .write()
            .unwrap()
            .insert(read.id, (external.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`External`] from the store.
    ///
    pub fn exhume_external(&self, id: &Uuid) -> Option<Arc<RwLock<External>>> {
        self.external
            .read()
            .unwrap()
            .get(id)
            .map(|external| external.0.clone())
    }

    /// Exorcise (remove) [`External`] from the store.
    ///
    pub fn exorcise_external(&mut self, id: &Uuid) -> Option<Arc<RwLock<External>>> {
        self.external
            .write()
            .unwrap()
            .remove(id)
            .map(|external| external.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, External>`.
    ///
    pub fn iter_external(&self) -> impl Iterator<Item = Arc<RwLock<External>>> + '_ {
        let values: Vec<Arc<RwLock<External>>> = self
            .external
            .read()
            .unwrap()
            .values()
            .map(|external| external.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for External.
    ///
    pub fn external_timestamp(&self, external: &External) -> SystemTime {
        self.external
            .read()
            .unwrap()
            .get(&external.id)
            .map(|external| external.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Isa`] into the store.
    ///
    pub fn inter_isa(&mut self, isa: Arc<RwLock<Isa>>) {
        let read = isa.read().unwrap();
        self.isa
            .write()
            .unwrap()
            .insert(read.id, (isa.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Isa`] from the store.
    ///
    pub fn exhume_isa(&self, id: &Uuid) -> Option<Arc<RwLock<Isa>>> {
        self.isa.read().unwrap().get(id).map(|isa| isa.0.clone())
    }

    /// Exorcise (remove) [`Isa`] from the store.
    ///
    pub fn exorcise_isa(&mut self, id: &Uuid) -> Option<Arc<RwLock<Isa>>> {
        self.isa
            .write()
            .unwrap()
            .remove(id)
            .map(|isa| isa.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Isa>`.
    ///
    pub fn iter_isa(&self) -> impl Iterator<Item = Arc<RwLock<Isa>>> + '_ {
        let values: Vec<Arc<RwLock<Isa>>> = self
            .isa
            .read()
            .unwrap()
            .values()
            .map(|isa| isa.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Isa.
    ///
    pub fn isa_timestamp(&self, isa: &Isa) -> SystemTime {
        self.isa
            .read()
            .unwrap()
            .get(&isa.id)
            .map(|isa| isa.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Object`] into the store.
    ///
    pub fn inter_object(&mut self, object: Arc<RwLock<Object>>) {
        let read = object.read().unwrap();
        let value = (object.clone(), SystemTime::now());
        self.object_id_by_name
            .write()
            .unwrap()
            .insert(read.name.to_upper_camel_case(), (read.id, value.1));
        self.object.write().unwrap().insert(read.id, value);
    }

    /// Exhume (get) [`Object`] from the store.
    ///
    pub fn exhume_object(&self, id: &Uuid) -> Option<Arc<RwLock<Object>>> {
        self.object
            .read()
            .unwrap()
            .get(id)
            .map(|object| object.0.clone())
    }

    /// Exorcise (remove) [`Object`] from the store.
    ///
    pub fn exorcise_object(&mut self, id: &Uuid) -> Option<Arc<RwLock<Object>>> {
        self.object
            .write()
            .unwrap()
            .remove(id)
            .map(|object| object.0.clone())
    }

    /// Exhume [`Object`] id from the store by name.
    ///
    pub fn exhume_object_id_by_name(&self, name: &str) -> Option<Uuid> {
        self.object_id_by_name
            .read()
            .unwrap()
            .get(name)
            .map(|object| object.0)
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Object>`.
    ///
    pub fn iter_object(&self) -> impl Iterator<Item = Arc<RwLock<Object>>> + '_ {
        let values: Vec<Arc<RwLock<Object>>> = self
            .object
            .read()
            .unwrap()
            .values()
            .map(|object| object.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Object.
    ///
    pub fn object_timestamp(&self, object: &Object) -> SystemTime {
        self.object
            .read()
            .unwrap()
            .get(&object.id)
            .map(|object| object.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Referent`] into the store.
    ///
    pub fn inter_referent(&mut self, referent: Arc<RwLock<Referent>>) {
        let read = referent.read().unwrap();
        self.referent
            .write()
            .unwrap()
            .insert(read.id, (referent.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Referent`] from the store.
    ///
    pub fn exhume_referent(&self, id: &Uuid) -> Option<Arc<RwLock<Referent>>> {
        self.referent
            .read()
            .unwrap()
            .get(id)
            .map(|referent| referent.0.clone())
    }

    /// Exorcise (remove) [`Referent`] from the store.
    ///
    pub fn exorcise_referent(&mut self, id: &Uuid) -> Option<Arc<RwLock<Referent>>> {
        self.referent
            .write()
            .unwrap()
            .remove(id)
            .map(|referent| referent.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Referent>`.
    ///
    pub fn iter_referent(&self) -> impl Iterator<Item = Arc<RwLock<Referent>>> + '_ {
        let values: Vec<Arc<RwLock<Referent>>> = self
            .referent
            .read()
            .unwrap()
            .values()
            .map(|referent| referent.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Referent.
    ///
    pub fn referent_timestamp(&self, referent: &Referent) -> SystemTime {
        self.referent
            .read()
            .unwrap()
            .get(&referent.id)
            .map(|referent| referent.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Referrer`] into the store.
    ///
    pub fn inter_referrer(&mut self, referrer: Arc<RwLock<Referrer>>) {
        let read = referrer.read().unwrap();
        self.referrer
            .write()
            .unwrap()
            .insert(read.id, (referrer.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Referrer`] from the store.
    ///
    pub fn exhume_referrer(&self, id: &Uuid) -> Option<Arc<RwLock<Referrer>>> {
        self.referrer
            .read()
            .unwrap()
            .get(id)
            .map(|referrer| referrer.0.clone())
    }

    /// Exorcise (remove) [`Referrer`] from the store.
    ///
    pub fn exorcise_referrer(&mut self, id: &Uuid) -> Option<Arc<RwLock<Referrer>>> {
        self.referrer
            .write()
            .unwrap()
            .remove(id)
            .map(|referrer| referrer.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Referrer>`.
    ///
    pub fn iter_referrer(&self) -> impl Iterator<Item = Arc<RwLock<Referrer>>> + '_ {
        let values: Vec<Arc<RwLock<Referrer>>> = self
            .referrer
            .read()
            .unwrap()
            .values()
            .map(|referrer| referrer.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Referrer.
    ///
    pub fn referrer_timestamp(&self, referrer: &Referrer) -> SystemTime {
        self.referrer
            .read()
            .unwrap()
            .get(&referrer.id)
            .map(|referrer| referrer.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Relationship`] into the store.
    ///
    pub fn inter_relationship(&mut self, relationship: Arc<RwLock<Relationship>>) {
        let read = relationship.read().unwrap();
        self.relationship
            .write()
            .unwrap()
            .insert(read.id(), (relationship.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Relationship`] from the store.
    ///
    pub fn exhume_relationship(&self, id: &Uuid) -> Option<Arc<RwLock<Relationship>>> {
        self.relationship
            .read()
            .unwrap()
            .get(id)
            .map(|relationship| relationship.0.clone())
    }

    /// Exorcise (remove) [`Relationship`] from the store.
    ///
    pub fn exorcise_relationship(&mut self, id: &Uuid) -> Option<Arc<RwLock<Relationship>>> {
        self.relationship
            .write()
            .unwrap()
            .remove(id)
            .map(|relationship| relationship.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Relationship>`.
    ///
    pub fn iter_relationship(&self) -> impl Iterator<Item = Arc<RwLock<Relationship>>> + '_ {
        let values: Vec<Arc<RwLock<Relationship>>> = self
            .relationship
            .read()
            .unwrap()
            .values()
            .map(|relationship| relationship.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Relationship.
    ///
    pub fn relationship_timestamp(&self, relationship: &Relationship) -> SystemTime {
        self.relationship
            .read()
            .unwrap()
            .get(&relationship.id())
            .map(|relationship| relationship.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`State`] into the store.
    ///
    pub fn inter_state(&mut self, state: Arc<RwLock<State>>) {
        let read = state.read().unwrap();
        self.state
            .write()
            .unwrap()
            .insert(read.id, (state.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`State`] from the store.
    ///
    pub fn exhume_state(&self, id: &Uuid) -> Option<Arc<RwLock<State>>> {
        self.state
            .read()
            .unwrap()
            .get(id)
            .map(|state| state.0.clone())
    }

    /// Exorcise (remove) [`State`] from the store.
    ///
    pub fn exorcise_state(&mut self, id: &Uuid) -> Option<Arc<RwLock<State>>> {
        self.state
            .write()
            .unwrap()
            .remove(id)
            .map(|state| state.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, State>`.
    ///
    pub fn iter_state(&self) -> impl Iterator<Item = Arc<RwLock<State>>> + '_ {
        let values: Vec<Arc<RwLock<State>>> = self
            .state
            .read()
            .unwrap()
            .values()
            .map(|state| state.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for State.
    ///
    pub fn state_timestamp(&self, state: &State) -> SystemTime {
        self.state
            .read()
            .unwrap()
            .get(&state.id)
            .map(|state| state.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Subtype`] into the store.
    ///
    pub fn inter_subtype(&mut self, subtype: Arc<RwLock<Subtype>>) {
        let read = subtype.read().unwrap();
        self.subtype
            .write()
            .unwrap()
            .insert(read.id, (subtype.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Subtype`] from the store.
    ///
    pub fn exhume_subtype(&self, id: &Uuid) -> Option<Arc<RwLock<Subtype>>> {
        self.subtype
            .read()
            .unwrap()
            .get(id)
            .map(|subtype| subtype.0.clone())
    }

    /// Exorcise (remove) [`Subtype`] from the store.
    ///
    pub fn exorcise_subtype(&mut self, id: &Uuid) -> Option<Arc<RwLock<Subtype>>> {
        self.subtype
            .write()
            .unwrap()
            .remove(id)
            .map(|subtype| subtype.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Subtype>`.
    ///
    pub fn iter_subtype(&self) -> impl Iterator<Item = Arc<RwLock<Subtype>>> + '_ {
        let values: Vec<Arc<RwLock<Subtype>>> = self
            .subtype
            .read()
            .unwrap()
            .values()
            .map(|subtype| subtype.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Subtype.
    ///
    pub fn subtype_timestamp(&self, subtype: &Subtype) -> SystemTime {
        self.subtype
            .read()
            .unwrap()
            .get(&subtype.id)
            .map(|subtype| subtype.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Supertype`] into the store.
    ///
    pub fn inter_supertype(&mut self, supertype: Arc<RwLock<Supertype>>) {
        let read = supertype.read().unwrap();
        self.supertype
            .write()
            .unwrap()
            .insert(read.id, (supertype.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Supertype`] from the store.
    ///
    pub fn exhume_supertype(&self, id: &Uuid) -> Option<Arc<RwLock<Supertype>>> {
        self.supertype
            .read()
            .unwrap()
            .get(id)
            .map(|supertype| supertype.0.clone())
    }

    /// Exorcise (remove) [`Supertype`] from the store.
    ///
    pub fn exorcise_supertype(&mut self, id: &Uuid) -> Option<Arc<RwLock<Supertype>>> {
        self.supertype
            .write()
            .unwrap()
            .remove(id)
            .map(|supertype| supertype.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Supertype>`.
    ///
    pub fn iter_supertype(&self) -> impl Iterator<Item = Arc<RwLock<Supertype>>> + '_ {
        let values: Vec<Arc<RwLock<Supertype>>> = self
            .supertype
            .read()
            .unwrap()
            .values()
            .map(|supertype| supertype.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Supertype.
    ///
    pub fn supertype_timestamp(&self, supertype: &Supertype) -> SystemTime {
        self.supertype
            .read()
            .unwrap()
            .get(&supertype.id)
            .map(|supertype| supertype.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Ty`] into the store.
    ///
    pub fn inter_ty(&mut self, ty: Arc<RwLock<Ty>>) {
        let read = ty.read().unwrap();
        self.ty
            .write()
            .unwrap()
            .insert(read.id(), (ty.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Ty`] from the store.
    ///
    pub fn exhume_ty(&self, id: &Uuid) -> Option<Arc<RwLock<Ty>>> {
        self.ty.read().unwrap().get(id).map(|ty| ty.0.clone())
    }

    /// Exorcise (remove) [`Ty`] from the store.
    ///
    pub fn exorcise_ty(&mut self, id: &Uuid) -> Option<Arc<RwLock<Ty>>> {
        self.ty.write().unwrap().remove(id).map(|ty| ty.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Ty>`.
    ///
    pub fn iter_ty(&self) -> impl Iterator<Item = Arc<RwLock<Ty>>> + '_ {
        let values: Vec<Arc<RwLock<Ty>>> = self
            .ty
            .read()
            .unwrap()
            .values()
            .map(|ty| ty.0.clone())
            .collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Ty.
    ///
    pub fn ty_timestamp(&self, ty: &Ty) -> SystemTime {
        self.ty
            .read()
            .unwrap()
            .get(&ty.id())
            .map(|ty| ty.1)
            .unwrap_or(SystemTime::now())
    }

    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"sarzak-object-store-persistence"}}}
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

        let path = path.join("sarzak.json");
        fs::create_dir_all(&path)?;

        // Persist Acknowledged Event.
        {
            let path = path.join("acknowledged_event");
            fs::create_dir_all(&path)?;
            for acknowledged_event_tuple in self.acknowledged_event.read().unwrap().values() {
                let path = path.join(format!(
                    "{}.json",
                    acknowledged_event_tuple.0.read().unwrap().id
                ));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<AcknowledgedEvent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != acknowledged_event_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &acknowledged_event_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &acknowledged_event_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.acknowledged_event.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist An Associative Referent.
        {
            let path = path.join("an_associative_referent");
            fs::create_dir_all(&path)?;
            for an_associative_referent_tuple in
                self.an_associative_referent.read().unwrap().values()
            {
                let path = path.join(format!(
                    "{}.json",
                    an_associative_referent_tuple.0.read().unwrap().id
                ));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<AnAssociativeReferent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != an_associative_referent_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &an_associative_referent_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &an_associative_referent_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self
                        .an_associative_referent
                        .read()
                        .unwrap()
                        .contains_key(&id)
                    {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Associative.
        {
            let path = path.join("associative");
            fs::create_dir_all(&path)?;
            for associative_tuple in self.associative.read().unwrap().values() {
                let path = path.join(format!("{}.json", associative_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Associative>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != associative_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &associative_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &associative_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.associative.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Associative Referent.
        {
            let path = path.join("associative_referent");
            fs::create_dir_all(&path)?;
            for associative_referent_tuple in self.associative_referent.read().unwrap().values() {
                let path = path.join(format!(
                    "{}.json",
                    associative_referent_tuple.0.read().unwrap().id
                ));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<AssociativeReferent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != associative_referent_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &associative_referent_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &associative_referent_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.associative_referent.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Associative Referrer.
        {
            let path = path.join("associative_referrer");
            fs::create_dir_all(&path)?;
            for associative_referrer_tuple in self.associative_referrer.read().unwrap().values() {
                let path = path.join(format!(
                    "{}.json",
                    associative_referrer_tuple.0.read().unwrap().id
                ));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<AssociativeReferrer>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != associative_referrer_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &associative_referrer_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &associative_referrer_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.associative_referrer.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Attribute.
        {
            let path = path.join("attribute");
            fs::create_dir_all(&path)?;
            for attribute_tuple in self.attribute.read().unwrap().values() {
                let path = path.join(format!("{}.json", attribute_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Attribute>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != attribute_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &attribute_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &attribute_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.attribute.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Binary.
        {
            let path = path.join("binary");
            fs::create_dir_all(&path)?;
            for binary_tuple in self.binary.read().unwrap().values() {
                let path = path.join(format!("{}.json", binary_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Binary>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != binary_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &binary_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &binary_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.binary.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Cardinality.
        {
            let path = path.join("cardinality");
            fs::create_dir_all(&path)?;
            for cardinality_tuple in self.cardinality.read().unwrap().values() {
                let path = path.join(format!("{}.json", cardinality_tuple.0.read().unwrap().id()));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Cardinality>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != cardinality_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &cardinality_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &cardinality_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.cardinality.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Conditionality.
        {
            let path = path.join("conditionality");
            fs::create_dir_all(&path)?;
            for conditionality_tuple in self.conditionality.read().unwrap().values() {
                let path = path.join(format!(
                    "{}.json",
                    conditionality_tuple.0.read().unwrap().id()
                ));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Conditionality>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != conditionality_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &conditionality_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &conditionality_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.conditionality.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Event.
        {
            let path = path.join("event");
            fs::create_dir_all(&path)?;
            for event_tuple in self.event.read().unwrap().values() {
                let path = path.join(format!("{}.json", event_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Event>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != event_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &event_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &event_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.event.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist External.
        {
            let path = path.join("external");
            fs::create_dir_all(&path)?;
            for external_tuple in self.external.read().unwrap().values() {
                let path = path.join(format!("{}.json", external_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<External>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != external_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &external_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &external_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.external.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Isa.
        {
            let path = path.join("isa");
            fs::create_dir_all(&path)?;
            for isa_tuple in self.isa.read().unwrap().values() {
                let path = path.join(format!("{}.json", isa_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Isa>>, SystemTime) = serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != isa_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &isa_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &isa_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.isa.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Object.
        {
            let path = path.join("object");
            fs::create_dir_all(&path)?;
            for object_tuple in self.object.read().unwrap().values() {
                let path = path.join(format!("{}.json", object_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Object>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != object_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &object_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &object_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.object.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Referent.
        {
            let path = path.join("referent");
            fs::create_dir_all(&path)?;
            for referent_tuple in self.referent.read().unwrap().values() {
                let path = path.join(format!("{}.json", referent_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Referent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != referent_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &referent_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &referent_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.referent.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Referrer.
        {
            let path = path.join("referrer");
            fs::create_dir_all(&path)?;
            for referrer_tuple in self.referrer.read().unwrap().values() {
                let path = path.join(format!("{}.json", referrer_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Referrer>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != referrer_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &referrer_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &referrer_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.referrer.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Relationship.
        {
            let path = path.join("relationship");
            fs::create_dir_all(&path)?;
            for relationship_tuple in self.relationship.read().unwrap().values() {
                let path = path.join(format!(
                    "{}.json",
                    relationship_tuple.0.read().unwrap().id()
                ));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Relationship>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != relationship_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &relationship_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &relationship_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.relationship.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist State.
        {
            let path = path.join("state");
            fs::create_dir_all(&path)?;
            for state_tuple in self.state.read().unwrap().values() {
                let path = path.join(format!("{}.json", state_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<State>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != state_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &state_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &state_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.state.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Subtype.
        {
            let path = path.join("subtype");
            fs::create_dir_all(&path)?;
            for subtype_tuple in self.subtype.read().unwrap().values() {
                let path = path.join(format!("{}.json", subtype_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Subtype>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != subtype_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &subtype_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &subtype_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.subtype.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Supertype.
        {
            let path = path.join("supertype");
            fs::create_dir_all(&path)?;
            for supertype_tuple in self.supertype.read().unwrap().values() {
                let path = path.join(format!("{}.json", supertype_tuple.0.read().unwrap().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Supertype>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned()
                        != supertype_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &supertype_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &supertype_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.supertype.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Type.
        {
            let path = path.join("ty");
            fs::create_dir_all(&path)?;
            for ty_tuple in self.ty.read().unwrap().values() {
                let path = path.join(format!("{}.json", ty_tuple.0.read().unwrap().id()));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Arc<RwLock<Ty>>, SystemTime) = serde_json::from_reader(reader)?;
                    if on_disk.0.read().unwrap().to_owned() != ty_tuple.0.read().unwrap().to_owned()
                    {
                        let file = fs::File::create(path)?;
                        let mut writer = io::BufWriter::new(file);
                        serde_json::to_writer_pretty(&mut writer, &ty_tuple)?;
                    }
                } else {
                    let file = fs::File::create(&path)?;
                    let mut writer = io::BufWriter::new(file);
                    serde_json::to_writer_pretty(&mut writer, &ty_tuple)?;
                }
            }
            for file in fs::read_dir(&path)? {
                let file = file?;
                let path = file.path();
                let file_name = path.file_name().unwrap().to_str().unwrap();
                let id = file_name.split('.').next().unwrap();
                if let Ok(id) = Uuid::parse_str(id) {
                    if !self.ty.read().unwrap().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
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
        let path = path.join("sarzak.json");

        let store = Self::new();

        // Load Acknowledged Event.
        {
            let path = path.join("acknowledged_event");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let acknowledged_event: (Arc<RwLock<AcknowledgedEvent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.acknowledged_event.write().unwrap().insert(
                    acknowledged_event.0.read().unwrap().id,
                    acknowledged_event.clone(),
                );
            }
        }

        // Load An Associative Referent.
        {
            let path = path.join("an_associative_referent");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let an_associative_referent: (Arc<RwLock<AnAssociativeReferent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.an_associative_referent.write().unwrap().insert(
                    an_associative_referent.0.read().unwrap().id,
                    an_associative_referent.clone(),
                );
            }
        }

        // Load Associative.
        {
            let path = path.join("associative");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let associative: (Arc<RwLock<Associative>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .associative
                    .write()
                    .unwrap()
                    .insert(associative.0.read().unwrap().id, associative.clone());
            }
        }

        // Load Associative Referent.
        {
            let path = path.join("associative_referent");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let associative_referent: (Arc<RwLock<AssociativeReferent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.associative_referent.write().unwrap().insert(
                    associative_referent.0.read().unwrap().id,
                    associative_referent.clone(),
                );
            }
        }

        // Load Associative Referrer.
        {
            let path = path.join("associative_referrer");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let associative_referrer: (Arc<RwLock<AssociativeReferrer>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.associative_referrer.write().unwrap().insert(
                    associative_referrer.0.read().unwrap().id,
                    associative_referrer.clone(),
                );
            }
        }

        // Load Attribute.
        {
            let path = path.join("attribute");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let attribute: (Arc<RwLock<Attribute>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .attribute
                    .write()
                    .unwrap()
                    .insert(attribute.0.read().unwrap().id, attribute.clone());
            }
        }

        // Load Binary.
        {
            let path = path.join("binary");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let binary: (Arc<RwLock<Binary>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .binary
                    .write()
                    .unwrap()
                    .insert(binary.0.read().unwrap().id, binary.clone());
            }
        }

        // Load Cardinality.
        {
            let path = path.join("cardinality");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let cardinality: (Arc<RwLock<Cardinality>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .cardinality
                    .write()
                    .unwrap()
                    .insert(cardinality.0.read().unwrap().id(), cardinality.clone());
            }
        }

        // Load Conditionality.
        {
            let path = path.join("conditionality");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let conditionality: (Arc<RwLock<Conditionality>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.conditionality.write().unwrap().insert(
                    conditionality.0.read().unwrap().id(),
                    conditionality.clone(),
                );
            }
        }

        // Load Event.
        {
            let path = path.join("event");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let event: (Arc<RwLock<Event>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .event
                    .write()
                    .unwrap()
                    .insert(event.0.read().unwrap().id, event.clone());
            }
        }

        // Load External.
        {
            let path = path.join("external");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let external: (Arc<RwLock<External>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .external
                    .write()
                    .unwrap()
                    .insert(external.0.read().unwrap().id, external.clone());
            }
        }

        // Load Isa.
        {
            let path = path.join("isa");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let isa: (Arc<RwLock<Isa>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .isa
                    .write()
                    .unwrap()
                    .insert(isa.0.read().unwrap().id, isa.clone());
            }
        }

        // Load Object.
        {
            let path = path.join("object");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let object: (Arc<RwLock<Object>>, SystemTime) = serde_json::from_reader(reader)?;
                store.object_id_by_name.write().unwrap().insert(
                    object.0.read().unwrap().name.to_upper_camel_case(),
                    (object.0.read().unwrap().id, object.1),
                );
                store
                    .object
                    .write()
                    .unwrap()
                    .insert(object.0.read().unwrap().id, object.clone());
            }
        }

        // Load Referent.
        {
            let path = path.join("referent");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let referent: (Arc<RwLock<Referent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .referent
                    .write()
                    .unwrap()
                    .insert(referent.0.read().unwrap().id, referent.clone());
            }
        }

        // Load Referrer.
        {
            let path = path.join("referrer");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let referrer: (Arc<RwLock<Referrer>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .referrer
                    .write()
                    .unwrap()
                    .insert(referrer.0.read().unwrap().id, referrer.clone());
            }
        }

        // Load Relationship.
        {
            let path = path.join("relationship");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let relationship: (Arc<RwLock<Relationship>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .relationship
                    .write()
                    .unwrap()
                    .insert(relationship.0.read().unwrap().id(), relationship.clone());
            }
        }

        // Load State.
        {
            let path = path.join("state");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let state: (Arc<RwLock<State>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .state
                    .write()
                    .unwrap()
                    .insert(state.0.read().unwrap().id, state.clone());
            }
        }

        // Load Subtype.
        {
            let path = path.join("subtype");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let subtype: (Arc<RwLock<Subtype>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .subtype
                    .write()
                    .unwrap()
                    .insert(subtype.0.read().unwrap().id, subtype.clone());
            }
        }

        // Load Supertype.
        {
            let path = path.join("supertype");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let supertype: (Arc<RwLock<Supertype>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .supertype
                    .write()
                    .unwrap()
                    .insert(supertype.0.read().unwrap().id, supertype.clone());
            }
        }

        // Load Type.
        {
            let path = path.join("ty");
            let entries = fs::read_dir(path)?;
            for entry in entries {
                let entry = entry?;
                let path = entry.path();
                let file = fs::File::open(path)?;
                let reader = io::BufReader::new(file);
                let ty: (Arc<RwLock<Ty>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .ty
                    .write()
                    .unwrap()
                    .insert(ty.0.read().unwrap().id(), ty.clone());
            }
        }

        Ok(store)
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
