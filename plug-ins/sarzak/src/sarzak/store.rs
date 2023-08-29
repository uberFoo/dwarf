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
use std::cell::RefCell;
use std::rc::Rc;
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
    acknowledged_event: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<AcknowledgedEvent>>, SystemTime)>>>,
    an_associative_referent:
        Rc<RefCell<HashMap<Uuid, (Rc<RefCell<AnAssociativeReferent>>, SystemTime)>>>,
    associative: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Associative>>, SystemTime)>>>,
    associative_referent:
        Rc<RefCell<HashMap<Uuid, (Rc<RefCell<AssociativeReferent>>, SystemTime)>>>,
    associative_referrer:
        Rc<RefCell<HashMap<Uuid, (Rc<RefCell<AssociativeReferrer>>, SystemTime)>>>,
    attribute: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Attribute>>, SystemTime)>>>,
    binary: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Binary>>, SystemTime)>>>,
    cardinality: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Cardinality>>, SystemTime)>>>,
    conditionality: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Conditionality>>, SystemTime)>>>,
    event: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Event>>, SystemTime)>>>,
    external: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<External>>, SystemTime)>>>,
    isa: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Isa>>, SystemTime)>>>,
    object: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Object>>, SystemTime)>>>,
    object_id_by_name: Rc<RefCell<HashMap<String, (Uuid, SystemTime)>>>,
    referent: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Referent>>, SystemTime)>>>,
    referrer: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Referrer>>, SystemTime)>>>,
    relationship: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Relationship>>, SystemTime)>>>,
    state: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<State>>, SystemTime)>>>,
    subtype: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Subtype>>, SystemTime)>>>,
    supertype: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Supertype>>, SystemTime)>>>,
    ty: Rc<RefCell<HashMap<Uuid, (Rc<RefCell<Ty>>, SystemTime)>>>,
}

impl ObjectStore {
    pub fn new() -> Self {
        let mut store = Self {
            acknowledged_event: Rc::new(RefCell::new(HashMap::default())),
            an_associative_referent: Rc::new(RefCell::new(HashMap::default())),
            associative: Rc::new(RefCell::new(HashMap::default())),
            associative_referent: Rc::new(RefCell::new(HashMap::default())),
            associative_referrer: Rc::new(RefCell::new(HashMap::default())),
            attribute: Rc::new(RefCell::new(HashMap::default())),
            binary: Rc::new(RefCell::new(HashMap::default())),
            cardinality: Rc::new(RefCell::new(HashMap::default())),
            conditionality: Rc::new(RefCell::new(HashMap::default())),
            event: Rc::new(RefCell::new(HashMap::default())),
            external: Rc::new(RefCell::new(HashMap::default())),
            isa: Rc::new(RefCell::new(HashMap::default())),
            object: Rc::new(RefCell::new(HashMap::default())),
            object_id_by_name: Rc::new(RefCell::new(HashMap::default())),
            referent: Rc::new(RefCell::new(HashMap::default())),
            referrer: Rc::new(RefCell::new(HashMap::default())),
            relationship: Rc::new(RefCell::new(HashMap::default())),
            state: Rc::new(RefCell::new(HashMap::default())),
            subtype: Rc::new(RefCell::new(HashMap::default())),
            supertype: Rc::new(RefCell::new(HashMap::default())),
            ty: Rc::new(RefCell::new(HashMap::default())),
        };

        // Initialize Singleton Subtypes
        // 💥 Look at how beautiful this generated code is for super/sub-type graphs!
        // I remember having a bit of a struggle making it work. It's recursive, with
        // a lot of special cases, and I think it calls other recursive functions...💥
        store.inter_cardinality(Rc::new(RefCell::new(Cardinality::Many(MANY))));
        store.inter_cardinality(Rc::new(RefCell::new(Cardinality::One(ONE))));
        store.inter_conditionality(Rc::new(RefCell::new(Conditionality::Conditional(
            CONDITIONAL,
        ))));
        store.inter_conditionality(Rc::new(RefCell::new(Conditionality::Unconditional(
            UNCONDITIONAL,
        ))));
        store.inter_ty(Rc::new(RefCell::new(Ty::Boolean(BOOLEAN))));
        store.inter_ty(Rc::new(RefCell::new(Ty::Float(FLOAT))));
        store.inter_ty(Rc::new(RefCell::new(Ty::Integer(INTEGER))));
        store.inter_ty(Rc::new(RefCell::new(Ty::SString(S_STRING))));
        store.inter_ty(Rc::new(RefCell::new(Ty::SUuid(S_UUID))));

        store
    }

    // {"magic":"","directive":{"Start":{"directive":"ignore-orig","tag":"sarzak-object-store-methods"}}}
    /// Inter (insert) [`AcknowledgedEvent`] into the store.
    ///
    pub fn inter_acknowledged_event(&mut self, acknowledged_event: Rc<RefCell<AcknowledgedEvent>>) {
        let read = acknowledged_event.borrow();
        self.acknowledged_event
            .borrow_mut()
            .insert(read.id, (acknowledged_event.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`AcknowledgedEvent`] from the store.
    ///
    pub fn exhume_acknowledged_event(&self, id: &Uuid) -> Option<Rc<RefCell<AcknowledgedEvent>>> {
        self.acknowledged_event
            .borrow()
            .get(id)
            .map(|acknowledged_event| acknowledged_event.0.clone())
    }

    /// Exorcise (remove) [`AcknowledgedEvent`] from the store.
    ///
    pub fn exorcise_acknowledged_event(
        &mut self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<AcknowledgedEvent>>> {
        self.acknowledged_event
            .borrow_mut()
            .remove(id)
            .map(|acknowledged_event| acknowledged_event.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AcknowledgedEvent>`.
    ///
    pub fn iter_acknowledged_event(
        &self,
    ) -> impl Iterator<Item = Rc<RefCell<AcknowledgedEvent>>> + '_ {
        let values: Vec<Rc<RefCell<AcknowledgedEvent>>> = self
            .acknowledged_event
            .borrow()
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
            .borrow()
            .get(&acknowledged_event.id)
            .map(|acknowledged_event| acknowledged_event.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`AnAssociativeReferent`] into the store.
    ///
    pub fn inter_an_associative_referent(
        &mut self,
        an_associative_referent: Rc<RefCell<AnAssociativeReferent>>,
    ) {
        let read = an_associative_referent.borrow();
        self.an_associative_referent.borrow_mut().insert(
            read.id,
            (an_associative_referent.clone(), SystemTime::now()),
        );
    }

    /// Exhume (get) [`AnAssociativeReferent`] from the store.
    ///
    pub fn exhume_an_associative_referent(
        &self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<AnAssociativeReferent>>> {
        self.an_associative_referent
            .borrow()
            .get(id)
            .map(|an_associative_referent| an_associative_referent.0.clone())
    }

    /// Exorcise (remove) [`AnAssociativeReferent`] from the store.
    ///
    pub fn exorcise_an_associative_referent(
        &mut self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<AnAssociativeReferent>>> {
        self.an_associative_referent
            .borrow_mut()
            .remove(id)
            .map(|an_associative_referent| an_associative_referent.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AnAssociativeReferent>`.
    ///
    pub fn iter_an_associative_referent(
        &self,
    ) -> impl Iterator<Item = Rc<RefCell<AnAssociativeReferent>>> + '_ {
        let values: Vec<Rc<RefCell<AnAssociativeReferent>>> = self
            .an_associative_referent
            .borrow()
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
            .borrow()
            .get(&an_associative_referent.id)
            .map(|an_associative_referent| an_associative_referent.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Associative`] into the store.
    ///
    pub fn inter_associative(&mut self, associative: Rc<RefCell<Associative>>) {
        let read = associative.borrow();
        self.associative
            .borrow_mut()
            .insert(read.id, (associative.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Associative`] from the store.
    ///
    pub fn exhume_associative(&self, id: &Uuid) -> Option<Rc<RefCell<Associative>>> {
        self.associative
            .borrow()
            .get(id)
            .map(|associative| associative.0.clone())
    }

    /// Exorcise (remove) [`Associative`] from the store.
    ///
    pub fn exorcise_associative(&mut self, id: &Uuid) -> Option<Rc<RefCell<Associative>>> {
        self.associative
            .borrow_mut()
            .remove(id)
            .map(|associative| associative.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Associative>`.
    ///
    pub fn iter_associative(&self) -> impl Iterator<Item = Rc<RefCell<Associative>>> + '_ {
        let values: Vec<Rc<RefCell<Associative>>> = self
            .associative
            .borrow()
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
            .borrow()
            .get(&associative.id)
            .map(|associative| associative.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`AssociativeReferent`] into the store.
    ///
    pub fn inter_associative_referent(
        &mut self,
        associative_referent: Rc<RefCell<AssociativeReferent>>,
    ) {
        let read = associative_referent.borrow();
        self.associative_referent
            .borrow_mut()
            .insert(read.id, (associative_referent.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`AssociativeReferent`] from the store.
    ///
    pub fn exhume_associative_referent(
        &self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<AssociativeReferent>>> {
        self.associative_referent
            .borrow()
            .get(id)
            .map(|associative_referent| associative_referent.0.clone())
    }

    /// Exorcise (remove) [`AssociativeReferent`] from the store.
    ///
    pub fn exorcise_associative_referent(
        &mut self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<AssociativeReferent>>> {
        self.associative_referent
            .borrow_mut()
            .remove(id)
            .map(|associative_referent| associative_referent.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AssociativeReferent>`.
    ///
    pub fn iter_associative_referent(
        &self,
    ) -> impl Iterator<Item = Rc<RefCell<AssociativeReferent>>> + '_ {
        let values: Vec<Rc<RefCell<AssociativeReferent>>> = self
            .associative_referent
            .borrow()
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
            .borrow()
            .get(&associative_referent.id)
            .map(|associative_referent| associative_referent.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`AssociativeReferrer`] into the store.
    ///
    pub fn inter_associative_referrer(
        &mut self,
        associative_referrer: Rc<RefCell<AssociativeReferrer>>,
    ) {
        let read = associative_referrer.borrow();
        self.associative_referrer
            .borrow_mut()
            .insert(read.id, (associative_referrer.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`AssociativeReferrer`] from the store.
    ///
    pub fn exhume_associative_referrer(
        &self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<AssociativeReferrer>>> {
        self.associative_referrer
            .borrow()
            .get(id)
            .map(|associative_referrer| associative_referrer.0.clone())
    }

    /// Exorcise (remove) [`AssociativeReferrer`] from the store.
    ///
    pub fn exorcise_associative_referrer(
        &mut self,
        id: &Uuid,
    ) -> Option<Rc<RefCell<AssociativeReferrer>>> {
        self.associative_referrer
            .borrow_mut()
            .remove(id)
            .map(|associative_referrer| associative_referrer.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, AssociativeReferrer>`.
    ///
    pub fn iter_associative_referrer(
        &self,
    ) -> impl Iterator<Item = Rc<RefCell<AssociativeReferrer>>> + '_ {
        let values: Vec<Rc<RefCell<AssociativeReferrer>>> = self
            .associative_referrer
            .borrow()
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
            .borrow()
            .get(&associative_referrer.id)
            .map(|associative_referrer| associative_referrer.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Attribute`] into the store.
    ///
    pub fn inter_attribute(&mut self, attribute: Rc<RefCell<Attribute>>) {
        let read = attribute.borrow();
        self.attribute
            .borrow_mut()
            .insert(read.id, (attribute.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Attribute`] from the store.
    ///
    pub fn exhume_attribute(&self, id: &Uuid) -> Option<Rc<RefCell<Attribute>>> {
        self.attribute
            .borrow()
            .get(id)
            .map(|attribute| attribute.0.clone())
    }

    /// Exorcise (remove) [`Attribute`] from the store.
    ///
    pub fn exorcise_attribute(&mut self, id: &Uuid) -> Option<Rc<RefCell<Attribute>>> {
        self.attribute
            .borrow_mut()
            .remove(id)
            .map(|attribute| attribute.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Attribute>`.
    ///
    pub fn iter_attribute(&self) -> impl Iterator<Item = Rc<RefCell<Attribute>>> + '_ {
        let values: Vec<Rc<RefCell<Attribute>>> = self
            .attribute
            .borrow()
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
            .borrow()
            .get(&attribute.id)
            .map(|attribute| attribute.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Binary`] into the store.
    ///
    pub fn inter_binary(&mut self, binary: Rc<RefCell<Binary>>) {
        let read = binary.borrow();
        self.binary
            .borrow_mut()
            .insert(read.id, (binary.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Binary`] from the store.
    ///
    pub fn exhume_binary(&self, id: &Uuid) -> Option<Rc<RefCell<Binary>>> {
        self.binary.borrow().get(id).map(|binary| binary.0.clone())
    }

    /// Exorcise (remove) [`Binary`] from the store.
    ///
    pub fn exorcise_binary(&mut self, id: &Uuid) -> Option<Rc<RefCell<Binary>>> {
        self.binary
            .borrow_mut()
            .remove(id)
            .map(|binary| binary.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Binary>`.
    ///
    pub fn iter_binary(&self) -> impl Iterator<Item = Rc<RefCell<Binary>>> + '_ {
        let values: Vec<Rc<RefCell<Binary>>> = self
            .binary
            .borrow()
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
            .borrow()
            .get(&binary.id)
            .map(|binary| binary.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Cardinality`] into the store.
    ///
    pub fn inter_cardinality(&mut self, cardinality: Rc<RefCell<Cardinality>>) {
        let read = cardinality.borrow();
        self.cardinality
            .borrow_mut()
            .insert(read.id(), (cardinality.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Cardinality`] from the store.
    ///
    pub fn exhume_cardinality(&self, id: &Uuid) -> Option<Rc<RefCell<Cardinality>>> {
        self.cardinality
            .borrow()
            .get(id)
            .map(|cardinality| cardinality.0.clone())
    }

    /// Exorcise (remove) [`Cardinality`] from the store.
    ///
    pub fn exorcise_cardinality(&mut self, id: &Uuid) -> Option<Rc<RefCell<Cardinality>>> {
        self.cardinality
            .borrow_mut()
            .remove(id)
            .map(|cardinality| cardinality.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Cardinality>`.
    ///
    pub fn iter_cardinality(&self) -> impl Iterator<Item = Rc<RefCell<Cardinality>>> + '_ {
        let values: Vec<Rc<RefCell<Cardinality>>> = self
            .cardinality
            .borrow()
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
            .borrow()
            .get(&cardinality.id())
            .map(|cardinality| cardinality.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Conditionality`] into the store.
    ///
    pub fn inter_conditionality(&mut self, conditionality: Rc<RefCell<Conditionality>>) {
        let read = conditionality.borrow();
        self.conditionality
            .borrow_mut()
            .insert(read.id(), (conditionality.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Conditionality`] from the store.
    ///
    pub fn exhume_conditionality(&self, id: &Uuid) -> Option<Rc<RefCell<Conditionality>>> {
        self.conditionality
            .borrow()
            .get(id)
            .map(|conditionality| conditionality.0.clone())
    }

    /// Exorcise (remove) [`Conditionality`] from the store.
    ///
    pub fn exorcise_conditionality(&mut self, id: &Uuid) -> Option<Rc<RefCell<Conditionality>>> {
        self.conditionality
            .borrow_mut()
            .remove(id)
            .map(|conditionality| conditionality.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Conditionality>`.
    ///
    pub fn iter_conditionality(&self) -> impl Iterator<Item = Rc<RefCell<Conditionality>>> + '_ {
        let values: Vec<Rc<RefCell<Conditionality>>> = self
            .conditionality
            .borrow()
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
            .borrow()
            .get(&conditionality.id())
            .map(|conditionality| conditionality.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Event`] into the store.
    ///
    pub fn inter_event(&mut self, event: Rc<RefCell<Event>>) {
        let read = event.borrow();
        self.event
            .borrow_mut()
            .insert(read.id, (event.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Event`] from the store.
    ///
    pub fn exhume_event(&self, id: &Uuid) -> Option<Rc<RefCell<Event>>> {
        self.event.borrow().get(id).map(|event| event.0.clone())
    }

    /// Exorcise (remove) [`Event`] from the store.
    ///
    pub fn exorcise_event(&mut self, id: &Uuid) -> Option<Rc<RefCell<Event>>> {
        self.event
            .borrow_mut()
            .remove(id)
            .map(|event| event.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Event>`.
    ///
    pub fn iter_event(&self) -> impl Iterator<Item = Rc<RefCell<Event>>> + '_ {
        let values: Vec<Rc<RefCell<Event>>> = self
            .event
            .borrow()
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
            .borrow()
            .get(&event.id)
            .map(|event| event.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`External`] into the store.
    ///
    pub fn inter_external(&mut self, external: Rc<RefCell<External>>) {
        let read = external.borrow();
        self.external
            .borrow_mut()
            .insert(read.id, (external.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`External`] from the store.
    ///
    pub fn exhume_external(&self, id: &Uuid) -> Option<Rc<RefCell<External>>> {
        self.external
            .borrow()
            .get(id)
            .map(|external| external.0.clone())
    }

    /// Exorcise (remove) [`External`] from the store.
    ///
    pub fn exorcise_external(&mut self, id: &Uuid) -> Option<Rc<RefCell<External>>> {
        self.external
            .borrow_mut()
            .remove(id)
            .map(|external| external.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, External>`.
    ///
    pub fn iter_external(&self) -> impl Iterator<Item = Rc<RefCell<External>>> + '_ {
        let values: Vec<Rc<RefCell<External>>> = self
            .external
            .borrow()
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
            .borrow()
            .get(&external.id)
            .map(|external| external.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Isa`] into the store.
    ///
    pub fn inter_isa(&mut self, isa: Rc<RefCell<Isa>>) {
        let read = isa.borrow();
        self.isa
            .borrow_mut()
            .insert(read.id, (isa.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Isa`] from the store.
    ///
    pub fn exhume_isa(&self, id: &Uuid) -> Option<Rc<RefCell<Isa>>> {
        self.isa.borrow().get(id).map(|isa| isa.0.clone())
    }

    /// Exorcise (remove) [`Isa`] from the store.
    ///
    pub fn exorcise_isa(&mut self, id: &Uuid) -> Option<Rc<RefCell<Isa>>> {
        self.isa.borrow_mut().remove(id).map(|isa| isa.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Isa>`.
    ///
    pub fn iter_isa(&self) -> impl Iterator<Item = Rc<RefCell<Isa>>> + '_ {
        let values: Vec<Rc<RefCell<Isa>>> = self
            .isa
            .borrow()
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
            .borrow()
            .get(&isa.id)
            .map(|isa| isa.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Object`] into the store.
    ///
    pub fn inter_object(&mut self, object: Rc<RefCell<Object>>) {
        let read = object.borrow();
        let value = (object.clone(), SystemTime::now());
        self.object_id_by_name
            .borrow_mut()
            .insert(read.name.to_upper_camel_case(), (read.id, value.1));
        self.object.borrow_mut().insert(read.id, value);
    }

    /// Exhume (get) [`Object`] from the store.
    ///
    pub fn exhume_object(&self, id: &Uuid) -> Option<Rc<RefCell<Object>>> {
        self.object.borrow().get(id).map(|object| object.0.clone())
    }

    /// Exorcise (remove) [`Object`] from the store.
    ///
    pub fn exorcise_object(&mut self, id: &Uuid) -> Option<Rc<RefCell<Object>>> {
        self.object
            .borrow_mut()
            .remove(id)
            .map(|object| object.0.clone())
    }

    /// Exhume [`Object`] id from the store by name.
    ///
    pub fn exhume_object_id_by_name(&self, name: &str) -> Option<Uuid> {
        self.object_id_by_name
            .borrow()
            .get(name)
            .map(|object| object.0)
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Object>`.
    ///
    pub fn iter_object(&self) -> impl Iterator<Item = Rc<RefCell<Object>>> + '_ {
        let values: Vec<Rc<RefCell<Object>>> = self
            .object
            .borrow()
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
            .borrow()
            .get(&object.id)
            .map(|object| object.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Referent`] into the store.
    ///
    pub fn inter_referent(&mut self, referent: Rc<RefCell<Referent>>) {
        let read = referent.borrow();
        self.referent
            .borrow_mut()
            .insert(read.id, (referent.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Referent`] from the store.
    ///
    pub fn exhume_referent(&self, id: &Uuid) -> Option<Rc<RefCell<Referent>>> {
        self.referent
            .borrow()
            .get(id)
            .map(|referent| referent.0.clone())
    }

    /// Exorcise (remove) [`Referent`] from the store.
    ///
    pub fn exorcise_referent(&mut self, id: &Uuid) -> Option<Rc<RefCell<Referent>>> {
        self.referent
            .borrow_mut()
            .remove(id)
            .map(|referent| referent.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Referent>`.
    ///
    pub fn iter_referent(&self) -> impl Iterator<Item = Rc<RefCell<Referent>>> + '_ {
        let values: Vec<Rc<RefCell<Referent>>> = self
            .referent
            .borrow()
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
            .borrow()
            .get(&referent.id)
            .map(|referent| referent.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Referrer`] into the store.
    ///
    pub fn inter_referrer(&mut self, referrer: Rc<RefCell<Referrer>>) {
        let read = referrer.borrow();
        self.referrer
            .borrow_mut()
            .insert(read.id, (referrer.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Referrer`] from the store.
    ///
    pub fn exhume_referrer(&self, id: &Uuid) -> Option<Rc<RefCell<Referrer>>> {
        self.referrer
            .borrow()
            .get(id)
            .map(|referrer| referrer.0.clone())
    }

    /// Exorcise (remove) [`Referrer`] from the store.
    ///
    pub fn exorcise_referrer(&mut self, id: &Uuid) -> Option<Rc<RefCell<Referrer>>> {
        self.referrer
            .borrow_mut()
            .remove(id)
            .map(|referrer| referrer.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Referrer>`.
    ///
    pub fn iter_referrer(&self) -> impl Iterator<Item = Rc<RefCell<Referrer>>> + '_ {
        let values: Vec<Rc<RefCell<Referrer>>> = self
            .referrer
            .borrow()
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
            .borrow()
            .get(&referrer.id)
            .map(|referrer| referrer.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Relationship`] into the store.
    ///
    pub fn inter_relationship(&mut self, relationship: Rc<RefCell<Relationship>>) {
        let read = relationship.borrow();
        self.relationship
            .borrow_mut()
            .insert(read.id(), (relationship.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Relationship`] from the store.
    ///
    pub fn exhume_relationship(&self, id: &Uuid) -> Option<Rc<RefCell<Relationship>>> {
        self.relationship
            .borrow()
            .get(id)
            .map(|relationship| relationship.0.clone())
    }

    /// Exorcise (remove) [`Relationship`] from the store.
    ///
    pub fn exorcise_relationship(&mut self, id: &Uuid) -> Option<Rc<RefCell<Relationship>>> {
        self.relationship
            .borrow_mut()
            .remove(id)
            .map(|relationship| relationship.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Relationship>`.
    ///
    pub fn iter_relationship(&self) -> impl Iterator<Item = Rc<RefCell<Relationship>>> + '_ {
        let values: Vec<Rc<RefCell<Relationship>>> = self
            .relationship
            .borrow()
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
            .borrow()
            .get(&relationship.id())
            .map(|relationship| relationship.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`State`] into the store.
    ///
    pub fn inter_state(&mut self, state: Rc<RefCell<State>>) {
        let read = state.borrow();
        self.state
            .borrow_mut()
            .insert(read.id, (state.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`State`] from the store.
    ///
    pub fn exhume_state(&self, id: &Uuid) -> Option<Rc<RefCell<State>>> {
        self.state.borrow().get(id).map(|state| state.0.clone())
    }

    /// Exorcise (remove) [`State`] from the store.
    ///
    pub fn exorcise_state(&mut self, id: &Uuid) -> Option<Rc<RefCell<State>>> {
        self.state
            .borrow_mut()
            .remove(id)
            .map(|state| state.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, State>`.
    ///
    pub fn iter_state(&self) -> impl Iterator<Item = Rc<RefCell<State>>> + '_ {
        let values: Vec<Rc<RefCell<State>>> = self
            .state
            .borrow()
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
            .borrow()
            .get(&state.id)
            .map(|state| state.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Subtype`] into the store.
    ///
    pub fn inter_subtype(&mut self, subtype: Rc<RefCell<Subtype>>) {
        let read = subtype.borrow();
        self.subtype
            .borrow_mut()
            .insert(read.id, (subtype.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Subtype`] from the store.
    ///
    pub fn exhume_subtype(&self, id: &Uuid) -> Option<Rc<RefCell<Subtype>>> {
        self.subtype
            .borrow()
            .get(id)
            .map(|subtype| subtype.0.clone())
    }

    /// Exorcise (remove) [`Subtype`] from the store.
    ///
    pub fn exorcise_subtype(&mut self, id: &Uuid) -> Option<Rc<RefCell<Subtype>>> {
        self.subtype
            .borrow_mut()
            .remove(id)
            .map(|subtype| subtype.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Subtype>`.
    ///
    pub fn iter_subtype(&self) -> impl Iterator<Item = Rc<RefCell<Subtype>>> + '_ {
        let values: Vec<Rc<RefCell<Subtype>>> = self
            .subtype
            .borrow()
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
            .borrow()
            .get(&subtype.id)
            .map(|subtype| subtype.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Supertype`] into the store.
    ///
    pub fn inter_supertype(&mut self, supertype: Rc<RefCell<Supertype>>) {
        let read = supertype.borrow();
        self.supertype
            .borrow_mut()
            .insert(read.id, (supertype.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Supertype`] from the store.
    ///
    pub fn exhume_supertype(&self, id: &Uuid) -> Option<Rc<RefCell<Supertype>>> {
        self.supertype
            .borrow()
            .get(id)
            .map(|supertype| supertype.0.clone())
    }

    /// Exorcise (remove) [`Supertype`] from the store.
    ///
    pub fn exorcise_supertype(&mut self, id: &Uuid) -> Option<Rc<RefCell<Supertype>>> {
        self.supertype
            .borrow_mut()
            .remove(id)
            .map(|supertype| supertype.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Supertype>`.
    ///
    pub fn iter_supertype(&self) -> impl Iterator<Item = Rc<RefCell<Supertype>>> + '_ {
        let values: Vec<Rc<RefCell<Supertype>>> = self
            .supertype
            .borrow()
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
            .borrow()
            .get(&supertype.id)
            .map(|supertype| supertype.1)
            .unwrap_or(SystemTime::now())
    }

    /// Inter (insert) [`Ty`] into the store.
    ///
    pub fn inter_ty(&mut self, ty: Rc<RefCell<Ty>>) {
        let read = ty.borrow();
        self.ty
            .borrow_mut()
            .insert(read.id(), (ty.clone(), SystemTime::now()));
    }

    /// Exhume (get) [`Ty`] from the store.
    ///
    pub fn exhume_ty(&self, id: &Uuid) -> Option<Rc<RefCell<Ty>>> {
        self.ty.borrow().get(id).map(|ty| ty.0.clone())
    }

    /// Exorcise (remove) [`Ty`] from the store.
    ///
    pub fn exorcise_ty(&mut self, id: &Uuid) -> Option<Rc<RefCell<Ty>>> {
        self.ty.borrow_mut().remove(id).map(|ty| ty.0.clone())
    }

    /// Get an iterator over the internal `HashMap<&Uuid, Ty>`.
    ///
    pub fn iter_ty(&self) -> impl Iterator<Item = Rc<RefCell<Ty>>> + '_ {
        let values: Vec<Rc<RefCell<Ty>>> =
            self.ty.borrow().values().map(|ty| ty.0.clone()).collect();
        let len = values.len();
        (0..len).map(move |i| values[i].clone())
    }

    /// Get the timestamp for Ty.
    ///
    pub fn ty_timestamp(&self, ty: &Ty) -> SystemTime {
        self.ty
            .borrow()
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
            for acknowledged_event_tuple in self.acknowledged_event.borrow().values() {
                let path = path.join(format!("{}.json", acknowledged_event_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<AcknowledgedEvent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned()
                        != acknowledged_event_tuple.0.borrow().to_owned()
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
                    if !self.acknowledged_event.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist An Associative Referent.
        {
            let path = path.join("an_associative_referent");
            fs::create_dir_all(&path)?;
            for an_associative_referent_tuple in self.an_associative_referent.borrow().values() {
                let path = path.join(format!(
                    "{}.json",
                    an_associative_referent_tuple.0.borrow().id
                ));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<AnAssociativeReferent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned()
                        != an_associative_referent_tuple.0.borrow().to_owned()
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
                    if !self.an_associative_referent.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Associative.
        {
            let path = path.join("associative");
            fs::create_dir_all(&path)?;
            for associative_tuple in self.associative.borrow().values() {
                let path = path.join(format!("{}.json", associative_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Associative>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != associative_tuple.0.borrow().to_owned() {
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
                    if !self.associative.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Associative Referent.
        {
            let path = path.join("associative_referent");
            fs::create_dir_all(&path)?;
            for associative_referent_tuple in self.associative_referent.borrow().values() {
                let path = path.join(format!("{}.json", associative_referent_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<AssociativeReferent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned()
                        != associative_referent_tuple.0.borrow().to_owned()
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
                    if !self.associative_referent.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Associative Referrer.
        {
            let path = path.join("associative_referrer");
            fs::create_dir_all(&path)?;
            for associative_referrer_tuple in self.associative_referrer.borrow().values() {
                let path = path.join(format!("{}.json", associative_referrer_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<AssociativeReferrer>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned()
                        != associative_referrer_tuple.0.borrow().to_owned()
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
                    if !self.associative_referrer.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Attribute.
        {
            let path = path.join("attribute");
            fs::create_dir_all(&path)?;
            for attribute_tuple in self.attribute.borrow().values() {
                let path = path.join(format!("{}.json", attribute_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Attribute>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != attribute_tuple.0.borrow().to_owned() {
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
                    if !self.attribute.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Binary.
        {
            let path = path.join("binary");
            fs::create_dir_all(&path)?;
            for binary_tuple in self.binary.borrow().values() {
                let path = path.join(format!("{}.json", binary_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Binary>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != binary_tuple.0.borrow().to_owned() {
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
                    if !self.binary.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Cardinality.
        {
            let path = path.join("cardinality");
            fs::create_dir_all(&path)?;
            for cardinality_tuple in self.cardinality.borrow().values() {
                let path = path.join(format!("{}.json", cardinality_tuple.0.borrow().id()));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Cardinality>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != cardinality_tuple.0.borrow().to_owned() {
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
                    if !self.cardinality.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Conditionality.
        {
            let path = path.join("conditionality");
            fs::create_dir_all(&path)?;
            for conditionality_tuple in self.conditionality.borrow().values() {
                let path = path.join(format!("{}.json", conditionality_tuple.0.borrow().id()));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Conditionality>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != conditionality_tuple.0.borrow().to_owned() {
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
                    if !self.conditionality.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Event.
        {
            let path = path.join("event");
            fs::create_dir_all(&path)?;
            for event_tuple in self.event.borrow().values() {
                let path = path.join(format!("{}.json", event_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Event>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != event_tuple.0.borrow().to_owned() {
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
                    if !self.event.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist External.
        {
            let path = path.join("external");
            fs::create_dir_all(&path)?;
            for external_tuple in self.external.borrow().values() {
                let path = path.join(format!("{}.json", external_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<External>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != external_tuple.0.borrow().to_owned() {
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
                    if !self.external.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Isa.
        {
            let path = path.join("isa");
            fs::create_dir_all(&path)?;
            for isa_tuple in self.isa.borrow().values() {
                let path = path.join(format!("{}.json", isa_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Isa>>, SystemTime) = serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != isa_tuple.0.borrow().to_owned() {
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
                    if !self.isa.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Object.
        {
            let path = path.join("object");
            fs::create_dir_all(&path)?;
            for object_tuple in self.object.borrow().values() {
                let path = path.join(format!("{}.json", object_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Object>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != object_tuple.0.borrow().to_owned() {
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
                    if !self.object.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Referent.
        {
            let path = path.join("referent");
            fs::create_dir_all(&path)?;
            for referent_tuple in self.referent.borrow().values() {
                let path = path.join(format!("{}.json", referent_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Referent>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != referent_tuple.0.borrow().to_owned() {
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
                    if !self.referent.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Referrer.
        {
            let path = path.join("referrer");
            fs::create_dir_all(&path)?;
            for referrer_tuple in self.referrer.borrow().values() {
                let path = path.join(format!("{}.json", referrer_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Referrer>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != referrer_tuple.0.borrow().to_owned() {
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
                    if !self.referrer.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Relationship.
        {
            let path = path.join("relationship");
            fs::create_dir_all(&path)?;
            for relationship_tuple in self.relationship.borrow().values() {
                let path = path.join(format!("{}.json", relationship_tuple.0.borrow().id()));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Relationship>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != relationship_tuple.0.borrow().to_owned() {
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
                    if !self.relationship.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist State.
        {
            let path = path.join("state");
            fs::create_dir_all(&path)?;
            for state_tuple in self.state.borrow().values() {
                let path = path.join(format!("{}.json", state_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<State>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != state_tuple.0.borrow().to_owned() {
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
                    if !self.state.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Subtype.
        {
            let path = path.join("subtype");
            fs::create_dir_all(&path)?;
            for subtype_tuple in self.subtype.borrow().values() {
                let path = path.join(format!("{}.json", subtype_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Subtype>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != subtype_tuple.0.borrow().to_owned() {
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
                    if !self.subtype.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Supertype.
        {
            let path = path.join("supertype");
            fs::create_dir_all(&path)?;
            for supertype_tuple in self.supertype.borrow().values() {
                let path = path.join(format!("{}.json", supertype_tuple.0.borrow().id));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Supertype>>, SystemTime) =
                        serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != supertype_tuple.0.borrow().to_owned() {
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
                    if !self.supertype.borrow().contains_key(&id) {
                        fs::remove_file(path)?;
                    }
                }
            }
        }

        // Persist Type.
        {
            let path = path.join("ty");
            fs::create_dir_all(&path)?;
            for ty_tuple in self.ty.borrow().values() {
                let path = path.join(format!("{}.json", ty_tuple.0.borrow().id()));
                if path.exists() {
                    let file = fs::File::open(&path)?;
                    let reader = io::BufReader::new(file);
                    let on_disk: (Rc<RefCell<Ty>>, SystemTime) = serde_json::from_reader(reader)?;
                    if on_disk.0.borrow().to_owned() != ty_tuple.0.borrow().to_owned() {
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
                    if !self.ty.borrow().contains_key(&id) {
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
                let acknowledged_event: (Rc<RefCell<AcknowledgedEvent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .acknowledged_event
                    .borrow_mut()
                    .insert(acknowledged_event.0.borrow().id, acknowledged_event.clone());
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
                let an_associative_referent: (Rc<RefCell<AnAssociativeReferent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.an_associative_referent.borrow_mut().insert(
                    an_associative_referent.0.borrow().id,
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
                let associative: (Rc<RefCell<Associative>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .associative
                    .borrow_mut()
                    .insert(associative.0.borrow().id, associative.clone());
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
                let associative_referent: (Rc<RefCell<AssociativeReferent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.associative_referent.borrow_mut().insert(
                    associative_referent.0.borrow().id,
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
                let associative_referrer: (Rc<RefCell<AssociativeReferrer>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store.associative_referrer.borrow_mut().insert(
                    associative_referrer.0.borrow().id,
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
                let attribute: (Rc<RefCell<Attribute>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .attribute
                    .borrow_mut()
                    .insert(attribute.0.borrow().id, attribute.clone());
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
                let binary: (Rc<RefCell<Binary>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .binary
                    .borrow_mut()
                    .insert(binary.0.borrow().id, binary.clone());
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
                let cardinality: (Rc<RefCell<Cardinality>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .cardinality
                    .borrow_mut()
                    .insert(cardinality.0.borrow().id(), cardinality.clone());
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
                let conditionality: (Rc<RefCell<Conditionality>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .conditionality
                    .borrow_mut()
                    .insert(conditionality.0.borrow().id(), conditionality.clone());
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
                let event: (Rc<RefCell<Event>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .event
                    .borrow_mut()
                    .insert(event.0.borrow().id, event.clone());
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
                let external: (Rc<RefCell<External>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .external
                    .borrow_mut()
                    .insert(external.0.borrow().id, external.clone());
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
                let isa: (Rc<RefCell<Isa>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .isa
                    .borrow_mut()
                    .insert(isa.0.borrow().id, isa.clone());
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
                let object: (Rc<RefCell<Object>>, SystemTime) = serde_json::from_reader(reader)?;
                store.object_id_by_name.borrow_mut().insert(
                    object.0.borrow().name.to_upper_camel_case(),
                    (object.0.borrow().id, object.1),
                );
                store
                    .object
                    .borrow_mut()
                    .insert(object.0.borrow().id, object.clone());
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
                let referent: (Rc<RefCell<Referent>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .referent
                    .borrow_mut()
                    .insert(referent.0.borrow().id, referent.clone());
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
                let referrer: (Rc<RefCell<Referrer>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .referrer
                    .borrow_mut()
                    .insert(referrer.0.borrow().id, referrer.clone());
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
                let relationship: (Rc<RefCell<Relationship>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .relationship
                    .borrow_mut()
                    .insert(relationship.0.borrow().id(), relationship.clone());
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
                let state: (Rc<RefCell<State>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .state
                    .borrow_mut()
                    .insert(state.0.borrow().id, state.clone());
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
                let subtype: (Rc<RefCell<Subtype>>, SystemTime) = serde_json::from_reader(reader)?;
                store
                    .subtype
                    .borrow_mut()
                    .insert(subtype.0.borrow().id, subtype.clone());
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
                let supertype: (Rc<RefCell<Supertype>>, SystemTime) =
                    serde_json::from_reader(reader)?;
                store
                    .supertype
                    .borrow_mut()
                    .insert(supertype.0.borrow().id, supertype.clone());
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
                let ty: (Rc<RefCell<Ty>>, SystemTime) = serde_json::from_reader(reader)?;
                store.ty.borrow_mut().insert(ty.0.borrow().id(), ty.clone());
            }
        }

        Ok(store)
    }
    // {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
// {"magic":"","directive":{"End":{"directive":"ignore-orig"}}}
