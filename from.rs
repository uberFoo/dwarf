//! v2::sarzak Object From Trait Implementations
//!
//! These are [`From`] trait implementations for the domain: _sarzak_. They are
//! generated to be used during the extrusion process. This is the process
//! by which instances of one domain are transformed into instances of another.
//! In this case the source domain is `v1::sarzak`.
//!
//! It is hoped that the model has not changed enough to render
//! these implementations useless. In any case it's expected that
//! the generated code will need to be manually edited.
// {"magic":"","directive":{"Start":{"directive":"ignore-gen","tag":"v2::sarzak-from-impl-file"}}}
// {"magic":"","directive":{"Start":{"directive":"ignore-gen","tag":"v2::sarzak-from-impl-definition"}}}
use uuid::Uuid;

use crate::sarzak::types::{
    AcknowledgedEvent, AnAssociativeReferent, Associative, AssociativeReferent,
    AssociativeReferrer, Attribute, Binary, Cardinality, Conditionality, Event, External, Isa,
    Object, Referent, Referrer, Relationship, State, Subtype, Supertype, Ty, BOOLEAN, CONDITIONAL,
    FLOAT, INTEGER, MANY, ONE, S_STRING, S_UUID, UNCONDITIONAL,
};
use crate:sarzak::ObjectStore;

use crate::v1::sarzak::types::{
    AcknowledgedEvent as FromAcknowledgedEvent, Associative as FromAssociative,
    AssociativeReferent as FromAssociativeReferent, AssociativeReferrer as FromAssociativeReferrer,
    Attribute as FromAttribute, Binary as FromBinary, Cardinality as FromCardinality,
    Conditionality as FromConditionality, Event as FromEvent, External as FromExternal,
    Isa as FromIsa, Object as FromObject, Referent as FromReferent, Referrer as FromReferrer,
    Relationship as FromRelationship, State as FromState, Subtype as FromSubtype,
    Supertype as FromSupertype, Type as FromTy, BOOLEAN as FROM_BOOLEAN,
    CONDITIONAL as FROM_CONDITIONAL, FLOAT as FROM_FLOAT, INTEGER as FROM_INTEGER,
    MANY as FROM_MANY, ONE as FROM_ONE, STRING as FROM_STRING, UNCONDITIONAL as FROM_UNCONDITIONAL,
    UUID as FROM_UUID,
};
use crate::v1::sarzak::ObjectStore as SarzakStore;

impl From<&SarzakStore> for ObjectStore {
    fn from(from: &SarzakStore) -> Self {
        let mut to = ObjectStore::new();

        for (_, instance) in from.iter_acknowledged_event() {
            let instance = AcknowledgedEvent::from(instance);
            to.inter_acknowledged_event(instance);
        }

        // The order of the next two is important. We need the referents in the
        // store before the associative in order to create the AnAssociativeReferent
        // instances.
        for (_, instance) in from.iter_associative_referent() {
            let instance = AssociativeReferent::from(instance);
            to.inter_associative_referent(instance);
        }

        for (_, instance) in from.iter_associative() {
            let instance = Associative::from((instance, from, &mut to));
            to.inter_associative(instance);
        }

        for (_, instance) in from.iter_associative_referrer() {
            let instance = AssociativeReferrer::from(instance);
            to.inter_associative_referrer(instance);
        }

        for (_, instance) in from.iter_attribute() {
            let instance = Attribute::from(instance);
            to.inter_attribute(instance);
        }

        for (_, instance) in from.iter_binary() {
            let instance = Binary::from(instance);
            to.inter_binary(instance);
        }

        for (_, instance) in from.iter_cardinality() {
            let instance = Cardinality::from(instance);
            to.inter_cardinality(instance);
        }

        for (_, instance) in from.iter_conditionality() {
            let instance = Conditionality::from(instance);
            to.inter_conditionality(instance);
        }

        for (_, instance) in from.iter_event() {
            let instance = Event::from(instance);
            to.inter_event(instance);
        }

        for (_, instance) in from.iter_external() {
            let instance = External::from(instance);
            to.inter_external(instance);
        }

        for (_, instance) in from.iter_isa() {
            let instance = Isa::from(instance);
            to.inter_isa(instance);
        }

        for (_, instance) in from.iter_object() {
            let instance = Object::from(instance);
            to.inter_object(instance);
        }

        for (_, instance) in from.iter_referent() {
            let instance = Referent::from(instance);
            to.inter_referent(instance);
        }

        for (_, instance) in from.iter_referrer() {
            let instance = Referrer::from(instance);
            to.inter_referrer(instance);
        }

        for (_, instance) in from.iter_relationship() {
            let instance = Relationship::from(instance);
            to.inter_relationship(instance);
        }

        for (_, instance) in from.iter_state() {
            let instance = State::from(instance);
            to.inter_state(instance);
        }

        for (_, instance) in from.iter_subtype() {
            let instance = Subtype::from(instance);
            to.inter_subtype(instance);
        }

        for (_, instance) in from.iter_supertype() {
            let instance = Supertype::from(instance);
            to.inter_supertype(instance);
        }

        for (_, instance) in from.iter_ty() {
            let instance = Ty::from(instance);
            to.inter_ty(instance);
        }

        to
    }
}

impl From<&FromAcknowledgedEvent> for AcknowledgedEvent {
    fn from(src: &FromAcknowledgedEvent) -> Self {
        Self {
            id: src.id,
            event_id: src.event_id,
            state_id: src.state_id,
        }
    }
}

impl From<(&FromAssociative, &SarzakStore, &mut ObjectStore)> for Associative {
    fn from((src, sarzak, store): (&FromAssociative, &SarzakStore, &mut ObjectStore)) -> Self {
        let this = Self {
            id: src.id,
            number: src.number,
            from: src.from,
        };

        let referrer = sarzak.exhume_associative_referrer(&src.from).unwrap();

        // Create instances of the associative object
        let one = store.exhume_associative_referent(&src.one).unwrap().clone();
        let other = store
            .exhume_associative_referent(&src.other)
            .unwrap()
            .clone();

        let _ = AnAssociativeReferent::new(
            referrer.one_referential_attribute.clone(),
            &this,
            &one,
            store,
        );
        let _ = AnAssociativeReferent::new(
            referrer.other_referential_attribute.clone(),
            &this,
            &other,
            store,
        );

        this
    }
}

impl From<&FromAssociativeReferent> for AssociativeReferent {
    fn from(src: &FromAssociativeReferent) -> Self {
        Self {
            id: src.id,
            obj_id: src.obj_id,
            cardinality: from_const(&src.cardinality),
            conditionality: from_const(&src.conditionality),
            description: src.description.clone(),
        }
    }
}

impl From<&FromAssociativeReferrer> for AssociativeReferrer {
    fn from(src: &FromAssociativeReferrer) -> Self {
        Self {
            id: src.id,
            obj_id: src.obj_id,
            cardinality: from_const(&src.cardinality),
        }
    }
}

impl From<&FromAttribute> for Attribute {
    fn from(src: &FromAttribute) -> Self {
        Self {
            id: src.id,
            name: src.name.clone(),
            obj_id: src.obj_id.unwrap(),
            ty: from_const(&src.ty),
        }
    }
}

impl From<&FromBinary> for Binary {
    fn from(src: &FromBinary) -> Self {
        Self {
            id: src.id,
            number: src.number,
            to: src.to,
            from: src.from,
        }
    }
}

impl From<&FromCardinality> for Cardinality {
    fn from(src: &FromCardinality) -> Self {
        match src {
            FromCardinality::Many(_) => Cardinality::Many(MANY),
            FromCardinality::One(_) => Cardinality::One(ONE),
        }
    }
}

impl From<&FromConditionality> for Conditionality {
    fn from(src: &FromConditionality) -> Self {
        match src {
            FromConditionality::Conditional(_) => Conditionality::Conditional(CONDITIONAL),
            FromConditionality::Unconditional(_) => Conditionality::Unconditional(UNCONDITIONAL),
        }
    }
}
impl From<&FromEvent> for Event {
    fn from(src: &FromEvent) -> Self {
        Self {
            id: src.id,
            name: src.name.clone(),
            obj_id: src.obj_id,
        }
    }
}

impl From<&FromExternal> for External {
    fn from(src: &FromExternal) -> Self {
        Self {
            id: src.id,
            ctor: "new".to_owned(),
            name: src.name.clone(),
            path: src.path.clone(),
        }
    }
}

impl From<&FromIsa> for Isa {
    fn from(src: &FromIsa) -> Self {
        Self {
            id: src.id,
            number: src.number,
            supertype: src.supertype,
        }
    }
}

impl From<&FromObject> for Object {
    fn from(src: &FromObject) -> Self {
        Self {
            description: src.description.clone(),
            id: src.id,
            key_letters: src.key_letters.clone(),
            name: src.name.clone(),
        }
    }
}

impl From<&FromReferent> for Referent {
    fn from(src: &FromReferent) -> Self {
        Self {
            description: src.description.clone(),
            id: src.id,
            cardinality: from_const(&src.cardinality),
            conditionality: from_const(&src.conditionality),
            obj_id: src.obj_id,
        }
    }
}

impl From<&FromReferrer> for Referrer {
    fn from(src: &FromReferrer) -> Self {
        Self {
            description: src.description.clone(),
            id: src.id,
            referential_attribute: src.referential_attribute.clone(),
            cardinality: from_const(&src.cardinality),
            conditionality: from_const(&src.conditionality),
            obj_id: src.obj_id,
        }
    }
}

impl From<&FromRelationship> for Relationship {
    fn from(src: &FromRelationship) -> Self {
        match src {
            FromRelationship::Associative(src) => Relationship::Associative(src.clone()),
            FromRelationship::Binary(src) => Relationship::Binary(src.clone()),
            FromRelationship::Isa(src) => Relationship::Isa(src.clone()),
        }
    }
}
impl From<&FromState> for State {
    fn from(src: &FromState) -> Self {
        Self {
            id: src.id,
            name: src.name.clone(),
            obj_id: src.obj_id,
        }
    }
}

impl From<&FromSubtype> for Subtype {
    fn from(src: &FromSubtype) -> Self {
        Self {
            id: src.id,
            isa: src.isa,
            obj_id: src.obj_id,
        }
    }
}

impl From<&FromSupertype> for Supertype {
    fn from(src: &FromSupertype) -> Self {
        Self {
            id: src.id,
            obj_id: src.obj_id,
        }
    }
}

impl From<&FromTy> for Ty {
    fn from(src: &FromTy) -> Self {
        match src {
            FromTy::Boolean(_) => Ty::Boolean(BOOLEAN),
            FromTy::External(src) => Ty::External(src.clone()),
            FromTy::Float(_) => Ty::Float(FLOAT),
            FromTy::Integer(_) => Ty::Integer(INTEGER),
            FromTy::Object(src) => Ty::Object(src.clone()),
            FromTy::String(_) => Ty::SString(S_STRING),
            FromTy::Uuid(_) => Ty::SUuid(S_UUID),
        }
    }
}

fn from_const(from: &Uuid) -> Uuid {
    match *from {
        FROM_BOOLEAN => BOOLEAN,
        FROM_FLOAT => FLOAT,
        FROM_INTEGER => INTEGER,
        FROM_STRING => S_STRING,
        FROM_UUID => S_UUID,
        FROM_CONDITIONAL => CONDITIONAL,
        FROM_UNCONDITIONAL => UNCONDITIONAL,
        FROM_MANY => MANY,
        FROM_ONE => ONE,
        huh => panic!("from_const: unexpected value {}", huh),
    }
}
// {"magic":"","directive":{"End":{"directive":"ignore-gen"}}}
// {"magic":"","directive":{"End":{"directive":"ignore-gen"}}}
