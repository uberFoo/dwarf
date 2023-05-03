//! This is the second iteration of the drawing domain. The first sucked.
//!
//! This domain represents the visual aspect of a model.
use std::{
    any::Any,
    collections::VecDeque,
    fmt,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use derivative::Derivative;
use lazy_static::lazy_static;
use sarzak::{
    lu_dog::{Empty, List, ObjectStore as LuDogStore, ValueType},
    sarzak::SUuid,
};
use uuid::{uuid, Uuid};

use sarzak::merlin::{
    Anchor, Bisection, Edge, Glyph, Line, LineSegment, LineSegmentPoint,
    ObjectStore as MerlinStore, Point, RelationshipName, RelationshipPhrase, XBox,
};

use crate::{ChaChaError, Result, StoreProxy, Value};

lazy_static! {
    static ref MODEL: Arc<RwLock<MerlinStore>> = Arc::new(RwLock::new(
        MerlinStore::load("/Users/uberfoo/projects/sarzak/sarzak/models/lu_dog.v2.json").unwrap()
    ));
}

use crate::woog_structs::ANCHOR_TYPE_UUID;
const ANCHOR_STORE_TYPE_UUID: Uuid = uuid!("27edcc78-f257-5a0b-a2e4-c233987e0889");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct AnchorProxy {
    self_: Option<Arc<RwLock<Anchor>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl AnchorProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&ANCHOR_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for AnchorProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Anchor"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        ANCHOR_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        ANCHOR_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let offset = args.pop_front().unwrap().try_into()?;
                    let x_offset = args.pop_front().unwrap().try_into()?;
                    let y_offset = args.pop_front().unwrap().try_into()?;
                    let edge: EdgeProxy = args.pop_front().unwrap().try_into()?;
                    let edge = edge.self_.unwrap();
                    let glyph: GlyphProxy = args.pop_front().unwrap().try_into()?;
                    let glyph = glyph.self_.unwrap();
                    let x_box: XBoxProxy = args.pop_front().unwrap().try_into()?;
                    let x_box = x_box.self_.unwrap();
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let anchor = Anchor::new(
                        offset, x_offset, y_offset, &edge, &glyph, &x_box, &line, &mut model,
                    );

                    let mut anchor_proxy = self.clone();
                    anchor_proxy.self_ = Some(anchor);

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(anchor_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_anchor()
                        .map(|anchor| {
                            let mut anchor_proxy = self.clone();
                            anchor_proxy.self_ = Some(anchor);
                            Value::ProxyType(Arc::new(RwLock::new(anchor_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "offset" => Ok(Value::Float(self_.read().unwrap().offset)),
                "x_offset" => Ok(Value::Integer(self_.read().unwrap().x_offset)),
                "y_offset" => Ok(Value::Integer(self_.read().unwrap().y_offset)),
                "edge" => {
                    let edge = MODEL
                        .read()
                        .unwrap()
                        .exhume_anchor(&self_.read().unwrap().edge)
                        .unwrap();

                    Ok((edge, self.lu_dog.clone()).into())
                }
                "glyph" => {
                    let glyph = MODEL
                        .read()
                        .unwrap()
                        .exhume_anchor(&self_.read().unwrap().glyph)
                        .unwrap();

                    Ok((glyph, self.lu_dog.clone()).into())
                }
                "x_box" => {
                    let x_box = MODEL
                        .read()
                        .unwrap()
                        .exhume_anchor(&self_.read().unwrap().x_box)
                        .unwrap();

                    Ok((x_box, self.lu_dog.clone()).into())
                }
                "line" => {
                    let line = MODEL
                        .read()
                        .unwrap()
                        .exhume_anchor(&self_.read().unwrap().line)
                        .unwrap();

                    Ok((line, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for AnchorProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "AnchorProxy({})", self_.read().unwrap().id)
        } else {
            write!(
                f,
                "{} AnchorProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Anchor>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((anchor, store): (Arc<RwLock<Anchor>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(AnchorProxy {
            self_: Some(anchor),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&ANCHOR_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for AnchorProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <AnchorProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == ANCHOR_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<AnchorProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "AnchorProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "AnchorProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::BISECTION_TYPE_UUID;
const BISECTION_STORE_TYPE_UUID: Uuid = uuid!("f6496c3c-adfa-5cf5-80b3-21bf2f0d7040");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BisectionProxy {
    self_: Option<Arc<RwLock<Bisection>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl BisectionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&BISECTION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for BisectionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Bisection"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        BISECTION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        BISECTION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let offset = args.pop_front().unwrap().try_into()?;
                    let segment: LineSegmentProxy = args.pop_front().unwrap().try_into()?;
                    let segment = segment.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let bisection = Bisection::new(offset, &segment, &mut model);

                    let mut bisection_proxy = self.clone();
                    bisection_proxy.self_ = Some(bisection);

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(bisection_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_bisection()
                        .map(|bisection| {
                            let mut bisection_proxy = self.clone();
                            bisection_proxy.self_ = Some(bisection);
                            Value::ProxyType(Arc::new(RwLock::new(bisection_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "offset" => Ok(Value::Float(self_.read().unwrap().offset)),
                "segment" => {
                    let segment = MODEL
                        .read()
                        .unwrap()
                        .exhume_bisection(&self_.read().unwrap().segment)
                        .unwrap();

                    Ok((segment, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for BisectionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "BisectionProxy({})", self_.read().unwrap().id)
        } else {
            write!(
                f,
                "{} BisectionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Bisection>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((bisection, store): (Arc<RwLock<Bisection>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(BisectionProxy {
            self_: Some(bisection),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&BISECTION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for BisectionProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <BisectionProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == BISECTION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<BisectionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "BisectionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "BisectionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::X_BOX_TYPE_UUID;
const X_BOX_STORE_TYPE_UUID: Uuid = uuid!("a27db16f-fea8-5db0-9e1e-30b12486bb75");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct XBoxProxy {
    self_: Option<Arc<RwLock<XBox>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl XBoxProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&X_BOX_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for XBoxProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "XBox"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        X_BOX_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        X_BOX_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_x_box()
                        .map(|x_box| {
                            let mut x_box_proxy = self.clone();
                            x_box_proxy.self_ = Some(x_box);
                            Value::ProxyType(Arc::new(RwLock::new(x_box_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "height" => Ok(Value::Integer(self_.read().unwrap().height)),
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "width" => Ok(Value::Integer(self_.read().unwrap().width)),
                "x" => Ok(Value::Integer(self_.read().unwrap().x)),
                "y" => Ok(Value::Integer(self_.read().unwrap().y)),
                "object" => {
                    let object = MODEL
                        .read()
                        .unwrap()
                        .exhume_x_box(&self_.read().unwrap().object)
                        .unwrap();

                    Ok((object, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for XBoxProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "XBoxProxy({})", self_.read().unwrap().id)
        } else {
            write!(f, "{} XBoxProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<XBox>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((x_box, store): (Arc<RwLock<XBox>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(XBoxProxy {
            self_: Some(x_box),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&X_BOX_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for XBoxProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <XBoxProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == X_BOX_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<XBoxProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "XBoxProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "XBoxProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::EDGE_TYPE_UUID;
const EDGE_STORE_TYPE_UUID: Uuid = uuid!("d01f2378-3539-5b5f-ad97-0d0558f7d40e");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct EdgeProxy {
    self_: Option<Arc<RwLock<Edge>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl EdgeProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&EDGE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for EdgeProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Edge"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        EDGE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        EDGE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id()),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_bottom" => {
                    let mut model = MODEL.write().unwrap();
                    let mut bottom_proxy = self.clone();
                    bottom_proxy.self_ = Some(Edge::new_bottom(&mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(bottom_proxy))),
                        self.type_.clone(),
                    ))
                }
                "new_left" => {
                    let mut model = MODEL.write().unwrap();
                    let mut left_proxy = self.clone();
                    left_proxy.self_ = Some(Edge::new_left(&mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(left_proxy))),
                        self.type_.clone(),
                    ))
                }
                "new_right" => {
                    let mut model = MODEL.write().unwrap();
                    let mut right_proxy = self.clone();
                    right_proxy.self_ = Some(Edge::new_right(&mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(right_proxy))),
                        self.type_.clone(),
                    ))
                }
                "new_top" => {
                    let mut model = MODEL.write().unwrap();
                    let mut top_proxy = self.clone();
                    top_proxy.self_ = Some(Edge::new_top(&mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(top_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_edge()
                        .map(|edge| {
                            let mut edge_proxy = self.clone();
                            edge_proxy.self_ = Some(edge);
                            Value::ProxyType(Arc::new(RwLock::new(edge_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id())),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for EdgeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "EdgeProxy({})", self_.read().unwrap().id())
        } else {
            write!(f, "{} EdgeProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Edge>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((edge, store): (Arc<RwLock<Edge>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_edge = edge.read().unwrap();

        match *read_edge {
            Edge::Bottom(_) => {
                let mut edge_proxy = EdgeProxy::new_type(store.clone());
                edge_proxy.self_ = Some(edge.clone());
                Value::ProxyType(Arc::new(RwLock::new(edge_proxy)))
            }
            Edge::Left(_) => {
                let mut edge_proxy = EdgeProxy::new_type(store.clone());
                edge_proxy.self_ = Some(edge.clone());
                Value::ProxyType(Arc::new(RwLock::new(edge_proxy)))
            }
            Edge::Right(_) => {
                let mut edge_proxy = EdgeProxy::new_type(store.clone());
                edge_proxy.self_ = Some(edge.clone());
                Value::ProxyType(Arc::new(RwLock::new(edge_proxy)))
            }
            Edge::Top(_) => {
                let mut edge_proxy = EdgeProxy::new_type(store.clone());
                edge_proxy.self_ = Some(edge.clone());
                Value::ProxyType(Arc::new(RwLock::new(edge_proxy)))
            }
        }
    }
}

impl TryFrom<Value> for EdgeProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <EdgeProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == EDGE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<EdgeProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "EdgeProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "EdgeProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::GLYPH_TYPE_UUID;
const GLYPH_STORE_TYPE_UUID: Uuid = uuid!("47ccc17a-dde2-54b8-8d70-33b8aa683b36");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct GlyphProxy {
    self_: Option<Arc<RwLock<Glyph>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl GlyphProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&GLYPH_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for GlyphProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Glyph"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        GLYPH_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        GLYPH_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_many" => {
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut many_proxy = self.clone();
                    many_proxy.self_ = Some(Glyph::new_many(&line, &mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(many_proxy))),
                        self.type_.clone(),
                    ))
                }
                "new_one" => {
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut one_proxy = self.clone();
                    one_proxy.self_ = Some(Glyph::new_one(&line, &mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(one_proxy))),
                        self.type_.clone(),
                    ))
                }
                "new_sub" => {
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut sub_proxy = self.clone();
                    sub_proxy.self_ = Some(Glyph::new_sub(&line, &mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(sub_proxy))),
                        self.type_.clone(),
                    ))
                }
                "new_x_super" => {
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut x_super_proxy = self.clone();
                    x_super_proxy.self_ = Some(Glyph::new_x_super(&line, &mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(x_super_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_glyph()
                        .map(|glyph| {
                            let mut glyph_proxy = self.clone();
                            glyph_proxy.self_ = Some(glyph);
                            Value::ProxyType(Arc::new(RwLock::new(glyph_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "line" => {
                    let line = MODEL
                        .read()
                        .unwrap()
                        .exhume_glyph(&self_.read().unwrap().line)
                        .unwrap();

                    Ok((line, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for GlyphProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "GlyphProxy({})", self_.read().unwrap().id)
        } else {
            write!(f, "{} GlyphProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Glyph>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((glyph, store): (Arc<RwLock<Glyph>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(GlyphProxy {
            self_: Some(glyph),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&GLYPH_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for GlyphProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <GlyphProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == GLYPH_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<GlyphProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "GlyphProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "GlyphProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LINE_TYPE_UUID;
const LINE_STORE_TYPE_UUID: Uuid = uuid!("c8778dc8-ae80-5211-99f3-48982bce758e");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct LineProxy {
    self_: Option<Arc<RwLock<Line>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl LineProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LINE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for LineProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Line"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LINE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LINE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_line()
                        .map(|line| {
                            let mut line_proxy = self.clone();
                            line_proxy.self_ = Some(line);
                            Value::ProxyType(Arc::new(RwLock::new(line_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "relationship" => {
                    let relationship = MODEL
                        .read()
                        .unwrap()
                        .exhume_line(&self_.read().unwrap().relationship)
                        .unwrap();

                    Ok((relationship, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for LineProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "LineProxy({})", self_.read().unwrap().id)
        } else {
            write!(f, "{} LineProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Line>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((line, store): (Arc<RwLock<Line>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(LineProxy {
            self_: Some(line),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LINE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for LineProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <LineProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LINE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<LineProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "LineProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "LineProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LINE_SEGMENT_TYPE_UUID;
const LINE_SEGMENT_STORE_TYPE_UUID: Uuid = uuid!("f09d5cf8-9778-5b41-a50c-87e8670e93dd");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct LineSegmentProxy {
    self_: Option<Arc<RwLock<LineSegment>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl LineSegmentProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LINE_SEGMENT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for LineSegmentProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "LineSegment"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LINE_SEGMENT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LINE_SEGMENT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let line_segment = LineSegment::new(&line, &mut model);

                    let mut line_segment_proxy = self.clone();
                    line_segment_proxy.self_ = Some(line_segment);

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(line_segment_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_line_segment()
                        .map(|line_segment| {
                            let mut line_segment_proxy = self.clone();
                            line_segment_proxy.self_ = Some(line_segment);
                            Value::ProxyType(Arc::new(RwLock::new(line_segment_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "line" => {
                    let line = MODEL
                        .read()
                        .unwrap()
                        .exhume_line_segment(&self_.read().unwrap().line)
                        .unwrap();

                    Ok((line, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for LineSegmentProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "LineSegmentProxy({})", self_.read().unwrap().id)
        } else {
            write!(
                f,
                "{} LineSegmentProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<LineSegment>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((line_segment, store): (Arc<RwLock<LineSegment>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(LineSegmentProxy {
            self_: Some(line_segment),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LINE_SEGMENT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for LineSegmentProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <LineSegmentProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LINE_SEGMENT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<LineSegmentProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "LineSegmentProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "LineSegmentProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LINE_SEGMENT_POINT_TYPE_UUID;
const LINE_SEGMENT_POINT_STORE_TYPE_UUID: Uuid = uuid!("49615ec3-09d3-54d9-b32c-ebd5741f7af8");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct LineSegmentPointProxy {
    self_: Option<Arc<RwLock<LineSegmentPoint>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl LineSegmentPointProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LINE_SEGMENT_POINT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for LineSegmentPointProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "LineSegmentPoint"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LINE_SEGMENT_POINT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LINE_SEGMENT_POINT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let segment: LineSegmentProxy = args.pop_front().unwrap().try_into()?;
                    let segment = segment.self_.unwrap();
                    let point: PointProxy = args.pop_front().unwrap().try_into()?;
                    let point = point.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let line_segment_point = LineSegmentPoint::new(&segment, &point, &mut model);

                    let mut line_segment_point_proxy = self.clone();
                    line_segment_point_proxy.self_ = Some(line_segment_point);

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(line_segment_point_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_line_segment_point()
                        .map(|line_segment_point| {
                            let mut line_segment_point_proxy = self.clone();
                            line_segment_point_proxy.self_ = Some(line_segment_point);
                            Value::ProxyType(Arc::new(RwLock::new(line_segment_point_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "segment" => {
                    let segment = MODEL
                        .read()
                        .unwrap()
                        .exhume_line_segment_point(&self_.read().unwrap().segment)
                        .unwrap();

                    Ok((segment, self.lu_dog.clone()).into())
                }
                "point" => {
                    let point = MODEL
                        .read()
                        .unwrap()
                        .exhume_line_segment_point(&self_.read().unwrap().point)
                        .unwrap();

                    Ok((point, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for LineSegmentPointProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "LineSegmentPointProxy({})", self_.read().unwrap().id)
        } else {
            write!(
                f,
                "{} LineSegmentPointProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<LineSegmentPoint>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (line_segment_point, store): (Arc<RwLock<LineSegmentPoint>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(LineSegmentPointProxy {
            self_: Some(line_segment_point),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LINE_SEGMENT_POINT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for LineSegmentPointProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <LineSegmentPointProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LINE_SEGMENT_POINT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<LineSegmentPointProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "LineSegmentPointProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "LineSegmentPointProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::POINT_TYPE_UUID;
const POINT_STORE_TYPE_UUID: Uuid = uuid!("423935ca-86d2-5d0a-ad35-8e7f00663448");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct PointProxy {
    self_: Option<Arc<RwLock<Point>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl PointProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&POINT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for PointProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Point"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        POINT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        POINT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_inflection" => {
                    let x = args.pop_front().unwrap().try_into()?;
                    let y = args.pop_front().unwrap().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let mut inflection_proxy = self.clone();
                    inflection_proxy.self_ = Some(Point::new_inflection(x, y, &mut model));

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(inflection_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_point()
                        .map(|point| {
                            let mut point_proxy = self.clone();
                            point_proxy.self_ = Some(point);
                            Value::ProxyType(Arc::new(RwLock::new(point_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "x" => Ok(Value::Integer(self_.read().unwrap().x)),
                "y" => Ok(Value::Integer(self_.read().unwrap().y)),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for PointProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "PointProxy({})", self_.read().unwrap().id)
        } else {
            write!(f, "{} PointProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Point>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((point, store): (Arc<RwLock<Point>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(PointProxy {
            self_: Some(point),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&POINT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for PointProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <PointProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == POINT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<PointProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "PointProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "PointProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::RELATIONSHIP_NAME_TYPE_UUID;
const RELATIONSHIP_NAME_STORE_TYPE_UUID: Uuid = uuid!("a6cad864-7edb-5a36-a3d9-c43df43fd140");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct RelationshipNameProxy {
    self_: Option<Arc<RwLock<RelationshipName>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl RelationshipNameProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&RELATIONSHIP_NAME_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for RelationshipNameProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "RelationshipName"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        RELATIONSHIP_NAME_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        RELATIONSHIP_NAME_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let text = args.pop_front().unwrap().try_into()?;
                    let x = args.pop_front().unwrap().try_into()?;
                    let y = args.pop_front().unwrap().try_into()?;
                    let origin: BisectionProxy = args.pop_front().unwrap().try_into()?;
                    let origin = origin.self_.unwrap();
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let relationship_name =
                        RelationshipName::new(text, x, y, &origin, &line, &mut model);

                    let mut relationship_name_proxy = self.clone();
                    relationship_name_proxy.self_ = Some(relationship_name);

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(relationship_name_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_relationship_name()
                        .map(|relationship_name| {
                            let mut relationship_name_proxy = self.clone();
                            relationship_name_proxy.self_ = Some(relationship_name);
                            Value::ProxyType(Arc::new(RwLock::new(relationship_name_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "text" => Ok(Value::String(self_.read().unwrap().text.clone())),
                "x" => Ok(Value::Integer(self_.read().unwrap().x)),
                "y" => Ok(Value::Integer(self_.read().unwrap().y)),
                "origin" => {
                    let origin = MODEL
                        .read()
                        .unwrap()
                        .exhume_relationship_name(&self_.read().unwrap().origin)
                        .unwrap();

                    Ok((origin, self.lu_dog.clone()).into())
                }
                "line" => {
                    let line = MODEL
                        .read()
                        .unwrap()
                        .exhume_relationship_name(&self_.read().unwrap().line)
                        .unwrap();

                    Ok((line, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for RelationshipNameProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "RelationshipNameProxy({})", self_.read().unwrap().id)
        } else {
            write!(
                f,
                "{} RelationshipNameProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<RelationshipName>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (relationship_name, store): (Arc<RwLock<RelationshipName>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(RelationshipNameProxy {
            self_: Some(relationship_name),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&RELATIONSHIP_NAME_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for RelationshipNameProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <RelationshipNameProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == RELATIONSHIP_NAME_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any.downcast_ref::<RelationshipNameProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "RelationshipNameProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "RelationshipNameProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::RELATIONSHIP_PHRASE_TYPE_UUID;
const RELATIONSHIP_PHRASE_STORE_TYPE_UUID: Uuid = uuid!("ba4b2db0-a361-5e9b-a3d4-1aab7ebe55b0");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct RelationshipPhraseProxy {
    self_: Option<Arc<RwLock<RelationshipPhrase>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl RelationshipPhraseProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&RELATIONSHIP_PHRASE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for RelationshipPhraseProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "RelationshipPhrase"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn into_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        RELATIONSHIP_PHRASE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        RELATIONSHIP_PHRASE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        mut args: VecDeque<Value>,
    ) -> Result<(Value, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.read().unwrap().id),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let text = args.pop_front().unwrap().try_into()?;
                    let x = args.pop_front().unwrap().try_into()?;
                    let y = args.pop_front().unwrap().try_into()?;
                    let origin: AnchorProxy = args.pop_front().unwrap().try_into()?;
                    let origin = origin.self_.unwrap();
                    let line: LineProxy = args.pop_front().unwrap().try_into()?;
                    let line = line.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let relationship_phrase =
                        RelationshipPhrase::new(text, x, y, &origin, &line, &mut model);

                    let mut relationship_phrase_proxy = self.clone();
                    relationship_phrase_proxy.self_ = Some(relationship_phrase);

                    Ok((
                        Value::ProxyType(Arc::new(RwLock::new(relationship_phrase_proxy))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_relationship_phrase()
                        .map(|relationship_phrase| {
                            let mut relationship_phrase_proxy = self.clone();
                            relationship_phrase_proxy.self_ = Some(relationship_phrase);
                            Value::ProxyType(Arc::new(RwLock::new(relationship_phrase_proxy)))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Value::Vector(instances), ty))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Value> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Value::Uuid(self_.read().unwrap().id)),
                "text" => Ok(Value::String(self_.read().unwrap().text.clone())),
                "x" => Ok(Value::Integer(self_.read().unwrap().x)),
                "y" => Ok(Value::Integer(self_.read().unwrap().y)),
                "origin" => {
                    let origin = MODEL
                        .read()
                        .unwrap()
                        .exhume_relationship_phrase(&self_.read().unwrap().origin)
                        .unwrap();

                    Ok((origin, self.lu_dog.clone()).into())
                }
                "line" => {
                    let line = MODEL
                        .read()
                        .unwrap()
                        .exhume_relationship_phrase(&self_.read().unwrap().line)
                        .unwrap();

                    Ok((line, self.lu_dog.clone()).into())
                }
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for RelationshipPhraseProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            write!(f, "RelationshipPhraseProxy({})", self_.read().unwrap().id)
        } else {
            write!(
                f,
                "{} RelationshipPhraseProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<RelationshipPhrase>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (relationship_phrase, store): (Arc<RwLock<RelationshipPhrase>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(RelationshipPhraseProxy {
            self_: Some(relationship_phrase),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&RELATIONSHIP_PHRASE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<Value> for RelationshipPhraseProxy {
    type Error = ChaChaError;

    fn try_from(value: Value) -> Result<Self, <RelationshipPhraseProxy as TryFrom<Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == RELATIONSHIP_PHRASE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).into_any();
                    Ok(any
                        .downcast_ref::<RelationshipPhraseProxy>()
                        .unwrap()
                        .clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "RelationshipPhraseProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "RelationshipPhraseProxy".to_owned(),
            }),
        }
    }
}
