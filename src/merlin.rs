//! 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
//! The following will need to be generated
//!
use std::{
    fmt,
    sync::{Arc, RwLock},
};

use lazy_static::lazy_static;
use sarzak::{
    lu_dog::{Empty, List, ValueType},
    merlin::{Inflection, ObjectStore as MerlinStore, Point},
    sarzak::{ObjectStore as SarzakStore, SUuid},
};
use uuid::uuid;

use crate::{Result, UserType, Value};

lazy_static! {
    // These are instances of the `model` loaded in the interpreter. In other
    // words, this is a reification of the model described by the model, called,
    // `model` in the interpreter, which is itself instances of the meta-model,
    // sarzak.
    //
    // We can share this with the interpreter via the MerlinStore UserType. I'm
    // just not sure that we need to do that. We have access here, and we should
    // only be doing stuff to the store from here, so maybe we don't need to
    // segregate UserType and StoreType. That seems to track...
    static ref MODEL: Arc<RwLock<MerlinStore>> = Arc::new(RwLock::new(
        MerlinStore::load("../sarzak/lu_dog.v2.json").unwrap()
    ));
}

/// This is a type in the store
///
/// The store is the one that we are generating code for. So there will be one
/// entry for each object.
///
/// Each entry in a proxy to an instance in the store. The proxy should contain
/// all of the methods defined on that type. Notably, new, and the relationship
/// navigation methods.
// #[derive(Clone, Debug)]
// pub(crate) enum StoreType {
//     Inflection(InflectionStoreType),
//     Point(PointStoreType),
// }

// impl StoreType {
//     pub fn get_type(&self) -> Arc<RwLock<ValueType>> {
//         match self {
//             // I could look this up if I had a pointer to the LuDog store.
//             Self::Inflection(_) => Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
//                 "42a2498a-62a9-4ff1-b765-7662910be974"
//             )))),
//             Self::Point(_) => Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
//                 "236d8f9c-5440-4ca2-8934-31cb07b1992e"
//             )))),
//         }
//     }
// }

/// A proxy for the [`Inflection] type
///
/// This proxies method calls to the [`Inflection] type. It manages this by
/// basically using a giant switch statement. We first check to see if we have
/// a `self`. If so, we know that we are making a regular method call, and we
/// do the right thing based on the name of the function.
///
/// If we don't have `self` then we know it's a static method call and do a
/// different match.
///
/// This could be better by using a PHF, I think. We know a-priori what the
/// methods are going to be, and we always ... No, it won't work. The function
/// arguments are going to be different, so we can't just store pointers to
/// functions. I've been down this road, I wonder how many times I'll retread it?
#[derive(Clone, Debug)]
pub(crate) struct InflectionStoreType {
    pub self_: Option<Inflection>,
}

impl InflectionStoreType {
    // This should maybe be a trait. Especially if we want to treat these all the
    // same, which I think we will want to do.
    // I also need to think about mutation. Do we want a separate mut version?
    pub fn call(
        &mut self,
        method: &str,
        _args: Vec<Value<MerlinType>>,
    ) -> Result<(Value<MerlinType>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.id()),
                    Arc::new(RwLock::new(ValueType::Ty(SUuid::new().id()))),
                )),
                // 🚧 This needs to be sorted out with the other error stuff.
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let inflection = Inflection::new();
                    let mut inflection_store_type = self.clone();
                    inflection_store_type.self_ = Some(inflection);
                    Ok((
                        Value::UserType(MerlinType::Inflection(inflection_store_type)),
                        // Clearly this will be generated...
                        // This is the id of the Inflection object
                        Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                            "42a2498a-62a9-4ff1-b765-7662910be974"
                        )))),
                    ))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }
}

impl Default for InflectionStoreType {
    fn default() -> Self {
        Self { self_: None }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PointStoreType {
    pub self_: Option<Point>,
}

// pub fn r5_line_segment_point<'a>(&'a self, store: &'a MerlinStore) -> Vec<&LineSegmentPoint> {
//     vec![store
//         .iter_line_segment_point()
//         .find(|line_segment_point| line_segment_point.point == self.id)
//         .unwrap()]
// }

impl PointStoreType {
    pub fn call(
        &mut self,
        method: &str,
        mut args: Vec<Value<MerlinType>>,
    ) -> Result<(Value<MerlinType>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Value::Uuid(self_.id),
                    Arc::new(RwLock::new(ValueType::Ty(SUuid::new().id()))),
                )),
                // "r5_line_segment_point" => Ok((
                //     Value::Vector(self_.r5_line_segment_point(&*MODEL)),
                //     Arc::new(RwLock::new(ValueType::List(List::ne(ValueType::new_ty))))
                // ))
                // 🚧 This needs to be sorted out with the other error stuff.
                道 => Ok((
                    Value::Error(format!("unknown method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let y = args.pop().unwrap().try_into()?;
                    let x = args.pop().unwrap().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let point = Point::new_inflection(x, y, &mut *model);
                    self.self_ = Some(point);
                    Ok((
                        Value::UserType(MerlinType::Point(self.clone())),
                        // Clearly this will be generated...
                        // This is the id of the point object
                        Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                            "236d8f9c-5440-4ca2-8934-31cb07b1992e"
                        )))),
                    ))
                }
                道 => Ok((
                    Value::Error(format!("unknown static method `{}`", 道)),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }
}

impl Default for PointStoreType {
    fn default() -> Self {
        Self { self_: None }
    }
}

/// User Defined Types
///
/// This is the main interface to the type system in the interpreter. This is wrapped
/// by `Value::UserType`. We are free to stuff whatever we like in here. It's anticipated
/// that there will be a type for the store, defined above. It's what holds the
/// instances. Then there will be the types that are actually in the store.
///
/// It needs to be a union of all of the types, for all of the domains that we are
/// working over.
///
/// I'm not sure how to do that. I can create one of these for each domain,
/// but we don't ever have them all in memory at once. So maybe a macro.
///
/// Or maybe this is a nested enum, that we build piecemeal? Sure would be nice
/// to be able to generate tokens for the main file, and manipulate the tree
/// directly, rather than having to parse the source code. We can do that with
/// syn and quote, I'm pretty sure.
///
/// For now I'll just pretend. Doing that thing above will be a big lift, I think,
/// and it's really a job for the svm compiler.
#[derive(Clone, Debug)]
pub(crate) enum MerlinType {
    // MerlinStore(Arc<RwLock<SarzakStore>>),
    Inflection(InflectionStoreType),
    Point(PointStoreType),
}

impl UserType for MerlinType {
    type Value<T> = Value<MerlinType>;

    fn get_type(&self) -> Arc<RwLock<ValueType>> {
        match self {
            // I could look this up if I had a pointer to the LuDog store.
            Self::Inflection(_) => Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                "42a2498a-62a9-4ff1-b765-7662910be974"
            )))),
            Self::Point(_) => Arc::new(RwLock::new(ValueType::WoogStruct(uuid!(
                "236d8f9c-5440-4ca2-8934-31cb07b1992e"
            )))),
        }
    }

    // fn call<T: UserType + fmt::Display + fmt::Debug + Clone>(
    fn call<T>(
        &mut self,
        method: &str,
        _args: Vec<Self::Value<T>>,
    ) -> Result<(Self::Value<T>, Arc<RwLock<ValueType>>)> {
        match self {
            // Self::MerlinStore(store) => store.write().unwrap().call(method, args),
            Self::Inflection(t) => t.call(method, _args),
            Self::Point(t) => t.call(method, _args),
        }
    }
}

impl fmt::Display for MerlinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // Self::MerlinStore(_) => write!(f, "MerlinStore"),
            // I think that we are guaranteed to have a self_ here.
            Self::Inflection(inflection) => {
                if let Some(self_) = &inflection.self_ {
                    write!(f, "Inflection({})\n", self_.id())
                } else {
                    write!(f, "Type: Inflection\n")
                }
            }
            Self::Point(point) => write!(f, "{:?}", point),
        }
    }
}
