//! A blank domain
use std::{
    any::Any,
    collections::VecDeque,
    fmt,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use derivative::Derivative;
use lazy_static::lazy_static;
use sarzak::sarzak::{Object, SUuid, Ty};
use uuid::{uuid, Uuid};

use crate::lu_dog::{
    Argument, Binary, Block, BooleanLiteral, Call, Comparison, DwarfSourceFile, Empty, Error,
    ErrorExpression, Expression, ExpressionStatement, Field, FieldAccess, FieldExpression,
    FloatLiteral, ForLoop, Function, Grouped, Implementation, Import, Index, IntegerLiteral, Item,
    LetStatement, List, ListElement, ListExpression, Literal, LocalVariable, MethodCall,
    ObjectStore as LuDogStore, Operator, Parameter, Print, RangeExpression, Reference,
    ResultStatement, Statement, StaticMethodCall, StringLiteral, StructExpression, ValueType,
    Variable, VariableExpression, WoogOption, WoogStruct, XIf, XReturn, XValue, ZObjectStore,
    ZSome,
};

use crate::{ChaChaError, Result, StoreProxy, Value};

lazy_static! {
    static ref MODEL: Arc<RwLock<LuDogStore>> = Arc::new(RwLock::new(
        LuDogStore::load_bincode(
            "/Users/uberfoo/projects/sarzak/sarzak/target/sarzak/lu_dog/lu_dog.道"
        )
        .unwrap()
    ));
}

use crate::woog_structs::ARGUMENT_TYPE_UUID;
const ARGUMENT_STORE_TYPE_UUID: Uuid = uuid!("846aa191-7b0c-4325-8c21-09fa5edf7c78");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ArgumentProxy {
    self_: Option<Arc<RwLock<Argument>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ArgumentProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&ARGUMENT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ArgumentProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Argument"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        ARGUMENT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        ARGUMENT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let next: ArgumentProxy = (&*arg).try_into()?;
                    let next = next.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let function: CallProxy = (&*arg).try_into()?;
                    let function = function.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let argument = Argument::new(next.as_ref(), &function, &expression, &mut model);

                    let mut argument_proxy = self.clone();
                    argument_proxy.self_ = Some(argument);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            argument_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_argument()
                        .map(|argument| {
                            let mut argument_proxy = self.clone();
                            argument_proxy.self_ = Some(argument);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                argument_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "next" => {
                    if let Some(next) = &self_.read().unwrap().next {
                        let next = MODEL.read().unwrap().exhume_argument(next).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((next, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
                }
                "function" => {
                    let function = MODEL
                        .read()
                        .unwrap()
                        .exhume_argument(&self_.read().unwrap().function)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (function, self.lu_dog.clone()).into(),
                    )))
                }
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_argument(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for ArgumentProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ArgumentProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	next: {:?},", self_.read().unwrap().next)?;
            writeln!(f, "	function: {:?},", self_.read().unwrap().function)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ArgumentProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Argument>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((argument, store): (Arc<RwLock<Argument>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ArgumentProxy {
            self_: Some(argument),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&ARGUMENT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ArgumentProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ArgumentProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == ARGUMENT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ArgumentProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ArgumentProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ArgumentProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::BINARY_TYPE_UUID;
const BINARY_STORE_TYPE_UUID: Uuid = uuid!("9c8bc563-b596-4348-9189-065d747f7c9f");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BinaryProxy {
    self_: Option<Arc<RwLock<Binary>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl BinaryProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&BINARY_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for BinaryProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Binary"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        BINARY_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        BINARY_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_addition" => {
                    let mut model = MODEL.write().unwrap();
                    let mut addition_proxy = self.clone();
                    addition_proxy.self_ = Some(Binary::new_addition(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            addition_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_assignment" => {
                    let mut model = MODEL.write().unwrap();
                    let mut assignment_proxy = self.clone();
                    assignment_proxy.self_ = Some(Binary::new_assignment(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            assignment_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_division" => {
                    let mut model = MODEL.write().unwrap();
                    let mut division_proxy = self.clone();
                    division_proxy.self_ = Some(Binary::new_division(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            division_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_multiplication" => {
                    let mut model = MODEL.write().unwrap();
                    let mut multiplication_proxy = self.clone();
                    multiplication_proxy.self_ = Some(Binary::new_multiplication(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            multiplication_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_subtraction" => {
                    let mut model = MODEL.write().unwrap();
                    let mut subtraction_proxy = self.clone();
                    subtraction_proxy.self_ = Some(Binary::new_subtraction(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            subtraction_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_binary()
                        .map(|binary| {
                            let mut binary_proxy = self.clone();
                            binary_proxy.self_ = Some(binary);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                binary_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for BinaryProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "BinaryProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} BinaryProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Binary>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((binary, store): (Arc<RwLock<Binary>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_binary = binary.read().unwrap();

        match *read_binary {
            Binary::Addition(_) => {
                let mut binary_proxy = BinaryProxy::new_type(store.clone());
                binary_proxy.self_ = Some(binary.clone());
                Value::ProxyType(Arc::new(RwLock::new(binary_proxy)))
            }
            Binary::Assignment(_) => {
                let mut binary_proxy = BinaryProxy::new_type(store.clone());
                binary_proxy.self_ = Some(binary.clone());
                Value::ProxyType(Arc::new(RwLock::new(binary_proxy)))
            }
            Binary::Division(_) => {
                let mut binary_proxy = BinaryProxy::new_type(store.clone());
                binary_proxy.self_ = Some(binary.clone());
                Value::ProxyType(Arc::new(RwLock::new(binary_proxy)))
            }
            Binary::Multiplication(_) => {
                let mut binary_proxy = BinaryProxy::new_type(store.clone());
                binary_proxy.self_ = Some(binary.clone());
                Value::ProxyType(Arc::new(RwLock::new(binary_proxy)))
            }
            Binary::Subtraction(_) => {
                let mut binary_proxy = BinaryProxy::new_type(store.clone());
                binary_proxy.self_ = Some(binary.clone());
                Value::ProxyType(Arc::new(RwLock::new(binary_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for BinaryProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <BinaryProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == BINARY_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<BinaryProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "BinaryProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "BinaryProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::BLOCK_TYPE_UUID;
const BLOCK_STORE_TYPE_UUID: Uuid = uuid!("30c31621-a4bd-4356-80b9-1226e00651c9");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BlockProxy {
    self_: Option<Arc<RwLock<Block>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl BlockProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&BLOCK_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for BlockProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Block"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        BLOCK_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        BLOCK_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let bug: &Uuid = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let block = Block::new(bug.to_owned(), &mut model);

                    let mut block_proxy = self.clone();
                    block_proxy.self_ = Some(block);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            block_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_block()
                        .map(|block| {
                            let mut block_proxy = self.clone();
                            block_proxy.self_ = Some(block);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                block_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "bug" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().bug,
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for BlockProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "BlockProxy({{")?;
            writeln!(f, "	bug: {:?},", self_.read().unwrap().bug)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} BlockProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Block>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((block, store): (Arc<RwLock<Block>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(BlockProxy {
            self_: Some(block),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&BLOCK_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for BlockProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <BlockProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == BLOCK_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<BlockProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "BlockProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "BlockProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::BOOLEAN_LITERAL_TYPE_UUID;
const BOOLEAN_LITERAL_STORE_TYPE_UUID: Uuid = uuid!("d86aef6f-0a91-4f3e-81cb-b220dbe7c7b5");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct BooleanLiteralProxy {
    self_: Option<Arc<RwLock<BooleanLiteral>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl BooleanLiteralProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&BOOLEAN_LITERAL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for BooleanLiteralProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "BooleanLiteral"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        BOOLEAN_LITERAL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        BOOLEAN_LITERAL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_false_literal" => {
                    let mut model = MODEL.write().unwrap();
                    let mut false_literal_proxy = self.clone();
                    false_literal_proxy.self_ = Some(BooleanLiteral::new_false_literal(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            false_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_true_literal" => {
                    let mut model = MODEL.write().unwrap();
                    let mut true_literal_proxy = self.clone();
                    true_literal_proxy.self_ = Some(BooleanLiteral::new_true_literal(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            true_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_boolean_literal()
                        .map(|boolean_literal| {
                            let mut boolean_literal_proxy = self.clone();
                            boolean_literal_proxy.self_ = Some(boolean_literal);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                boolean_literal_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for BooleanLiteralProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "BooleanLiteralProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} BooleanLiteralProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<BooleanLiteral>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (boolean_literal, store): (Arc<RwLock<BooleanLiteral>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        let read_boolean_literal = boolean_literal.read().unwrap();

        match *read_boolean_literal {
            BooleanLiteral::FalseLiteral(_) => {
                let mut boolean_literal_proxy = BooleanLiteralProxy::new_type(store.clone());
                boolean_literal_proxy.self_ = Some(boolean_literal.clone());
                Value::ProxyType(Arc::new(RwLock::new(boolean_literal_proxy)))
            }
            BooleanLiteral::TrueLiteral(_) => {
                let mut boolean_literal_proxy = BooleanLiteralProxy::new_type(store.clone());
                boolean_literal_proxy.self_ = Some(boolean_literal.clone());
                Value::ProxyType(Arc::new(RwLock::new(boolean_literal_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for BooleanLiteralProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <BooleanLiteralProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == BOOLEAN_LITERAL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<BooleanLiteralProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "BooleanLiteralProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "BooleanLiteralProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::CALL_TYPE_UUID;
const CALL_STORE_TYPE_UUID: Uuid = uuid!("26dba2c5-a7e9-4556-a9c2-79091a97b941");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct CallProxy {
    self_: Option<Arc<RwLock<Call>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl CallProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&CALL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for CallProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Call"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        CALL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        CALL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_function_call" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_;

                    let mut model = MODEL.write().unwrap();
                    let mut function_call_proxy = self.clone();
                    function_call_proxy.self_ =
                        Some(Call::new_function_call(expression.as_ref(), &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            function_call_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_method_call" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: MethodCallProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut method_call_proxy = self.clone();
                    method_call_proxy.self_ = Some(Call::new_method_call(
                        expression.as_ref(),
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            method_call_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_static_method_call" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: StaticMethodCallProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut static_method_call_proxy = self.clone();
                    static_method_call_proxy.self_ = Some(Call::new_static_method_call(
                        expression.as_ref(),
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            static_method_call_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_call()
                        .map(|call| {
                            let mut call_proxy = self.clone();
                            call_proxy.self_ = Some(call);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                call_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    if let Some(expression) = &self_.read().unwrap().expression {
                        let expression = MODEL.read().unwrap().exhume_call(expression).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((expression, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for CallProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "CallProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} CallProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Call>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((call, store): (Arc<RwLock<Call>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(CallProxy {
            self_: Some(call),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&CALL_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for CallProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <CallProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == CALL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<CallProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "CallProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "CallProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::COMPARISON_TYPE_UUID;
const COMPARISON_STORE_TYPE_UUID: Uuid = uuid!("c46e6b80-0365-429a-abf9-ca75ce4f469a");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ComparisonProxy {
    self_: Option<Arc<RwLock<Comparison>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ComparisonProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&COMPARISON_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ComparisonProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Comparison"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        COMPARISON_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        COMPARISON_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_equal" => {
                    let mut model = MODEL.write().unwrap();
                    let mut equal_proxy = self.clone();
                    equal_proxy.self_ = Some(Comparison::new_equal(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            equal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_greater_than_or_equal" => {
                    let mut model = MODEL.write().unwrap();
                    let mut greater_than_or_equal_proxy = self.clone();
                    greater_than_or_equal_proxy.self_ =
                        Some(Comparison::new_greater_than_or_equal(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            greater_than_or_equal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_less_than_or_equal" => {
                    let mut model = MODEL.write().unwrap();
                    let mut less_than_or_equal_proxy = self.clone();
                    less_than_or_equal_proxy.self_ =
                        Some(Comparison::new_less_than_or_equal(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            less_than_or_equal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_comparison()
                        .map(|comparison| {
                            let mut comparison_proxy = self.clone();
                            comparison_proxy.self_ = Some(comparison);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                comparison_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for ComparisonProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ComparisonProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ComparisonProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Comparison>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((comparison, store): (Arc<RwLock<Comparison>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_comparison = comparison.read().unwrap();

        match *read_comparison {
            Comparison::Equal(_) => {
                let mut comparison_proxy = ComparisonProxy::new_type(store.clone());
                comparison_proxy.self_ = Some(comparison.clone());
                Value::ProxyType(Arc::new(RwLock::new(comparison_proxy)))
            }
            Comparison::GreaterThanOrEqual(_) => {
                let mut comparison_proxy = ComparisonProxy::new_type(store.clone());
                comparison_proxy.self_ = Some(comparison.clone());
                Value::ProxyType(Arc::new(RwLock::new(comparison_proxy)))
            }
            Comparison::LessThanOrEqual(_) => {
                let mut comparison_proxy = ComparisonProxy::new_type(store.clone());
                comparison_proxy.self_ = Some(comparison.clone());
                Value::ProxyType(Arc::new(RwLock::new(comparison_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for ComparisonProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ComparisonProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == COMPARISON_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ComparisonProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ComparisonProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ComparisonProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::DWARF_SOURCE_FILE_TYPE_UUID;
const DWARF_SOURCE_FILE_STORE_TYPE_UUID: Uuid = uuid!("97f815f0-dcc4-4bd3-843d-13ff1dc5802c");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct DwarfSourceFileProxy {
    self_: Option<Arc<RwLock<DwarfSourceFile>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl DwarfSourceFileProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&DWARF_SOURCE_FILE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for DwarfSourceFileProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "DwarfSourceFile"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        DWARF_SOURCE_FILE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        DWARF_SOURCE_FILE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let source: &String = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let dwarf_source_file = DwarfSourceFile::new(source.to_owned(), &mut model);

                    let mut dwarf_source_file_proxy = self.clone();
                    dwarf_source_file_proxy.self_ = Some(dwarf_source_file);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            dwarf_source_file_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_dwarf_source_file()
                        .map(|dwarf_source_file| {
                            let mut dwarf_source_file_proxy = self.clone();
                            dwarf_source_file_proxy.self_ = Some(dwarf_source_file);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                dwarf_source_file_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "source" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().source.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for DwarfSourceFileProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "DwarfSourceFileProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	source: {:?},", self_.read().unwrap().source)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} DwarfSourceFileProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<DwarfSourceFile>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (dwarf_source_file, store): (Arc<RwLock<DwarfSourceFile>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(DwarfSourceFileProxy {
            self_: Some(dwarf_source_file),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&DWARF_SOURCE_FILE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for DwarfSourceFileProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <DwarfSourceFileProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == DWARF_SOURCE_FILE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<DwarfSourceFileProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "DwarfSourceFileProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "DwarfSourceFileProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::ERROR_TYPE_UUID;
const ERROR_STORE_TYPE_UUID: Uuid = uuid!("23a9a479-68e7-4a70-bcef-9041f10fd287");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ErrorProxy {
    self_: Option<Arc<RwLock<Error>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ErrorProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&ERROR_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ErrorProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Error"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        ERROR_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        ERROR_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_unknown_variable" => {
                    let mut model = MODEL.write().unwrap();
                    let mut unknown_variable_proxy = self.clone();
                    unknown_variable_proxy.self_ = Some(Error::new_unknown_variable(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            unknown_variable_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_error()
                        .map(|error| {
                            let mut error_proxy = self.clone();
                            error_proxy.self_ = Some(error);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                error_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for ErrorProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ErrorProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} ErrorProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Error>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((error, store): (Arc<RwLock<Error>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_error = error.read().unwrap();

        match *read_error {
            Error::UnknownVariable(_) => {
                let mut error_proxy = ErrorProxy::new_type(store.clone());
                error_proxy.self_ = Some(error.clone());
                Value::ProxyType(Arc::new(RwLock::new(error_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for ErrorProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ErrorProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == ERROR_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ErrorProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ErrorProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ErrorProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::ERROR_EXPRESSION_TYPE_UUID;
const ERROR_EXPRESSION_STORE_TYPE_UUID: Uuid = uuid!("43bebcb3-06a1-410d-a19e-b3c1c6d1fa6b");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ErrorExpressionProxy {
    self_: Option<Arc<RwLock<ErrorExpression>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ErrorExpressionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&ERROR_EXPRESSION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ErrorExpressionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ErrorExpression"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        ERROR_EXPRESSION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        ERROR_EXPRESSION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let span: &String = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let error_expression = ErrorExpression::new(span.to_owned(), &mut model);

                    let mut error_expression_proxy = self.clone();
                    error_expression_proxy.self_ = Some(error_expression);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            error_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_error_expression()
                        .map(|error_expression| {
                            let mut error_expression_proxy = self.clone();
                            error_expression_proxy.self_ = Some(error_expression);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                error_expression_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "span" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().span.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for ErrorExpressionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ErrorExpressionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	span: {:?},", self_.read().unwrap().span)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ErrorExpressionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ErrorExpression>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (error_expression, store): (Arc<RwLock<ErrorExpression>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ErrorExpressionProxy {
            self_: Some(error_expression),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&ERROR_EXPRESSION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ErrorExpressionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ErrorExpressionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == ERROR_EXPRESSION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ErrorExpressionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ErrorExpressionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ErrorExpressionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::EXPRESSION_TYPE_UUID;
const EXPRESSION_STORE_TYPE_UUID: Uuid = uuid!("9352c766-9f6b-413b-9ea0-13f9c8e4d86e");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ExpressionProxy {
    self_: Option<Arc<RwLock<Expression>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ExpressionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&EXPRESSION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ExpressionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Expression"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        EXPRESSION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        EXPRESSION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_block" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: BlockProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut block_proxy = self.clone();
                    block_proxy.self_ = Some(Expression::new_block(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            block_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_call" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: CallProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut call_proxy = self.clone();
                    call_proxy.self_ = Some(Expression::new_call(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            call_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_error_expression" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ErrorExpressionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut error_expression_proxy = self.clone();
                    error_expression_proxy.self_ =
                        Some(Expression::new_error_expression(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            error_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_field_access" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: FieldAccessProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut field_access_proxy = self.clone();
                    field_access_proxy.self_ =
                        Some(Expression::new_field_access(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            field_access_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_for_loop" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ForLoopProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut for_loop_proxy = self.clone();
                    for_loop_proxy.self_ = Some(Expression::new_for_loop(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            for_loop_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_grouped" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: GroupedProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut grouped_proxy = self.clone();
                    grouped_proxy.self_ = Some(Expression::new_grouped(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            grouped_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_x_if" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: XIfProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut x_if_proxy = self.clone();
                    x_if_proxy.self_ = Some(Expression::new_x_if(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            x_if_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_index" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: IndexProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut index_proxy = self.clone();
                    index_proxy.self_ = Some(Expression::new_index(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            index_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_list_element" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ListElementProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut list_element_proxy = self.clone();
                    list_element_proxy.self_ =
                        Some(Expression::new_list_element(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            list_element_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_list_expression" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ListExpressionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut list_expression_proxy = self.clone();
                    list_expression_proxy.self_ =
                        Some(Expression::new_list_expression(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            list_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_literal" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: LiteralProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut literal_proxy = self.clone();
                    literal_proxy.self_ = Some(Expression::new_literal(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_operator" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: OperatorProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut operator_proxy = self.clone();
                    operator_proxy.self_ = Some(Expression::new_operator(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            operator_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_print" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: PrintProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut print_proxy = self.clone();
                    print_proxy.self_ = Some(Expression::new_print(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            print_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_range_expression" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: RangeExpressionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut range_expression_proxy = self.clone();
                    range_expression_proxy.self_ =
                        Some(Expression::new_range_expression(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            range_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_x_return" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: XReturnProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut x_return_proxy = self.clone();
                    x_return_proxy.self_ = Some(Expression::new_x_return(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            x_return_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_struct_expression" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: StructExpressionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut struct_expression_proxy = self.clone();
                    struct_expression_proxy.self_ =
                        Some(Expression::new_struct_expression(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            struct_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_variable_expression" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: VariableExpressionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut variable_expression_proxy = self.clone();
                    variable_expression_proxy.self_ =
                        Some(Expression::new_variable_expression(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            variable_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_expression()
                        .map(|expression| {
                            let mut expression_proxy = self.clone();
                            expression_proxy.self_ = Some(expression);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                expression_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for ExpressionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ExpressionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ExpressionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Expression>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((expression, store): (Arc<RwLock<Expression>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_expression = expression.read().unwrap();

        match *read_expression {
            Expression::Block(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::Call(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::Debugger(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::ErrorExpression(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::FieldAccess(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::ForLoop(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::Grouped(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::XIf(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::Index(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::ListElement(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::ListExpression(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::Literal(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::Operator(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::Print(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::RangeExpression(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::XReturn(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::StructExpression(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
            Expression::VariableExpression(_) => {
                let mut expression_proxy = ExpressionProxy::new_type(store.clone());
                expression_proxy.self_ = Some(expression.clone());
                Value::ProxyType(Arc::new(RwLock::new(expression_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for ExpressionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ExpressionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == EXPRESSION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ExpressionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ExpressionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ExpressionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::EXPRESSION_STATEMENT_TYPE_UUID;
const EXPRESSION_STATEMENT_STORE_TYPE_UUID: Uuid = uuid!("59f269ab-2dbc-4b07-b9bc-48441f20e78f");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ExpressionStatementProxy {
    self_: Option<Arc<RwLock<ExpressionStatement>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ExpressionStatementProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&EXPRESSION_STATEMENT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ExpressionStatementProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ExpressionStatement"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        EXPRESSION_STATEMENT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        EXPRESSION_STATEMENT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let expression_statement = ExpressionStatement::new(&expression, &mut model);

                    let mut expression_statement_proxy = self.clone();
                    expression_statement_proxy.self_ = Some(expression_statement);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            expression_statement_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_expression_statement()
                        .map(|expression_statement| {
                            let mut expression_statement_proxy = self.clone();
                            expression_statement_proxy.self_ = Some(expression_statement);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                expression_statement_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_expression_statement(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for ExpressionStatementProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ExpressionStatementProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ExpressionStatementProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ExpressionStatement>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (expression_statement, store): (Arc<RwLock<ExpressionStatement>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ExpressionStatementProxy {
            self_: Some(expression_statement),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&EXPRESSION_STATEMENT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ExpressionStatementProxy {
    type Error = ChaChaError;

    fn try_from(
        value: &Value,
    ) -> Result<Self, <ExpressionStatementProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == EXPRESSION_STATEMENT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any
                        .downcast_ref::<ExpressionStatementProxy>()
                        .unwrap()
                        .clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ExpressionStatementProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ExpressionStatementProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::FIELD_TYPE_UUID;
const FIELD_STORE_TYPE_UUID: Uuid = uuid!("141350e6-b62d-4a6d-9ea2-47333e51e3ea");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct FieldProxy {
    self_: Option<Arc<RwLock<Field>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl FieldProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&FIELD_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for FieldProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Field"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        FIELD_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        FIELD_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let x_model: WoogStructProxy = (&*arg).try_into()?;
                    let x_model = x_model.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: ValueTypeProxy = (&*arg).try_into()?;
                    let ty = ty.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let field = Field::new(name.to_owned(), &x_model, &ty, &mut model);

                    let mut field_proxy = self.clone();
                    field_proxy.self_ = Some(field);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            field_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_field()
                        .map(|field| {
                            let mut field_proxy = self.clone();
                            field_proxy.self_ = Some(field);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                field_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                "x_model" => {
                    let x_model = MODEL
                        .read()
                        .unwrap()
                        .exhume_field(&self_.read().unwrap().x_model)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((x_model, self.lu_dog.clone()).into())))
                }
                "ty" => {
                    let ty = MODEL
                        .read()
                        .unwrap()
                        .exhume_field(&self_.read().unwrap().ty)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((ty, self.lu_dog.clone()).into())))
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

impl fmt::Display for FieldProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "FieldProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "	x_model: {:?},", self_.read().unwrap().x_model)?;
            writeln!(f, "	ty: {:?},", self_.read().unwrap().ty)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} FieldProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Field>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((field, store): (Arc<RwLock<Field>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(FieldProxy {
            self_: Some(field),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&FIELD_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for FieldProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <FieldProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == FIELD_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<FieldProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "FieldProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "FieldProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::FIELD_ACCESS_TYPE_UUID;
const FIELD_ACCESS_STORE_TYPE_UUID: Uuid = uuid!("1ae9bff3-fb8a-4b35-bd3f-7120691cc7e7");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct FieldAccessProxy {
    self_: Option<Arc<RwLock<FieldAccess>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl FieldAccessProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&FIELD_ACCESS_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for FieldAccessProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "FieldAccess"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        FIELD_ACCESS_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        FIELD_ACCESS_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let field_access = FieldAccess::new(name.to_owned(), &expression, &mut model);

                    let mut field_access_proxy = self.clone();
                    field_access_proxy.self_ = Some(field_access);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            field_access_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_field_access()
                        .map(|field_access| {
                            let mut field_access_proxy = self.clone();
                            field_access_proxy.self_ = Some(field_access);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                field_access_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_field_access(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for FieldAccessProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "FieldAccessProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} FieldAccessProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<FieldAccess>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((field_access, store): (Arc<RwLock<FieldAccess>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(FieldAccessProxy {
            self_: Some(field_access),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&FIELD_ACCESS_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for FieldAccessProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <FieldAccessProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == FIELD_ACCESS_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<FieldAccessProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "FieldAccessProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "FieldAccessProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::FIELD_EXPRESSION_TYPE_UUID;
const FIELD_EXPRESSION_STORE_TYPE_UUID: Uuid = uuid!("3874760d-0104-4670-ba8b-1af413c5ec4a");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct FieldExpressionProxy {
    self_: Option<Arc<RwLock<FieldExpression>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl FieldExpressionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&FIELD_EXPRESSION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for FieldExpressionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "FieldExpression"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        FIELD_EXPRESSION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        FIELD_EXPRESSION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let woog_struct: StructExpressionProxy = (&*arg).try_into()?;
                    let woog_struct = woog_struct.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let field_expression = FieldExpression::new(
                        name.to_owned(),
                        &expression,
                        &woog_struct,
                        &mut model,
                    );

                    let mut field_expression_proxy = self.clone();
                    field_expression_proxy.self_ = Some(field_expression);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            field_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_field_expression()
                        .map(|field_expression| {
                            let mut field_expression_proxy = self.clone();
                            field_expression_proxy.self_ = Some(field_expression);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                field_expression_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_field_expression(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
                }
                "woog_struct" => {
                    let woog_struct = MODEL
                        .read()
                        .unwrap()
                        .exhume_field_expression(&self_.read().unwrap().woog_struct)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (woog_struct, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for FieldExpressionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "FieldExpressionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "	woog_struct: {:?},", self_.read().unwrap().woog_struct)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} FieldExpressionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<FieldExpression>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (field_expression, store): (Arc<RwLock<FieldExpression>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(FieldExpressionProxy {
            self_: Some(field_expression),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&FIELD_EXPRESSION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for FieldExpressionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <FieldExpressionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == FIELD_EXPRESSION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<FieldExpressionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "FieldExpressionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "FieldExpressionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::FLOAT_LITERAL_TYPE_UUID;
const FLOAT_LITERAL_STORE_TYPE_UUID: Uuid = uuid!("fa42f4e2-1ff3-473f-a4b9-593c01134e96");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct FloatLiteralProxy {
    self_: Option<Arc<RwLock<FloatLiteral>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl FloatLiteralProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&FLOAT_LITERAL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for FloatLiteralProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "FloatLiteral"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        FLOAT_LITERAL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        FLOAT_LITERAL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let x_value = (&*arg).try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let float_literal = FloatLiteral::new(x_value, &mut model);

                    let mut float_literal_proxy = self.clone();
                    float_literal_proxy.self_ = Some(float_literal);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            float_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_float_literal()
                        .map(|float_literal| {
                            let mut float_literal_proxy = self.clone();
                            float_literal_proxy.self_ = Some(float_literal);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                float_literal_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "x_value" => Ok(Arc::new(RwLock::new(Value::Float(
                    self_.read().unwrap().x_value,
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for FloatLiteralProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "FloatLiteralProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	x_value: {:?},", self_.read().unwrap().x_value)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} FloatLiteralProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<FloatLiteral>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((float_literal, store): (Arc<RwLock<FloatLiteral>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(FloatLiteralProxy {
            self_: Some(float_literal),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&FLOAT_LITERAL_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for FloatLiteralProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <FloatLiteralProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == FLOAT_LITERAL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<FloatLiteralProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "FloatLiteralProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "FloatLiteralProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::FOR_LOOP_TYPE_UUID;
const FOR_LOOP_STORE_TYPE_UUID: Uuid = uuid!("72443b79-5645-4b5f-b317-e1a8f815b81c");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ForLoopProxy {
    self_: Option<Arc<RwLock<ForLoop>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ForLoopProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&FOR_LOOP_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ForLoopProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ForLoop"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        FOR_LOOP_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        FOR_LOOP_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ident: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let for_loop = ForLoop::new(ident.to_owned(), &block, &expression, &mut model);

                    let mut for_loop_proxy = self.clone();
                    for_loop_proxy.self_ = Some(for_loop);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            for_loop_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_for_loop()
                        .map(|for_loop| {
                            let mut for_loop_proxy = self.clone();
                            for_loop_proxy.self_ = Some(for_loop);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                for_loop_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "ident" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().ident.to_owned(),
                )))),
                "block" => {
                    let block = MODEL
                        .read()
                        .unwrap()
                        .exhume_for_loop(&self_.read().unwrap().block)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((block, self.lu_dog.clone()).into())))
                }
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_for_loop(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for ForLoopProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ForLoopProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	ident: {:?},", self_.read().unwrap().ident)?;
            writeln!(f, "	block: {:?},", self_.read().unwrap().block)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ForLoopProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ForLoop>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((for_loop, store): (Arc<RwLock<ForLoop>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ForLoopProxy {
            self_: Some(for_loop),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&FOR_LOOP_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ForLoopProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ForLoopProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == FOR_LOOP_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ForLoopProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ForLoopProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ForLoopProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::FUNCTION_TYPE_UUID;
const FUNCTION_STORE_TYPE_UUID: Uuid = uuid!("d8dbc4ff-77d0-470c-a8c9-5c700376fdd5");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct FunctionProxy {
    self_: Option<Arc<RwLock<Function>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl FunctionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&FUNCTION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for FunctionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Function"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        FUNCTION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        FUNCTION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let impl_block: ImplementationProxy = (&*arg).try_into()?;
                    let impl_block = impl_block.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let return_type: ValueTypeProxy = (&*arg).try_into()?;
                    let return_type = return_type.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let function = Function::new(
                        name.to_owned(),
                        &block,
                        impl_block.as_ref(),
                        &return_type,
                        &mut model,
                    );

                    let mut function_proxy = self.clone();
                    function_proxy.self_ = Some(function);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            function_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_function()
                        .map(|function| {
                            let mut function_proxy = self.clone();
                            function_proxy.self_ = Some(function);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                function_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                "block" => {
                    let block = MODEL
                        .read()
                        .unwrap()
                        .exhume_function(&self_.read().unwrap().block)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((block, self.lu_dog.clone()).into())))
                }
                "impl_block" => {
                    if let Some(impl_block) = &self_.read().unwrap().impl_block {
                        let impl_block = MODEL.read().unwrap().exhume_function(impl_block).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((impl_block, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
                }
                "return_type" => {
                    let return_type = MODEL
                        .read()
                        .unwrap()
                        .exhume_function(&self_.read().unwrap().return_type)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (return_type, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for FunctionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "FunctionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "	block: {:?},", self_.read().unwrap().block)?;
            writeln!(f, "	impl_block: {:?},", self_.read().unwrap().impl_block)?;
            writeln!(f, "	return_type: {:?},", self_.read().unwrap().return_type)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} FunctionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Function>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((function, store): (Arc<RwLock<Function>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(FunctionProxy {
            self_: Some(function),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&FUNCTION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for FunctionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <FunctionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == FUNCTION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<FunctionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "FunctionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "FunctionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::GROUPED_TYPE_UUID;
const GROUPED_STORE_TYPE_UUID: Uuid = uuid!("0dc0e921-e7ff-4766-9738-e6e312f7f0de");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct GroupedProxy {
    self_: Option<Arc<RwLock<Grouped>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl GroupedProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&GROUPED_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for GroupedProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Grouped"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        GROUPED_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        GROUPED_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let grouped = Grouped::new(&expression, &mut model);

                    let mut grouped_proxy = self.clone();
                    grouped_proxy.self_ = Some(grouped);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            grouped_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_grouped()
                        .map(|grouped| {
                            let mut grouped_proxy = self.clone();
                            grouped_proxy.self_ = Some(grouped);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                grouped_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_grouped(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for GroupedProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "GroupedProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} GroupedProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Grouped>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((grouped, store): (Arc<RwLock<Grouped>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(GroupedProxy {
            self_: Some(grouped),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&GROUPED_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for GroupedProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <GroupedProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == GROUPED_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<GroupedProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "GroupedProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "GroupedProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::X_IF_TYPE_UUID;
const X_IF_STORE_TYPE_UUID: Uuid = uuid!("e1321ffa-07d5-480d-89f9-227b13d27ce1");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct XIfProxy {
    self_: Option<Arc<RwLock<XIf>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl XIfProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&X_IF_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for XIfProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "XIf"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        X_IF_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        X_IF_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let false_block: BlockProxy = (&*arg).try_into()?;
                    let false_block = false_block.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let true_block: BlockProxy = (&*arg).try_into()?;
                    let true_block = true_block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let test: ExpressionProxy = (&*arg).try_into()?;
                    let test = test.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let x_if = XIf::new(false_block.as_ref(), &true_block, &test, &mut model);

                    let mut x_if_proxy = self.clone();
                    x_if_proxy.self_ = Some(x_if);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            x_if_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_x_if()
                        .map(|x_if| {
                            let mut x_if_proxy = self.clone();
                            x_if_proxy.self_ = Some(x_if);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                x_if_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "false_block" => {
                    if let Some(false_block) = &self_.read().unwrap().false_block {
                        let false_block = MODEL.read().unwrap().exhume_x_if(false_block).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((false_block, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
                }
                "true_block" => {
                    let true_block = MODEL
                        .read()
                        .unwrap()
                        .exhume_x_if(&self_.read().unwrap().true_block)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (true_block, self.lu_dog.clone()).into(),
                    )))
                }
                "test" => {
                    let test = MODEL
                        .read()
                        .unwrap()
                        .exhume_x_if(&self_.read().unwrap().test)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((test, self.lu_dog.clone()).into())))
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

impl fmt::Display for XIfProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "XIfProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	false_block: {:?},", self_.read().unwrap().false_block)?;
            writeln!(f, "	true_block: {:?},", self_.read().unwrap().true_block)?;
            writeln!(f, "	test: {:?},", self_.read().unwrap().test)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} XIfProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<XIf>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((x_if, store): (Arc<RwLock<XIf>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(XIfProxy {
            self_: Some(x_if),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&X_IF_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for XIfProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <XIfProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == X_IF_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<XIfProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "XIfProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "XIfProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::IMPLEMENTATION_TYPE_UUID;
const IMPLEMENTATION_STORE_TYPE_UUID: Uuid = uuid!("88687f76-5d9e-404e-a801-f6f57f9b30ca");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ImplementationProxy {
    self_: Option<Arc<RwLock<Implementation>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ImplementationProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&IMPLEMENTATION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ImplementationProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Implementation"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        IMPLEMENTATION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        IMPLEMENTATION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let model_type: WoogStructProxy = (&*arg).try_into()?;
                    let model_type = model_type.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let implementation = Implementation::new(&model_type, &mut model);

                    let mut implementation_proxy = self.clone();
                    implementation_proxy.self_ = Some(implementation);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            implementation_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_implementation()
                        .map(|implementation| {
                            let mut implementation_proxy = self.clone();
                            implementation_proxy.self_ = Some(implementation);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                implementation_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "model_type" => {
                    let model_type = MODEL
                        .read()
                        .unwrap()
                        .exhume_implementation(&self_.read().unwrap().model_type)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (model_type, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for ImplementationProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ImplementationProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	model_type: {:?},", self_.read().unwrap().model_type)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ImplementationProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Implementation>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (implementation, store): (Arc<RwLock<Implementation>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ImplementationProxy {
            self_: Some(implementation),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&IMPLEMENTATION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ImplementationProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ImplementationProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == IMPLEMENTATION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ImplementationProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ImplementationProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ImplementationProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::IMPORT_TYPE_UUID;
const IMPORT_STORE_TYPE_UUID: Uuid = uuid!("c89e362d-7905-4226-8713-97d3d6f05037");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ImportProxy {
    self_: Option<Arc<RwLock<Import>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ImportProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&IMPORT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ImportProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Import"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        IMPORT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        IMPORT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let alias: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let has_alias = (&*arg).try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let path: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let object: ObjectProxy = (&*arg).try_into()?;
                    let object = object.self_;

                    let mut model = MODEL.write().unwrap();
                    let import = Import::new(
                        alias.to_owned(),
                        has_alias,
                        name.to_owned(),
                        path.to_owned(),
                        object.as_ref(),
                        &mut model,
                    );

                    let mut import_proxy = self.clone();
                    import_proxy.self_ = Some(import);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            import_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_import()
                        .map(|import| {
                            let mut import_proxy = self.clone();
                            import_proxy.self_ = Some(import);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                import_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "alias" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().alias.to_owned(),
                )))),
                "has_alias" => Ok(Arc::new(RwLock::new(Value::Boolean(
                    self_.read().unwrap().has_alias,
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                "path" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().path.to_owned(),
                )))),
                "object" => {
                    if let Some(object) = &self_.read().unwrap().object {
                        let object = MODEL.read().unwrap().exhume_import(object).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((object, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for ImportProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ImportProxy({{")?;
            writeln!(f, "	alias: {:?},", self_.read().unwrap().alias)?;
            writeln!(f, "	has_alias: {:?},", self_.read().unwrap().has_alias)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "	path: {:?},", self_.read().unwrap().path)?;
            writeln!(f, "	object: {:?},", self_.read().unwrap().object)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ImportProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Import>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((import, store): (Arc<RwLock<Import>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ImportProxy {
            self_: Some(import),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&IMPORT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ImportProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ImportProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == IMPORT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ImportProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ImportProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ImportProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::INDEX_TYPE_UUID;
const INDEX_STORE_TYPE_UUID: Uuid = uuid!("4d5720eb-da8c-493d-ab8d-cb6111dd6099");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct IndexProxy {
    self_: Option<Arc<RwLock<Index>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl IndexProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&INDEX_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for IndexProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Index"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        INDEX_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        INDEX_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let index: ExpressionProxy = (&*arg).try_into()?;
                    let index = index.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let target: ExpressionProxy = (&*arg).try_into()?;
                    let target = target.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let index = Index::new(&index, &target, &mut model);

                    let mut index_proxy = self.clone();
                    index_proxy.self_ = Some(index);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            index_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_index()
                        .map(|index| {
                            let mut index_proxy = self.clone();
                            index_proxy.self_ = Some(index);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                index_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "index" => {
                    let index = MODEL
                        .read()
                        .unwrap()
                        .exhume_index(&self_.read().unwrap().index)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((index, self.lu_dog.clone()).into())))
                }
                "target" => {
                    let target = MODEL
                        .read()
                        .unwrap()
                        .exhume_index(&self_.read().unwrap().target)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((target, self.lu_dog.clone()).into())))
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

impl fmt::Display for IndexProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "IndexProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	index: {:?},", self_.read().unwrap().index)?;
            writeln!(f, "	target: {:?},", self_.read().unwrap().target)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} IndexProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Index>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((index, store): (Arc<RwLock<Index>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(IndexProxy {
            self_: Some(index),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&INDEX_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for IndexProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <IndexProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == INDEX_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<IndexProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "IndexProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "IndexProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::INTEGER_LITERAL_TYPE_UUID;
const INTEGER_LITERAL_STORE_TYPE_UUID: Uuid = uuid!("b1612607-a813-4bc2-896b-88ec4b249447");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct IntegerLiteralProxy {
    self_: Option<Arc<RwLock<IntegerLiteral>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl IntegerLiteralProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&INTEGER_LITERAL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for IntegerLiteralProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "IntegerLiteral"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        INTEGER_LITERAL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        INTEGER_LITERAL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let x_value = (&*arg).try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let integer_literal = IntegerLiteral::new(x_value, &mut model);

                    let mut integer_literal_proxy = self.clone();
                    integer_literal_proxy.self_ = Some(integer_literal);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            integer_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_integer_literal()
                        .map(|integer_literal| {
                            let mut integer_literal_proxy = self.clone();
                            integer_literal_proxy.self_ = Some(integer_literal);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                integer_literal_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "x_value" => Ok(Arc::new(RwLock::new(Value::Integer(
                    self_.read().unwrap().x_value,
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for IntegerLiteralProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "IntegerLiteralProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	x_value: {:?},", self_.read().unwrap().x_value)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} IntegerLiteralProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<IntegerLiteral>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (integer_literal, store): (Arc<RwLock<IntegerLiteral>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(IntegerLiteralProxy {
            self_: Some(integer_literal),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&INTEGER_LITERAL_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for IntegerLiteralProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <IntegerLiteralProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == INTEGER_LITERAL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<IntegerLiteralProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "IntegerLiteralProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "IntegerLiteralProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::ITEM_TYPE_UUID;
const ITEM_STORE_TYPE_UUID: Uuid = uuid!("45644594-b418-5362-9294-a62fe2a0fa8e");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ItemProxy {
    self_: Option<Arc<RwLock<Item>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ItemProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&ITEM_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ItemProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Item"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        ITEM_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        ITEM_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_function" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let source: DwarfSourceFileProxy = (&*arg).try_into()?;
                    let source = source.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: FunctionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut function_proxy = self.clone();
                    function_proxy.self_ = Some(Item::new_function(&source, &subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            function_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_implementation" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let source: DwarfSourceFileProxy = (&*arg).try_into()?;
                    let source = source.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ImplementationProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut implementation_proxy = self.clone();
                    implementation_proxy.self_ =
                        Some(Item::new_implementation(&source, &subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            implementation_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_import" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let source: DwarfSourceFileProxy = (&*arg).try_into()?;
                    let source = source.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ImportProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut import_proxy = self.clone();
                    import_proxy.self_ = Some(Item::new_import(&source, &subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            import_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_woog_struct" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let source: DwarfSourceFileProxy = (&*arg).try_into()?;
                    let source = source.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: WoogStructProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut woog_struct_proxy = self.clone();
                    woog_struct_proxy.self_ =
                        Some(Item::new_woog_struct(&source, &subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            woog_struct_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_item()
                        .map(|item| {
                            let mut item_proxy = self.clone();
                            item_proxy.self_ = Some(item);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                item_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "source" => {
                    let source = MODEL
                        .read()
                        .unwrap()
                        .exhume_item(&self_.read().unwrap().source)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((source, self.lu_dog.clone()).into())))
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

impl fmt::Display for ItemProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ItemProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	source: {:?},", self_.read().unwrap().source)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} ItemProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Item>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((item, store): (Arc<RwLock<Item>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ItemProxy {
            self_: Some(item),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&ITEM_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ItemProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ItemProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == ITEM_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ItemProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ItemProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ItemProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LET_STATEMENT_TYPE_UUID;
const LET_STATEMENT_STORE_TYPE_UUID: Uuid = uuid!("c32fcb6d-b505-492f-95c8-5f118079a399");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct LetStatementProxy {
    self_: Option<Arc<RwLock<LetStatement>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl LetStatementProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LET_STATEMENT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for LetStatementProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "LetStatement"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LET_STATEMENT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LET_STATEMENT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let variable: LocalVariableProxy = (&*arg).try_into()?;
                    let variable = variable.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let let_statement = LetStatement::new(&expression, &variable, &mut model);

                    let mut let_statement_proxy = self.clone();
                    let_statement_proxy.self_ = Some(let_statement);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            let_statement_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_let_statement()
                        .map(|let_statement| {
                            let mut let_statement_proxy = self.clone();
                            let_statement_proxy.self_ = Some(let_statement);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                let_statement_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_let_statement(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
                }
                "variable" => {
                    let variable = MODEL
                        .read()
                        .unwrap()
                        .exhume_let_statement(&self_.read().unwrap().variable)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (variable, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for LetStatementProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "LetStatementProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "	variable: {:?},", self_.read().unwrap().variable)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} LetStatementProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<LetStatement>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((let_statement, store): (Arc<RwLock<LetStatement>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(LetStatementProxy {
            self_: Some(let_statement),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LET_STATEMENT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for LetStatementProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <LetStatementProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LET_STATEMENT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<LetStatementProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "LetStatementProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "LetStatementProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LIST_TYPE_UUID;
const LIST_STORE_TYPE_UUID: Uuid = uuid!("cdb1afaf-245c-4d56-bfea-10f69e45007d");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ListProxy {
    self_: Option<Arc<RwLock<List>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ListProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LIST_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ListProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "List"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LIST_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LIST_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: ValueTypeProxy = (&*arg).try_into()?;
                    let ty = ty.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let list = List::new(&ty, &mut model);

                    let mut list_proxy = self.clone();
                    list_proxy.self_ = Some(list);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            list_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_list()
                        .map(|list| {
                            let mut list_proxy = self.clone();
                            list_proxy.self_ = Some(list);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                list_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "ty" => {
                    let ty = MODEL
                        .read()
                        .unwrap()
                        .exhume_list(&self_.read().unwrap().ty)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((ty, self.lu_dog.clone()).into())))
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

impl fmt::Display for ListProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ListProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	ty: {:?},", self_.read().unwrap().ty)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} ListProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<List>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((list, store): (Arc<RwLock<List>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ListProxy {
            self_: Some(list),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LIST_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ListProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ListProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LIST_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ListProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ListProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ListProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LIST_ELEMENT_TYPE_UUID;
const LIST_ELEMENT_STORE_TYPE_UUID: Uuid = uuid!("ef4fe655-79cc-4eda-ac4e-d3ed30bd189d");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ListElementProxy {
    self_: Option<Arc<RwLock<ListElement>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ListElementProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LIST_ELEMENT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ListElementProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ListElement"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LIST_ELEMENT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LIST_ELEMENT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let next: ListElementProxy = (&*arg).try_into()?;
                    let next = next.self_;

                    let mut model = MODEL.write().unwrap();
                    let list_element = ListElement::new(&expression, next.as_ref(), &mut model);

                    let mut list_element_proxy = self.clone();
                    list_element_proxy.self_ = Some(list_element);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            list_element_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_list_element()
                        .map(|list_element| {
                            let mut list_element_proxy = self.clone();
                            list_element_proxy.self_ = Some(list_element);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                list_element_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_list_element(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
                }
                "next" => {
                    if let Some(next) = &self_.read().unwrap().next {
                        let next = MODEL.read().unwrap().exhume_list_element(next).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((next, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for ListElementProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ListElementProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "	next: {:?},", self_.read().unwrap().next)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ListElementProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ListElement>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((list_element, store): (Arc<RwLock<ListElement>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ListElementProxy {
            self_: Some(list_element),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LIST_ELEMENT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ListElementProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ListElementProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LIST_ELEMENT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ListElementProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ListElementProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ListElementProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LIST_EXPRESSION_TYPE_UUID;
const LIST_EXPRESSION_STORE_TYPE_UUID: Uuid = uuid!("e05ab5f3-17e7-4594-8ce0-c02d793a1df9");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ListExpressionProxy {
    self_: Option<Arc<RwLock<ListExpression>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ListExpressionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LIST_EXPRESSION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ListExpressionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ListExpression"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LIST_EXPRESSION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LIST_EXPRESSION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let elements: ListElementProxy = (&*arg).try_into()?;
                    let elements = elements.self_;

                    let mut model = MODEL.write().unwrap();
                    let list_expression = ListExpression::new(elements.as_ref(), &mut model);

                    let mut list_expression_proxy = self.clone();
                    list_expression_proxy.self_ = Some(list_expression);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            list_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_list_expression()
                        .map(|list_expression| {
                            let mut list_expression_proxy = self.clone();
                            list_expression_proxy.self_ = Some(list_expression);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                list_expression_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "elements" => {
                    if let Some(elements) = &self_.read().unwrap().elements {
                        let elements = MODEL
                            .read()
                            .unwrap()
                            .exhume_list_expression(elements)
                            .unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((elements, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for ListExpressionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ListExpressionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	elements: {:?},", self_.read().unwrap().elements)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ListExpressionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ListExpression>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (list_expression, store): (Arc<RwLock<ListExpression>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ListExpressionProxy {
            self_: Some(list_expression),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LIST_EXPRESSION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ListExpressionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ListExpressionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LIST_EXPRESSION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ListExpressionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ListExpressionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ListExpressionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LITERAL_TYPE_UUID;
const LITERAL_STORE_TYPE_UUID: Uuid = uuid!("dbbb7d46-4072-49a6-b483-cc8b25cbb4ea");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct LiteralProxy {
    self_: Option<Arc<RwLock<Literal>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl LiteralProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LITERAL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for LiteralProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Literal"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LITERAL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LITERAL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_boolean_literal" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: BooleanLiteralProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut boolean_literal_proxy = self.clone();
                    boolean_literal_proxy.self_ =
                        Some(Literal::new_boolean_literal(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            boolean_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_float_literal" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: FloatLiteralProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut float_literal_proxy = self.clone();
                    float_literal_proxy.self_ =
                        Some(Literal::new_float_literal(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            float_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_integer_literal" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: IntegerLiteralProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut integer_literal_proxy = self.clone();
                    integer_literal_proxy.self_ =
                        Some(Literal::new_integer_literal(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            integer_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_string_literal" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: StringLiteralProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut string_literal_proxy = self.clone();
                    string_literal_proxy.self_ =
                        Some(Literal::new_string_literal(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            string_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_literal()
                        .map(|literal| {
                            let mut literal_proxy = self.clone();
                            literal_proxy.self_ = Some(literal);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                literal_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for LiteralProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "LiteralProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} LiteralProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Literal>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((literal, store): (Arc<RwLock<Literal>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_literal = literal.read().unwrap();

        match *read_literal {
            Literal::BooleanLiteral(_) => {
                let mut literal_proxy = LiteralProxy::new_type(store.clone());
                literal_proxy.self_ = Some(literal.clone());
                Value::ProxyType(Arc::new(RwLock::new(literal_proxy)))
            }
            Literal::FloatLiteral(_) => {
                let mut literal_proxy = LiteralProxy::new_type(store.clone());
                literal_proxy.self_ = Some(literal.clone());
                Value::ProxyType(Arc::new(RwLock::new(literal_proxy)))
            }
            Literal::IntegerLiteral(_) => {
                let mut literal_proxy = LiteralProxy::new_type(store.clone());
                literal_proxy.self_ = Some(literal.clone());
                Value::ProxyType(Arc::new(RwLock::new(literal_proxy)))
            }
            Literal::StringLiteral(_) => {
                let mut literal_proxy = LiteralProxy::new_type(store.clone());
                literal_proxy.self_ = Some(literal.clone());
                Value::ProxyType(Arc::new(RwLock::new(literal_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for LiteralProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <LiteralProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LITERAL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<LiteralProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "LiteralProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "LiteralProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::LOCAL_VARIABLE_TYPE_UUID;
const LOCAL_VARIABLE_STORE_TYPE_UUID: Uuid = uuid!("0365b40c-c40a-4653-84ab-44d1c12d294f");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct LocalVariableProxy {
    self_: Option<Arc<RwLock<LocalVariable>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl LocalVariableProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&LOCAL_VARIABLE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for LocalVariableProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "LocalVariable"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        LOCAL_VARIABLE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        LOCAL_VARIABLE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let bug: &Uuid = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let local_variable = LocalVariable::new(bug.to_owned(), &mut model);

                    let mut local_variable_proxy = self.clone();
                    local_variable_proxy.self_ = Some(local_variable);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            local_variable_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_local_variable()
                        .map(|local_variable| {
                            let mut local_variable_proxy = self.clone();
                            local_variable_proxy.self_ = Some(local_variable);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                local_variable_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "bug" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().bug,
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for LocalVariableProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "LocalVariableProxy({{")?;
            writeln!(f, "	bug: {:?},", self_.read().unwrap().bug)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} LocalVariableProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<LocalVariable>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (local_variable, store): (Arc<RwLock<LocalVariable>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(LocalVariableProxy {
            self_: Some(local_variable),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&LOCAL_VARIABLE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for LocalVariableProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <LocalVariableProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == LOCAL_VARIABLE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<LocalVariableProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "LocalVariableProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "LocalVariableProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::METHOD_CALL_TYPE_UUID;
const METHOD_CALL_STORE_TYPE_UUID: Uuid = uuid!("75a16785-d611-45ce-b52c-284a9da0b4b8");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct MethodCallProxy {
    self_: Option<Arc<RwLock<MethodCall>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl MethodCallProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&METHOD_CALL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for MethodCallProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "MethodCall"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        METHOD_CALL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        METHOD_CALL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let method_call = MethodCall::new(name.to_owned(), &mut model);

                    let mut method_call_proxy = self.clone();
                    method_call_proxy.self_ = Some(method_call);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            method_call_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_method_call()
                        .map(|method_call| {
                            let mut method_call_proxy = self.clone();
                            method_call_proxy.self_ = Some(method_call);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                method_call_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for MethodCallProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "MethodCallProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} MethodCallProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<MethodCall>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((method_call, store): (Arc<RwLock<MethodCall>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(MethodCallProxy {
            self_: Some(method_call),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&METHOD_CALL_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for MethodCallProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <MethodCallProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == METHOD_CALL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<MethodCallProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "MethodCallProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "MethodCallProxy".to_owned(),
            }),
        }
    }
}

const OBJECT_STORE_TYPE_UUID: Uuid = uuid!("7178e7a4-5131-504b-a7b3-c2c0cfedf343");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ObjectProxy {
    self_: Option<Arc<RwLock<Object>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ObjectProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&OBJECT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ObjectProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Object"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        OBJECT_STORE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        OBJECT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "description" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().description.to_owned(),
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "key_letters" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().key_letters.to_owned(),
                )))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for ObjectProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ObjectProxy({{")?;
            writeln!(f, "	description: {:?},", self_.read().unwrap().description)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	key_letters: {:?},", self_.read().unwrap().key_letters)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ObjectProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Object>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((object, store): (Arc<RwLock<Object>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ObjectProxy {
            self_: Some(object),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&OBJECT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ObjectProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ObjectProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == OBJECT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ObjectProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ObjectProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ObjectProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::Z_OBJECT_STORE_TYPE_UUID;
const Z_OBJECT_STORE_STORE_TYPE_UUID: Uuid = uuid!("b7483723-222d-4f08-b7b9-e8b14f0308cf");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ZObjectStoreProxy {
    self_: Option<Arc<RwLock<ZObjectStore>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ZObjectStoreProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&Z_OBJECT_STORE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ZObjectStoreProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ZObjectStore"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        Z_OBJECT_STORE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        Z_OBJECT_STORE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let domain: &String = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let z_object_store = ZObjectStore::new(domain.to_owned(), &mut model);

                    let mut z_object_store_proxy = self.clone();
                    z_object_store_proxy.self_ = Some(z_object_store);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            z_object_store_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_z_object_store()
                        .map(|z_object_store| {
                            let mut z_object_store_proxy = self.clone();
                            z_object_store_proxy.self_ = Some(z_object_store);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                z_object_store_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "domain" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().domain.to_owned(),
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for ZObjectStoreProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ZObjectStoreProxy({{")?;
            writeln!(f, "	domain: {:?},", self_.read().unwrap().domain)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ZObjectStoreProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ZObjectStore>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((z_object_store, store): (Arc<RwLock<ZObjectStore>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ZObjectStoreProxy {
            self_: Some(z_object_store),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&Z_OBJECT_STORE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ZObjectStoreProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ZObjectStoreProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == Z_OBJECT_STORE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ZObjectStoreProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ZObjectStoreProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ZObjectStoreProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::OPERATOR_TYPE_UUID;
const OPERATOR_STORE_TYPE_UUID: Uuid = uuid!("3f7fd816-518b-4b9e-8134-3059e78045a0");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct OperatorProxy {
    self_: Option<Arc<RwLock<Operator>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl OperatorProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&OPERATOR_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for OperatorProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Operator"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        OPERATOR_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        OPERATOR_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_binary" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let rhs: ExpressionProxy = (&*arg).try_into()?;
                    let rhs = rhs.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let lhs: ExpressionProxy = (&*arg).try_into()?;
                    let lhs = lhs.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: BinaryProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut binary_proxy = self.clone();
                    binary_proxy.self_ = Some(Operator::new_binary(
                        rhs.as_ref(),
                        &lhs,
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            binary_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_comparison" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let rhs: ExpressionProxy = (&*arg).try_into()?;
                    let rhs = rhs.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let lhs: ExpressionProxy = (&*arg).try_into()?;
                    let lhs = lhs.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ComparisonProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut comparison_proxy = self.clone();
                    comparison_proxy.self_ = Some(Operator::new_comparison(
                        rhs.as_ref(),
                        &lhs,
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            comparison_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_operator()
                        .map(|operator| {
                            let mut operator_proxy = self.clone();
                            operator_proxy.self_ = Some(operator);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                operator_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "rhs" => {
                    if let Some(rhs) = &self_.read().unwrap().rhs {
                        let rhs = MODEL.read().unwrap().exhume_operator(rhs).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((rhs, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
                }
                "lhs" => {
                    let lhs = MODEL
                        .read()
                        .unwrap()
                        .exhume_operator(&self_.read().unwrap().lhs)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((lhs, self.lu_dog.clone()).into())))
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

impl fmt::Display for OperatorProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "OperatorProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	rhs: {:?},", self_.read().unwrap().rhs)?;
            writeln!(f, "	lhs: {:?},", self_.read().unwrap().lhs)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} OperatorProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Operator>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((operator, store): (Arc<RwLock<Operator>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(OperatorProxy {
            self_: Some(operator),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&OPERATOR_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for OperatorProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <OperatorProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == OPERATOR_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<OperatorProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "OperatorProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "OperatorProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::WOOG_OPTION_TYPE_UUID;
const WOOG_OPTION_STORE_TYPE_UUID: Uuid = uuid!("95562975-d3d3-47ba-b9cc-168d3cdc2054");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct WoogOptionProxy {
    self_: Option<Arc<RwLock<WoogOption>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl WoogOptionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&WOOG_OPTION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for WoogOptionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "WoogOption"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        WOOG_OPTION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        WOOG_OPTION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_z_none" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: ValueTypeProxy = (&*arg).try_into()?;
                    let ty = ty.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut z_none_proxy = self.clone();
                    z_none_proxy.self_ = Some(WoogOption::new_z_none(&ty, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            z_none_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_z_some" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: ValueTypeProxy = (&*arg).try_into()?;
                    let ty = ty.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ZSomeProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut z_some_proxy = self.clone();
                    z_some_proxy.self_ = Some(WoogOption::new_z_some(&ty, &subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            z_some_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_woog_option()
                        .map(|woog_option| {
                            let mut woog_option_proxy = self.clone();
                            woog_option_proxy.self_ = Some(woog_option);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                woog_option_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "ty" => {
                    let ty = MODEL
                        .read()
                        .unwrap()
                        .exhume_woog_option(&self_.read().unwrap().ty)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((ty, self.lu_dog.clone()).into())))
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

impl fmt::Display for WoogOptionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "WoogOptionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	ty: {:?},", self_.read().unwrap().ty)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} WoogOptionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<WoogOption>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((woog_option, store): (Arc<RwLock<WoogOption>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(WoogOptionProxy {
            self_: Some(woog_option),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&WOOG_OPTION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for WoogOptionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <WoogOptionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == WOOG_OPTION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<WoogOptionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "WoogOptionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "WoogOptionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::PARAMETER_TYPE_UUID;
const PARAMETER_STORE_TYPE_UUID: Uuid = uuid!("f192b162-8ada-4128-8805-1953e9165c54");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ParameterProxy {
    self_: Option<Arc<RwLock<Parameter>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ParameterProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&PARAMETER_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ParameterProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Parameter"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        PARAMETER_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        PARAMETER_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let function: FunctionProxy = (&*arg).try_into()?;
                    let function = function.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let next: ParameterProxy = (&*arg).try_into()?;
                    let next = next.self_;

                    let mut model = MODEL.write().unwrap();
                    let parameter = Parameter::new(&function, next.as_ref(), &mut model);

                    let mut parameter_proxy = self.clone();
                    parameter_proxy.self_ = Some(parameter);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            parameter_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_parameter()
                        .map(|parameter| {
                            let mut parameter_proxy = self.clone();
                            parameter_proxy.self_ = Some(parameter);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                parameter_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "function" => {
                    let function = MODEL
                        .read()
                        .unwrap()
                        .exhume_parameter(&self_.read().unwrap().function)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (function, self.lu_dog.clone()).into(),
                    )))
                }
                "next" => {
                    if let Some(next) = &self_.read().unwrap().next {
                        let next = MODEL.read().unwrap().exhume_parameter(next).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((next, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for ParameterProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ParameterProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	function: {:?},", self_.read().unwrap().function)?;
            writeln!(f, "	next: {:?},", self_.read().unwrap().next)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ParameterProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Parameter>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((parameter, store): (Arc<RwLock<Parameter>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ParameterProxy {
            self_: Some(parameter),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&PARAMETER_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ParameterProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ParameterProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == PARAMETER_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ParameterProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ParameterProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ParameterProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::PRINT_TYPE_UUID;
const PRINT_STORE_TYPE_UUID: Uuid = uuid!("6a0a9ba3-81ef-4fdc-8de1-d67e84dfb656");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct PrintProxy {
    self_: Option<Arc<RwLock<Print>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl PrintProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&PRINT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for PrintProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Print"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        PRINT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        PRINT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let print = Print::new(&expression, &mut model);

                    let mut print_proxy = self.clone();
                    print_proxy.self_ = Some(print);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            print_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_print()
                        .map(|print| {
                            let mut print_proxy = self.clone();
                            print_proxy.self_ = Some(print);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                print_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_print(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for PrintProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "PrintProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} PrintProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Print>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((print, store): (Arc<RwLock<Print>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(PrintProxy {
            self_: Some(print),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&PRINT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for PrintProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <PrintProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == PRINT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<PrintProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "PrintProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "PrintProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::RANGE_EXPRESSION_TYPE_UUID;
const RANGE_EXPRESSION_STORE_TYPE_UUID: Uuid = uuid!("0b3e9de0-d139-4934-a043-d1913a24de0c");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct RangeExpressionProxy {
    self_: Option<Arc<RwLock<RangeExpression>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl RangeExpressionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&RANGE_EXPRESSION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for RangeExpressionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "RangeExpression"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        RANGE_EXPRESSION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        RANGE_EXPRESSION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_from" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let lhs: ExpressionProxy = (&*arg).try_into()?;
                    let lhs = lhs.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let rhs: ExpressionProxy = (&*arg).try_into()?;
                    let rhs = rhs.self_;

                    let mut model = MODEL.write().unwrap();
                    let mut from_proxy = self.clone();
                    from_proxy.self_ = Some(RangeExpression::new_from(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            from_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_full" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let lhs: ExpressionProxy = (&*arg).try_into()?;
                    let lhs = lhs.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let rhs: ExpressionProxy = (&*arg).try_into()?;
                    let rhs = rhs.self_;

                    let mut model = MODEL.write().unwrap();
                    let mut full_proxy = self.clone();
                    full_proxy.self_ = Some(RangeExpression::new_full(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            full_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_inclusive" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let lhs: ExpressionProxy = (&*arg).try_into()?;
                    let lhs = lhs.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let rhs: ExpressionProxy = (&*arg).try_into()?;
                    let rhs = rhs.self_;

                    let mut model = MODEL.write().unwrap();
                    let mut inclusive_proxy = self.clone();
                    inclusive_proxy.self_ = Some(RangeExpression::new_inclusive(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            inclusive_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_to" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let lhs: ExpressionProxy = (&*arg).try_into()?;
                    let lhs = lhs.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let rhs: ExpressionProxy = (&*arg).try_into()?;
                    let rhs = rhs.self_;

                    let mut model = MODEL.write().unwrap();
                    let mut to_proxy = self.clone();
                    to_proxy.self_ = Some(RangeExpression::new_to(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            to_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_to_inclusive" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let lhs: ExpressionProxy = (&*arg).try_into()?;
                    let lhs = lhs.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let rhs: ExpressionProxy = (&*arg).try_into()?;
                    let rhs = rhs.self_;

                    let mut model = MODEL.write().unwrap();
                    let mut to_inclusive_proxy = self.clone();
                    to_inclusive_proxy.self_ = Some(RangeExpression::new_to_inclusive(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            to_inclusive_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_range_expression()
                        .map(|range_expression| {
                            let mut range_expression_proxy = self.clone();
                            range_expression_proxy.self_ = Some(range_expression);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                range_expression_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "lhs" => {
                    if let Some(lhs) = &self_.read().unwrap().lhs {
                        let lhs = MODEL.read().unwrap().exhume_range_expression(lhs).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((lhs, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
                }
                "rhs" => {
                    if let Some(rhs) = &self_.read().unwrap().rhs {
                        let rhs = MODEL.read().unwrap().exhume_range_expression(rhs).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((rhs, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for RangeExpressionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "RangeExpressionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	lhs: {:?},", self_.read().unwrap().lhs)?;
            writeln!(f, "	rhs: {:?},", self_.read().unwrap().rhs)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} RangeExpressionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<RangeExpression>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (range_expression, store): (Arc<RwLock<RangeExpression>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(RangeExpressionProxy {
            self_: Some(range_expression),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&RANGE_EXPRESSION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for RangeExpressionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <RangeExpressionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == RANGE_EXPRESSION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<RangeExpressionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "RangeExpressionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "RangeExpressionProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::REFERENCE_TYPE_UUID;
const REFERENCE_STORE_TYPE_UUID: Uuid = uuid!("a9b74602-bdc5-481f-af4c-8021553b895a");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ReferenceProxy {
    self_: Option<Arc<RwLock<Reference>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ReferenceProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&REFERENCE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ReferenceProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Reference"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        REFERENCE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        REFERENCE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let address: &Uuid = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let is_valid = (&*arg).try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: ValueTypeProxy = (&*arg).try_into()?;
                    let ty = ty.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let reference = Reference::new(address.to_owned(), is_valid, &ty, &mut model);

                    let mut reference_proxy = self.clone();
                    reference_proxy.self_ = Some(reference);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            reference_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_reference()
                        .map(|reference| {
                            let mut reference_proxy = self.clone();
                            reference_proxy.self_ = Some(reference);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                reference_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "address" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().address,
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "is_valid" => Ok(Arc::new(RwLock::new(Value::Boolean(
                    self_.read().unwrap().is_valid,
                )))),
                "ty" => {
                    let ty = MODEL
                        .read()
                        .unwrap()
                        .exhume_reference(&self_.read().unwrap().ty)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((ty, self.lu_dog.clone()).into())))
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

impl fmt::Display for ReferenceProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ReferenceProxy({{")?;
            writeln!(f, "	address: {:?},", self_.read().unwrap().address)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	is_valid: {:?},", self_.read().unwrap().is_valid)?;
            writeln!(f, "	ty: {:?},", self_.read().unwrap().ty)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ReferenceProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Reference>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((reference, store): (Arc<RwLock<Reference>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ReferenceProxy {
            self_: Some(reference),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&REFERENCE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ReferenceProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ReferenceProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == REFERENCE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ReferenceProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ReferenceProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ReferenceProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::RESULT_STATEMENT_TYPE_UUID;
const RESULT_STATEMENT_STORE_TYPE_UUID: Uuid = uuid!("b359d531-77ae-436f-9f0d-6a5632f1648e");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ResultStatementProxy {
    self_: Option<Arc<RwLock<ResultStatement>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ResultStatementProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&RESULT_STATEMENT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ResultStatementProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ResultStatement"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        RESULT_STATEMENT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        RESULT_STATEMENT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let result_statement = ResultStatement::new(&expression, &mut model);

                    let mut result_statement_proxy = self.clone();
                    result_statement_proxy.self_ = Some(result_statement);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            result_statement_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_result_statement()
                        .map(|result_statement| {
                            let mut result_statement_proxy = self.clone();
                            result_statement_proxy.self_ = Some(result_statement);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                result_statement_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_result_statement(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for ResultStatementProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ResultStatementProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ResultStatementProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ResultStatement>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (result_statement, store): (Arc<RwLock<ResultStatement>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ResultStatementProxy {
            self_: Some(result_statement),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&RESULT_STATEMENT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ResultStatementProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ResultStatementProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == RESULT_STATEMENT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ResultStatementProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ResultStatementProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ResultStatementProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::X_RETURN_TYPE_UUID;
const X_RETURN_STORE_TYPE_UUID: Uuid = uuid!("93f288e7-d670-40a6-91f3-2006b5efa8b4");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct XReturnProxy {
    self_: Option<Arc<RwLock<XReturn>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl XReturnProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&X_RETURN_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for XReturnProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "XReturn"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        X_RETURN_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        X_RETURN_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let expression: ExpressionProxy = (&*arg).try_into()?;
                    let expression = expression.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let x_return = XReturn::new(&expression, &mut model);

                    let mut x_return_proxy = self.clone();
                    x_return_proxy.self_ = Some(x_return);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            x_return_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_x_return()
                        .map(|x_return| {
                            let mut x_return_proxy = self.clone();
                            x_return_proxy.self_ = Some(x_return);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                x_return_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "expression" => {
                    let expression = MODEL
                        .read()
                        .unwrap()
                        .exhume_x_return(&self_.read().unwrap().expression)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (expression, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for XReturnProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "XReturnProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	expression: {:?},", self_.read().unwrap().expression)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} XReturnProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<XReturn>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((x_return, store): (Arc<RwLock<XReturn>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(XReturnProxy {
            self_: Some(x_return),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&X_RETURN_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for XReturnProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <XReturnProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == X_RETURN_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<XReturnProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "XReturnProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "XReturnProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::Z_SOME_TYPE_UUID;
const Z_SOME_STORE_TYPE_UUID: Uuid = uuid!("fa561e81-c4f0-4224-b7bb-4592ef9cf787");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ZSomeProxy {
    self_: Option<Arc<RwLock<ZSome>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ZSomeProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&Z_SOME_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ZSomeProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ZSome"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        Z_SOME_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        Z_SOME_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let inner: XValueProxy = (&*arg).try_into()?;
                    let inner = inner.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let z_some = ZSome::new(&inner, &mut model);

                    let mut z_some_proxy = self.clone();
                    z_some_proxy.self_ = Some(z_some);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            z_some_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_z_some()
                        .map(|z_some| {
                            let mut z_some_proxy = self.clone();
                            z_some_proxy.self_ = Some(z_some);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                z_some_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "inner" => {
                    let inner = MODEL
                        .read()
                        .unwrap()
                        .exhume_z_some(&self_.read().unwrap().inner)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((inner, self.lu_dog.clone()).into())))
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

impl fmt::Display for ZSomeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ZSomeProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	inner: {:?},", self_.read().unwrap().inner)?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} ZSomeProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<ZSome>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((z_some, store): (Arc<RwLock<ZSome>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(ZSomeProxy {
            self_: Some(z_some),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&Z_SOME_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for ZSomeProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ZSomeProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == Z_SOME_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ZSomeProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ZSomeProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ZSomeProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::STATEMENT_TYPE_UUID;
const STATEMENT_STORE_TYPE_UUID: Uuid = uuid!("6c7969f3-f150-4975-a989-a7bc8164b168");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct StatementProxy {
    self_: Option<Arc<RwLock<Statement>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl StatementProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&STATEMENT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for StatementProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Statement"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        STATEMENT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        STATEMENT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_expression_statement" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let next: StatementProxy = (&*arg).try_into()?;
                    let next = next.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ExpressionStatementProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut expression_statement_proxy = self.clone();
                    expression_statement_proxy.self_ = Some(Statement::new_expression_statement(
                        &block,
                        next.as_ref(),
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            expression_statement_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_item_statement" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let next: StatementProxy = (&*arg).try_into()?;
                    let next = next.self_;

                    let mut model = MODEL.write().unwrap();
                    let mut item_statement_proxy = self.clone();
                    item_statement_proxy.self_ = Some(Statement::new_item_statement(
                        &block,
                        next.as_ref(),
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            item_statement_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_let_statement" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let next: StatementProxy = (&*arg).try_into()?;
                    let next = next.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: LetStatementProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut let_statement_proxy = self.clone();
                    let_statement_proxy.self_ = Some(Statement::new_let_statement(
                        &block,
                        next.as_ref(),
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            let_statement_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_result_statement" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let next: StatementProxy = (&*arg).try_into()?;
                    let next = next.self_;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ResultStatementProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut result_statement_proxy = self.clone();
                    result_statement_proxy.self_ = Some(Statement::new_result_statement(
                        &block,
                        next.as_ref(),
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            result_statement_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_statement()
                        .map(|statement| {
                            let mut statement_proxy = self.clone();
                            statement_proxy.self_ = Some(statement);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                statement_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "block" => {
                    let block = MODEL
                        .read()
                        .unwrap()
                        .exhume_statement(&self_.read().unwrap().block)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((block, self.lu_dog.clone()).into())))
                }
                "next" => {
                    if let Some(next) = &self_.read().unwrap().next {
                        let next = MODEL.read().unwrap().exhume_statement(next).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((next, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for StatementProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "StatementProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	block: {:?},", self_.read().unwrap().block)?;
            writeln!(f, "	next: {:?},", self_.read().unwrap().next)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} StatementProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Statement>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((statement, store): (Arc<RwLock<Statement>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(StatementProxy {
            self_: Some(statement),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&STATEMENT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for StatementProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <StatementProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == STATEMENT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<StatementProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "StatementProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "StatementProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::STATIC_METHOD_CALL_TYPE_UUID;
const STATIC_METHOD_CALL_STORE_TYPE_UUID: Uuid = uuid!("01c8907d-cb59-4fae-a3ca-8cb331d18387");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct StaticMethodCallProxy {
    self_: Option<Arc<RwLock<StaticMethodCall>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl StaticMethodCallProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&STATIC_METHOD_CALL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for StaticMethodCallProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "StaticMethodCall"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        STATIC_METHOD_CALL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        STATIC_METHOD_CALL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let func: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: &String = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let static_method_call =
                        StaticMethodCall::new(func.to_owned(), ty.to_owned(), &mut model);

                    let mut static_method_call_proxy = self.clone();
                    static_method_call_proxy.self_ = Some(static_method_call);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            static_method_call_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_static_method_call()
                        .map(|static_method_call| {
                            let mut static_method_call_proxy = self.clone();
                            static_method_call_proxy.self_ = Some(static_method_call);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                static_method_call_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "func" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().func.to_owned(),
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "ty" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().ty.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for StaticMethodCallProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "StaticMethodCallProxy({{")?;
            writeln!(f, "	func: {:?},", self_.read().unwrap().func)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	ty: {:?},", self_.read().unwrap().ty)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} StaticMethodCallProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<StaticMethodCall>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (static_method_call, store): (Arc<RwLock<StaticMethodCall>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(StaticMethodCallProxy {
            self_: Some(static_method_call),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&STATIC_METHOD_CALL_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for StaticMethodCallProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <StaticMethodCallProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == STATIC_METHOD_CALL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<StaticMethodCallProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "StaticMethodCallProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "StaticMethodCallProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::STRING_LITERAL_TYPE_UUID;
const STRING_LITERAL_STORE_TYPE_UUID: Uuid = uuid!("d30d2b03-732b-41bb-89ed-d053750bf987");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct StringLiteralProxy {
    self_: Option<Arc<RwLock<StringLiteral>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl StringLiteralProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&STRING_LITERAL_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for StringLiteralProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "StringLiteral"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        STRING_LITERAL_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        STRING_LITERAL_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let x_value: &String = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let string_literal = StringLiteral::new(x_value.to_owned(), &mut model);

                    let mut string_literal_proxy = self.clone();
                    string_literal_proxy.self_ = Some(string_literal);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            string_literal_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_string_literal()
                        .map(|string_literal| {
                            let mut string_literal_proxy = self.clone();
                            string_literal_proxy.self_ = Some(string_literal);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                string_literal_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "x_value" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().x_value.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for StringLiteralProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "StringLiteralProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	x_value: {:?},", self_.read().unwrap().x_value)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} StringLiteralProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<StringLiteral>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (string_literal, store): (Arc<RwLock<StringLiteral>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(StringLiteralProxy {
            self_: Some(string_literal),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&STRING_LITERAL_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for StringLiteralProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <StringLiteralProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == STRING_LITERAL_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<StringLiteralProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "StringLiteralProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "StringLiteralProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::WOOG_STRUCT_TYPE_UUID;
const WOOG_STRUCT_STORE_TYPE_UUID: Uuid = uuid!("e68f8912-9897-4cbd-b363-cb4203a726a9");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct WoogStructProxy {
    self_: Option<Arc<RwLock<WoogStruct>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl WoogStructProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&WOOG_STRUCT_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for WoogStructProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "WoogStruct"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        WOOG_STRUCT_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        WOOG_STRUCT_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let object: ObjectProxy = (&*arg).try_into()?;
                    let object = object.self_;

                    let mut model = MODEL.write().unwrap();
                    let woog_struct = WoogStruct::new(name.to_owned(), object.as_ref(), &mut model);

                    let mut woog_struct_proxy = self.clone();
                    woog_struct_proxy.self_ = Some(woog_struct);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            woog_struct_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_woog_struct()
                        .map(|woog_struct| {
                            let mut woog_struct_proxy = self.clone();
                            woog_struct_proxy.self_ = Some(woog_struct);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                woog_struct_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                "object" => {
                    if let Some(object) = &self_.read().unwrap().object {
                        let object = MODEL.read().unwrap().exhume_woog_struct(object).unwrap();

                        Ok(Arc::new(RwLock::new(Value::Option(Some(Arc::new(
                            RwLock::new((object, self.lu_dog.clone()).into()),
                        ))))))
                    } else {
                        Ok(Arc::new(RwLock::new(Value::Option(None))))
                    }
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

impl fmt::Display for WoogStructProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "WoogStructProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "	object: {:?},", self_.read().unwrap().object)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} WoogStructProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<WoogStruct>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((woog_struct, store): (Arc<RwLock<WoogStruct>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(WoogStructProxy {
            self_: Some(woog_struct),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&WOOG_STRUCT_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for WoogStructProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <WoogStructProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == WOOG_STRUCT_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<WoogStructProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "WoogStructProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "WoogStructProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::STRUCT_EXPRESSION_TYPE_UUID;
const STRUCT_EXPRESSION_STORE_TYPE_UUID: Uuid = uuid!("d546feef-91df-49ea-ac61-430bdcf9832c");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct StructExpressionProxy {
    self_: Option<Arc<RwLock<StructExpression>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl StructExpressionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&STRUCT_EXPRESSION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for StructExpressionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "StructExpression"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        STRUCT_EXPRESSION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        STRUCT_EXPRESSION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let bug: &Uuid = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let woog_struct: WoogStructProxy = (&*arg).try_into()?;
                    let woog_struct = woog_struct.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let struct_expression =
                        StructExpression::new(bug.to_owned(), &woog_struct, &mut model);

                    let mut struct_expression_proxy = self.clone();
                    struct_expression_proxy.self_ = Some(struct_expression);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            struct_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_struct_expression()
                        .map(|struct_expression| {
                            let mut struct_expression_proxy = self.clone();
                            struct_expression_proxy.self_ = Some(struct_expression);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                struct_expression_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "bug" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().bug,
                )))),
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "woog_struct" => {
                    let woog_struct = MODEL
                        .read()
                        .unwrap()
                        .exhume_struct_expression(&self_.read().unwrap().woog_struct)
                        .unwrap();

                    Ok(Arc::new(RwLock::new(
                        (woog_struct, self.lu_dog.clone()).into(),
                    )))
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

impl fmt::Display for StructExpressionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "StructExpressionProxy({{")?;
            writeln!(f, "	bug: {:?},", self_.read().unwrap().bug)?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	woog_struct: {:?},", self_.read().unwrap().woog_struct)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} StructExpressionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<StructExpression>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (struct_expression, store): (Arc<RwLock<StructExpression>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(StructExpressionProxy {
            self_: Some(struct_expression),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&STRUCT_EXPRESSION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for StructExpressionProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <StructExpressionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == STRUCT_EXPRESSION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<StructExpressionProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "StructExpressionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "StructExpressionProxy".to_owned(),
            }),
        }
    }
}

const TY_STORE_TYPE_UUID: Uuid = uuid!("b8ec6afc-ddbd-53d6-9be3-e4b738941c2f");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct TyProxy {
    self_: Option<Arc<RwLock<Ty>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl TyProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&TY_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for TyProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Ty"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        TY_STORE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        TY_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for TyProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "TyProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(f, "{} TyProxy", Colour::Yellow.underline().paint("Type"))
        }
    }
}

impl From<(Arc<RwLock<Ty>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((ty, store): (Arc<RwLock<Ty>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_ty = ty.read().unwrap();

        match *read_ty {
            Ty::Boolean(_) => {
                let mut ty_proxy = TyProxy::new_type(store.clone());
                ty_proxy.self_ = Some(ty.clone());
                Value::ProxyType(Arc::new(RwLock::new(ty_proxy)))
            }
            Ty::External(_) => {
                let mut ty_proxy = TyProxy::new_type(store.clone());
                ty_proxy.self_ = Some(ty.clone());
                Value::ProxyType(Arc::new(RwLock::new(ty_proxy)))
            }
            Ty::Float(_) => {
                let mut ty_proxy = TyProxy::new_type(store.clone());
                ty_proxy.self_ = Some(ty.clone());
                Value::ProxyType(Arc::new(RwLock::new(ty_proxy)))
            }
            Ty::Integer(_) => {
                let mut ty_proxy = TyProxy::new_type(store.clone());
                ty_proxy.self_ = Some(ty.clone());
                Value::ProxyType(Arc::new(RwLock::new(ty_proxy)))
            }
            Ty::Object(_) => {
                let mut ty_proxy = TyProxy::new_type(store.clone());
                ty_proxy.self_ = Some(ty.clone());
                Value::ProxyType(Arc::new(RwLock::new(ty_proxy)))
            }
            Ty::SString(_) => {
                let mut ty_proxy = TyProxy::new_type(store.clone());
                ty_proxy.self_ = Some(ty.clone());
                Value::ProxyType(Arc::new(RwLock::new(ty_proxy)))
            }
            Ty::SUuid(_) => {
                let mut ty_proxy = TyProxy::new_type(store.clone());
                ty_proxy.self_ = Some(ty.clone());
                Value::ProxyType(Arc::new(RwLock::new(ty_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for TyProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <TyProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == TY_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<TyProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "TyProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "TyProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::X_VALUE_TYPE_UUID;
const X_VALUE_STORE_TYPE_UUID: Uuid = uuid!("94fc0044-4b88-4f5c-ac60-3a44262ade10");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct XValueProxy {
    self_: Option<Arc<RwLock<XValue>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl XValueProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&X_VALUE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for XValueProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "XValue"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        X_VALUE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        X_VALUE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_expression" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: ValueTypeProxy = (&*arg).try_into()?;
                    let ty = ty.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ExpressionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut expression_proxy = self.clone();
                    expression_proxy.self_ =
                        Some(XValue::new_expression(&block, &ty, &subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_variable" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let block: BlockProxy = (&*arg).try_into()?;
                    let block = block.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let ty: ValueTypeProxy = (&*arg).try_into()?;
                    let ty = ty.self_.unwrap();
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: VariableProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut variable_proxy = self.clone();
                    variable_proxy.self_ =
                        Some(XValue::new_variable(&block, &ty, &subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            variable_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_x_value()
                        .map(|x_value| {
                            let mut x_value_proxy = self.clone();
                            x_value_proxy.self_ = Some(x_value);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                x_value_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "block" => {
                    let block = MODEL
                        .read()
                        .unwrap()
                        .exhume_x_value(&self_.read().unwrap().block)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((block, self.lu_dog.clone()).into())))
                }
                "ty" => {
                    let ty = MODEL
                        .read()
                        .unwrap()
                        .exhume_x_value(&self_.read().unwrap().ty)
                        .unwrap();

                    Ok(Arc::new(RwLock::new((ty, self.lu_dog.clone()).into())))
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

impl fmt::Display for XValueProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "XValueProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	block: {:?},", self_.read().unwrap().block)?;
            writeln!(f, "	ty: {:?},", self_.read().unwrap().ty)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} XValueProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<XValue>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((x_value, store): (Arc<RwLock<XValue>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(XValueProxy {
            self_: Some(x_value),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&X_VALUE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for XValueProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <XValueProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == X_VALUE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<XValueProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "XValueProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "XValueProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::VALUE_TYPE_TYPE_UUID;
const VALUE_TYPE_STORE_TYPE_UUID: Uuid = uuid!("2e277e34-ab6f-4028-9c52-935c31520fec");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ValueTypeProxy {
    self_: Option<Arc<RwLock<ValueType>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl ValueTypeProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&VALUE_TYPE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for ValueTypeProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "ValueType"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        VALUE_TYPE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        VALUE_TYPE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id()))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_empty" => {
                    let mut model = MODEL.write().unwrap();
                    let mut empty_proxy = self.clone();
                    empty_proxy.self_ = Some(ValueType::new_empty(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            empty_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_error" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ErrorProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut error_proxy = self.clone();
                    error_proxy.self_ = Some(ValueType::new_error(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            error_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_function" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: FunctionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut function_proxy = self.clone();
                    function_proxy.self_ = Some(ValueType::new_function(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            function_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_import" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ImportProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut import_proxy = self.clone();
                    import_proxy.self_ = Some(ValueType::new_import(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            import_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_list" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ListProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut list_proxy = self.clone();
                    list_proxy.self_ = Some(ValueType::new_list(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            list_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_z_object_store" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ZObjectStoreProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut z_object_store_proxy = self.clone();
                    z_object_store_proxy.self_ =
                        Some(ValueType::new_z_object_store(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            z_object_store_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_woog_option" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: WoogOptionProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut woog_option_proxy = self.clone();
                    woog_option_proxy.self_ =
                        Some(ValueType::new_woog_option(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            woog_option_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_range" => {
                    let mut model = MODEL.write().unwrap();
                    let mut range_proxy = self.clone();
                    range_proxy.self_ = Some(ValueType::new_range(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            range_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_reference" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ReferenceProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut reference_proxy = self.clone();
                    reference_proxy.self_ = Some(ValueType::new_reference(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            reference_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_woog_struct" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: WoogStructProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut woog_struct_proxy = self.clone();
                    woog_struct_proxy.self_ =
                        Some(ValueType::new_woog_struct(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            woog_struct_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_ty" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: TyProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut ty_proxy = self.clone();
                    ty_proxy.self_ = Some(ValueType::new_ty(&subtype, &mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            ty_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_unknown" => {
                    let mut model = MODEL.write().unwrap();
                    let mut unknown_proxy = self.clone();
                    unknown_proxy.self_ = Some(ValueType::new_unknown(&mut model));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            unknown_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_value_type()
                        .map(|value_type| {
                            let mut value_type_proxy = self.clone();
                            value_type_proxy.self_ = Some(value_type);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                value_type_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(
                    self_.read().unwrap().id(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for ValueTypeProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "ValueTypeProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id())?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} ValueTypeProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<ValueType>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((value_type, store): (Arc<RwLock<ValueType>>, Arc<RwLock<LuDogStore>>)) -> Self {
        let read_value_type = value_type.read().unwrap();

        match *read_value_type {
            ValueType::Empty(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::Error(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::Function(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::Import(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::List(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::ZObjectStore(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::WoogOption(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::Range(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::Reference(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::WoogStruct(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::Ty(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
            ValueType::Unknown(_) => {
                let mut value_type_proxy = ValueTypeProxy::new_type(store.clone());
                value_type_proxy.self_ = Some(value_type.clone());
                Value::ProxyType(Arc::new(RwLock::new(value_type_proxy)))
            }
        }
    }
}

impl TryFrom<&Value> for ValueTypeProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <ValueTypeProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == VALUE_TYPE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<ValueTypeProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "ValueTypeProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "ValueTypeProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::VARIABLE_TYPE_UUID;
const VARIABLE_STORE_TYPE_UUID: Uuid = uuid!("954b354f-3a90-440b-ab0f-43efc00d275e");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct VariableProxy {
    self_: Option<Arc<RwLock<Variable>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl VariableProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&VARIABLE_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for VariableProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "Variable"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        VARIABLE_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        VARIABLE_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new_local_variable" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: LocalVariableProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut local_variable_proxy = self.clone();
                    local_variable_proxy.self_ = Some(Variable::new_local_variable(
                        name.to_owned(),
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            local_variable_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "new_parameter" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let subtype: ParameterProxy = (&*arg).try_into()?;
                    let subtype = subtype.self_.unwrap();

                    let mut model = MODEL.write().unwrap();
                    let mut parameter_proxy = self.clone();
                    parameter_proxy.self_ = Some(Variable::new_parameter(
                        name.to_owned(),
                        &subtype,
                        &mut model,
                    ));

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            parameter_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_variable()
                        .map(|variable| {
                            let mut variable_proxy = self.clone();
                            variable_proxy.self_ = Some(variable);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                variable_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for VariableProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "VariableProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} VariableProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<Variable>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from((variable, store): (Arc<RwLock<Variable>>, Arc<RwLock<LuDogStore>>)) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(VariableProxy {
            self_: Some(variable),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&VARIABLE_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for VariableProxy {
    type Error = ChaChaError;

    fn try_from(value: &Value) -> Result<Self, <VariableProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == VARIABLE_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any.downcast_ref::<VariableProxy>().unwrap().clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "VariableProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "VariableProxy".to_owned(),
            }),
        }
    }
}

use crate::woog_structs::VARIABLE_EXPRESSION_TYPE_UUID;
const VARIABLE_EXPRESSION_STORE_TYPE_UUID: Uuid = uuid!("d715c5f9-23f8-45e0-a1df-34e27acd01f5");
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct VariableExpressionProxy {
    self_: Option<Arc<RwLock<VariableExpression>>>,
    type_: Arc<RwLock<ValueType>>,
    #[derivative(Debug = "ignore")]
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl VariableExpressionProxy {
    pub fn new_type(lu_dog: Arc<RwLock<LuDogStore>>) -> Self {
        let type_ = lu_dog
            .read()
            .unwrap()
            .exhume_value_type(&VARIABLE_EXPRESSION_STORE_TYPE_UUID)
            .unwrap();

        Self {
            self_: None,
            type_,
            lu_dog,
        }
    }
}

impl StoreProxy for VariableExpressionProxy {
    /// Return the name of the type for which we proxy. Proxy on baby! 🕺
    fn name(&self) -> &str {
        "VariableExpression"
    }

    /// Magic methods to make things appear from thin air. 🪄
    fn as_any(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }

    /// Return the WoogStruct id of the type using this proxy.
    fn struct_uuid(&self) -> Uuid {
        VARIABLE_EXPRESSION_TYPE_UUID
    }

    /// Return the sarzak Object id of the type for which we are proxying.
    fn store_uuid(&self) -> Uuid {
        VARIABLE_EXPRESSION_STORE_TYPE_UUID
    }

    /// This method acts as the function call proxy for the type.
    fn call(
        &mut self,
        method: &str,
        args: &mut VecDeque<Arc<RwLock<Value>>>,
    ) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
        if let Some(self_) = &self.self_ {
            match method {
                "id" => Ok((
                    Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id))),
                    self.lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&SUuid::new().id())
                        .unwrap(),
                )),
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        } else {
            match method {
                "new" => {
                    let arg = args.pop_front().unwrap();
                    let arg = arg.read().unwrap();
                    let name: &String = &(*arg).clone().try_into()?;

                    let mut model = MODEL.write().unwrap();
                    let variable_expression = VariableExpression::new(name.to_owned(), &mut model);

                    let mut variable_expression_proxy = self.clone();
                    variable_expression_proxy.self_ = Some(variable_expression);

                    Ok((
                        Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                            variable_expression_proxy,
                        ))))),
                        self.type_.clone(),
                    ))
                }
                "instances" => {
                    let instances = MODEL
                        .read()
                        .unwrap()
                        .iter_variable_expression()
                        .map(|variable_expression| {
                            let mut variable_expression_proxy = self.clone();
                            variable_expression_proxy.self_ = Some(variable_expression);
                            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(
                                variable_expression_proxy,
                            )))))
                        })
                        .collect();

                    let list = List::new(&self.type_, &mut self.lu_dog.write().unwrap());
                    let ty = ValueType::new_list(&list, &mut self.lu_dog.write().unwrap());

                    Ok((Arc::new(RwLock::new(Value::Vector(instances))), ty))
                }
                道 => Ok((
                    Arc::new(RwLock::new(Value::Error(format!(
                        "unknown static method `{}`",
                        道
                    )))),
                    Arc::new(RwLock::new(ValueType::Empty(Empty::new().id()))),
                )),
            }
        }
    }

    /// This method acts as the field access proxy for the type.
    fn get_attr_value(&self, field: &str) -> Result<Arc<RwLock<Value>>> {
        if let Some(self_) = &self.self_ {
            match field {
                "id" => Ok(Arc::new(RwLock::new(Value::Uuid(self_.read().unwrap().id)))),
                "name" => Ok(Arc::new(RwLock::new(Value::String(
                    self_.read().unwrap().name.to_owned(),
                )))),
                _ => Err(ChaChaError::NoSuchField {
                    field: field.to_owned(),
                }),
            }
        } else {
            Err(ChaChaError::NotAnInstance)
        }
    }
}

impl fmt::Display for VariableExpressionProxy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(self_) = &self.self_ {
            writeln!(f, "VariableExpressionProxy({{")?;
            writeln!(f, "	id: {:?},", self_.read().unwrap().id)?;
            writeln!(f, "	name: {:?},", self_.read().unwrap().name)?;
            writeln!(f, "}})")
        } else {
            writeln!(
                f,
                "{} VariableExpressionProxy",
                Colour::Yellow.underline().paint("Type")
            )
        }
    }
}

impl From<(Arc<RwLock<VariableExpression>>, Arc<RwLock<LuDogStore>>)> for Value {
    fn from(
        (variable_expression, store): (Arc<RwLock<VariableExpression>>, Arc<RwLock<LuDogStore>>),
    ) -> Self {
        Value::ProxyType(Arc::new(RwLock::new(VariableExpressionProxy {
            self_: Some(variable_expression),
            type_: store
                .read()
                .unwrap()
                .exhume_value_type(&VARIABLE_EXPRESSION_STORE_TYPE_UUID)
                .unwrap(),
            lu_dog: store.clone(),
        })))
    }
}

impl TryFrom<&Value> for VariableExpressionProxy {
    type Error = ChaChaError;

    fn try_from(
        value: &Value,
    ) -> Result<Self, <VariableExpressionProxy as TryFrom<&Value>>::Error> {
        match value {
            Value::ProxyType(proxy) => {
                let read_proxy = proxy.read().unwrap();

                if read_proxy.store_uuid() == VARIABLE_EXPRESSION_STORE_TYPE_UUID {
                    let any = (&*read_proxy).as_any();
                    Ok(any
                        .downcast_ref::<VariableExpressionProxy>()
                        .unwrap()
                        .clone())
                } else {
                    Err(ChaChaError::Conversion {
                        src: read_proxy.name().to_owned(),
                        dst: "VariableExpressionProxy".to_owned(),
                    })
                }
            }
            _ => Err(ChaChaError::Conversion {
                src: value.to_string(),
                dst: "VariableExpressionProxy".to_owned(),
            }),
        }
    }
}
