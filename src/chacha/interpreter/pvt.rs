use std::fmt;

use ansi_term::Colour;
use heck::ToUpperCamelCase;

use crate::{
    interpreter::{context::ModelContext, debug, error, function},
    lu_dog::{ValueType, ValueTypeEnum},
    s_read,
    sarzak::Ty,
    RefType,
};

pub(crate) struct PrintableValueType<'a>(pub bool, pub RefType<ValueType>, pub &'a ModelContext);

impl<'a> fmt::Display for PrintableValueType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pretty = self.0;
        if pretty {
            self.print_pretty(f)
        } else {
            self.print(f)
        }
    }
}

impl<'a> PrintableValueType<'a> {
    fn print_pretty(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const TY_CLR: Colour = Colour::Purple;
        const TY_WARN_CLR: Colour = Colour::Yellow;

        let value = s_read!(self.1);
        let context = self.2;
        let lu_dog = context.lu_dog();
        let sarzak = context.sarzak();
        let model = context.models();

        match &value.subtype {
            ValueTypeEnum::Char(_) => write!(f, "{}", TY_CLR.italic().paint("char")),
            ValueTypeEnum::Enumeration(ref enumeration) => {
                let enumeration = s_read!(lu_dog).exhume_enumeration(enumeration).unwrap();
                debug!("enumeration: {enumeration:?}");
                let enumeration = s_read!(enumeration);
                write!(
                    f,
                    "{} {}",
                    TY_WARN_CLR.paint("enum"),
                    TY_CLR.italic().paint(&enumeration.name)
                )
            }
            ValueTypeEnum::Empty(_) => write!(f, "{}", TY_CLR.italic().paint("()")),
            ValueTypeEnum::Function(_) => write!(f, "{}", TY_CLR.italic().paint("function")),
            ValueTypeEnum::XFuture(ref id) => {
                let future = s_read!(lu_dog).exhume_x_future(id).unwrap();
                let inner = s_read!(future).r2_value_type(&s_read!(lu_dog))[0].clone();
                let inner = PrintableValueType(true, inner, context);

                write!(f, "{}<{inner}>", TY_CLR.paint("Future"))
            }
            ValueTypeEnum::Generic(g) => {
                let g = s_read!(lu_dog).exhume_generic(g).unwrap();
                let g = s_read!(g);
                write!(f, "<{}>", TY_CLR.italic().paint(g.name.as_str()))
            }
            ValueTypeEnum::Import(ref import) => {
                let import = s_read!(lu_dog).exhume_import(import).unwrap();
                let import = s_read!(import);
                if import.has_alias {
                    write!(f, "{}", TY_CLR.italic().paint(&import.alias))
                } else {
                    write!(f, "{}", TY_CLR.italic().paint(&import.name))
                }
            }
            ValueTypeEnum::Lambda(_) => write!(f, "{}", TY_CLR.italic().paint("lambda")),
            ValueTypeEnum::List(ref list) => {
                let list = s_read!(lu_dog).exhume_list(list).unwrap();
                let list = s_read!(list);
                let ty = list.r36_value_type(&s_read!(lu_dog))[0].clone();
                write!(
                    f,
                    "{}",
                    TY_CLR
                        .italic()
                        .paint(format!("[@{}]", PrintableValueType(true, ty, context)))
                )
            }
            ValueTypeEnum::XPlugin(ref plugin) => {
                let plugin = s_read!(lu_dog).exhume_x_plugin(plugin).unwrap();
                let plugin = s_read!(plugin);
                write!(
                    f,
                    "{}",
                    TY_CLR.italic().paint(format!("plugin: {}", plugin.name))
                )
            }
            ValueTypeEnum::Range(_) => write!(f, "{}", TY_CLR.italic().paint("range")),
            ValueTypeEnum::Task(_) => write!(f, "{}", TY_CLR.italic().paint("task")),
            ValueTypeEnum::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                let sarzak = s_read!(sarzak);
                if let Some(ty) = sarzak.exhume_ty(ty) {
                    match &*ty.read().unwrap() {
                        Ty::Boolean(_) => write!(f, "{}", TY_CLR.italic().paint("bool")),
                        Ty::Float(_) => write!(f, "{}", TY_CLR.italic().paint("float")),
                        Ty::Integer(_) => write!(f, "{}", TY_CLR.italic().paint("int")),
                        Ty::Object(ref object) => {
                            // This should probably just be an unwrap().
                            if let Some(object) = sarzak.exhume_object(object) {
                                write!(f, "{}", TY_CLR.italic().paint(&object.read().unwrap().name))
                            } else {
                                write!(f, "{}", TY_WARN_CLR.italic().paint("<unknown object>"))
                            }
                        }
                        Ty::SString(_) => write!(f, "{}", TY_CLR.italic().paint("string")),
                        Ty::SUuid(_) => write!(f, "{}", TY_CLR.italic().paint("Uuid")),
                        gamma => {
                            error!("deal with sarzak type {gamma:?}");
                            write!(f, "todo")
                        }
                    }
                } else {
                    // It's not a sarzak type, so it must be an object imported from
                    // one of the model domains.
                    let models = s_read!(model);
                    // 🚧 HashMapFix
                    for model in models.values() {
                        if let Some(ty) = model.0.exhume_ty(ty) {
                            if let Ty::Object(ref object) = &*ty.read().unwrap() {
                                if let Some(object) = model.0.exhume_object(object) {
                                    return write!(
                                        f,
                                        "{}",
                                        TY_CLR
                                            .italic()
                                            .paint(format!("{}Proxy", object.read().unwrap().name))
                                    );
                                }
                            }
                        }
                    }
                    write!(f, "{}", TY_WARN_CLR.italic().paint("<unknown object>"))
                }
            }
            ValueTypeEnum::Unknown(_) => write!(f, "{}", TY_WARN_CLR.italic().paint("<unknown>")),
            ValueTypeEnum::WoogStruct(ref woog_struct) => {
                let woog_struct = s_read!(lu_dog).exhume_woog_struct(woog_struct).unwrap();
                debug!("woog_struct {woog_struct:?}");
                let woog_struct = s_read!(woog_struct);
                write!(
                    f,
                    "{} {}",
                    TY_WARN_CLR.paint("struct"),
                    TY_CLR.italic().paint(&woog_struct.name)
                )
            }
            ValueTypeEnum::ZObjectStore(ref id) => {
                let zobject_store = s_read!(lu_dog).exhume_z_object_store(id).unwrap();
                let zobject_store = s_read!(zobject_store);
                let domain_name = &zobject_store.domain;

                write!(
                    f,
                    "{}",
                    TY_CLR
                        .italic()
                        .paint(format!("{}Store", domain_name.to_upper_camel_case()))
                )
            }
        }
    }

    fn print(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = s_read!(self.1);
        let context = self.2;
        let lu_dog = context.lu_dog();
        let sarzak = context.sarzak();
        let model = context.models();

        match &value.subtype {
            ValueTypeEnum::Char(_) => write!(f, "char"),
            ValueTypeEnum::Enumeration(ref enumeration) => {
                let enumeration = s_read!(lu_dog).exhume_enumeration(enumeration).unwrap();
                debug!("enumeration: {enumeration:?}");
                let enumeration = s_read!(enumeration);
                write!(f, "enum {}", enumeration.name)
            }
            ValueTypeEnum::Empty(_) => write!(f, "()"),
            ValueTypeEnum::Function(_) => write!(f, "function"),
            ValueTypeEnum::XFuture(_) => write!(f, "future"),
            ValueTypeEnum::Generic(g) => {
                let g = s_read!(lu_dog).exhume_generic(g).unwrap();
                let g = s_read!(g);
                write!(f, "<{}>", g.name)
            }
            ValueTypeEnum::Import(ref import) => {
                let import = s_read!(lu_dog).exhume_import(import).unwrap();
                let import = s_read!(import);
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueTypeEnum::Lambda(_) => write!(f, "lambda"),
            ValueTypeEnum::List(ref list) => {
                let list = s_read!(lu_dog).exhume_list(list).unwrap();
                let list = s_read!(list);
                let ty = list.r36_value_type(&s_read!(lu_dog))[0].clone();
                write!(f, "{}", PrintableValueType(false, ty, context))
            }
            ValueTypeEnum::XPlugin(ref plugin) => {
                let plugin = s_read!(lu_dog).exhume_x_plugin(plugin).unwrap();
                let plugin = s_read!(plugin);
                write!(f, "plugin: {}", plugin.name)
            }
            ValueTypeEnum::Range(_) => write!(f, "range"),
            ValueTypeEnum::Task(_) => write!(f, "task"),
            ValueTypeEnum::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                let sarzak = s_read!(sarzak);
                if let Some(ty) = sarzak.exhume_ty(ty) {
                    match &*ty.read().unwrap() {
                        Ty::Boolean(_) => write!(f, "bool"),
                        Ty::Float(_) => write!(f, "float"),
                        Ty::Integer(_) => write!(f, "int"),
                        Ty::Object(ref object) => {
                            // This should probably just be an unwrap().
                            if let Some(object) = sarzak.exhume_object(object) {
                                write!(f, "{}", object.read().unwrap().name)
                            } else {
                                write!(f, "<unknown object>")
                            }
                        }
                        Ty::SString(_) => write!(f, "string"),
                        Ty::SUuid(_) => write!(f, "Uuid"),
                        gamma => {
                            error!("deal with sarzak type {gamma:?}");
                            write!(f, "todo")
                        }
                    }
                } else {
                    // It's not a sarzak type, so it must be an object imported from
                    // one of the model domains.
                    let models = s_read!(model);
                    // 🚧 HashMapFix
                    for model in models.values() {
                        if let Some(ty) = model.0.exhume_ty(ty) {
                            if let Ty::Object(ref object) = &*ty.read().unwrap() {
                                if let Some(object) = model.0.exhume_object(object) {
                                    return write!(f, "{}Proxy", object.read().unwrap().name);
                                }
                            }
                        }
                    }
                    write!(f, "<unknown object>")
                }
            }
            ValueTypeEnum::Unknown(_) => write!(f, "<unknown>"),
            ValueTypeEnum::WoogStruct(ref woog_struct) => {
                let woog_struct = s_read!(lu_dog).exhume_woog_struct(woog_struct).unwrap();
                debug!("woog_struct {woog_struct:?}");
                let woog_struct = s_read!(woog_struct);
                write!(f, "struct {}", woog_struct.name)
            }
            ValueTypeEnum::ZObjectStore(ref id) => {
                let zobject_store = s_read!(lu_dog).exhume_z_object_store(id).unwrap();
                let zobject_store = s_read!(zobject_store);
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}
