use std::fmt;

use ansi_term::Colour;
use heck::ToUpperCamelCase;

use crate::{
    interpreter::{debug, error, function, Context},
    lu_dog::{ValueType, ValueTypeEnum, WoogOptionEnum},
    s_read,
    sarzak::Ty,
    RefType,
};

pub(crate) struct PrintableValueType<'a>(pub &'a RefType<ValueType>, pub &'a Context);

impl<'a> fmt::Display for PrintableValueType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const TY_CLR: Colour = Colour::Purple;
        const TY_WARN_CLR: Colour = Colour::Yellow;
        const TY_ERR_CLR: Colour = Colour::Red;

        let value = s_read!(self.0);
        let context = self.1;
        let lu_dog = context.lu_dog_heel();
        let sarzak = context.sarzak_heel();
        let model = context.models();

        match &value.subtype {
            ValueTypeEnum::Char(c) => write!(f, "{}", TY_CLR.italic().paint(format!("'{}'", c))),
            ValueTypeEnum::Empty(_) => write!(f, "{}", TY_CLR.italic().paint("()")),
            ValueTypeEnum::Error(_) => write!(f, "{}", TY_ERR_CLR.italic().paint("error")),
            ValueTypeEnum::Function(_) => write!(f, "{}", TY_CLR.italic().paint("function")),
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
                        .paint(format!("[{}]", PrintableValueType(&ty, context)))
                )
            }
            ValueTypeEnum::Range(_) => write!(f, "{}", TY_CLR.italic().paint("range")),
            ValueTypeEnum::Reference(ref reference) => {
                let reference = s_read!(lu_dog).exhume_reference(reference).unwrap();
                let reference = s_read!(reference);
                let ty = reference.r35_value_type(&s_read!(lu_dog))[0].clone();
                write!(
                    f,
                    "{}",
                    TY_CLR
                        .italic()
                        .paint(format!("&{}", PrintableValueType(&ty, context)))
                )
            }
            ValueTypeEnum::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                let sarzak = s_read!(sarzak);
                if let Some(ty) = sarzak.exhume_ty(ty) {
                    match ty {
                        Ty::Boolean(_) => write!(f, "{}", TY_CLR.italic().paint("bool")),
                        Ty::Float(_) => write!(f, "{}", TY_CLR.italic().paint("float")),
                        Ty::Integer(_) => write!(f, "{}", TY_CLR.italic().paint("int")),
                        Ty::Object(ref object) => {
                            // This should probably just be an unwrap().
                            if let Some(object) = sarzak.exhume_object(object) {
                                write!(f, "{}", TY_CLR.italic().paint(&object.name))
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
                    for (_, model) in &*models {
                        if let Some(Ty::Object(ref object)) = model.exhume_ty(ty) {
                            if let Some(object) = model.exhume_object(object) {
                                return write!(
                                    f,
                                    "{}",
                                    TY_CLR.italic().paint(format!("{}Proxy", object.name))
                                );
                            }
                        }
                    }
                    write!(f, "{}", TY_WARN_CLR.italic().paint("<unknown object>"))
                }
            }
            ValueTypeEnum::Unknown(_) => write!(f, "{}", TY_WARN_CLR.italic().paint("<unknown>")),
            ValueTypeEnum::WoogOption(ref option) => {
                let option = s_read!(lu_dog).exhume_woog_option(option).unwrap();
                let option = s_read!(option);
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "{}", TY_CLR.italic().paint("None")),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = s_read!(lu_dog).exhume_z_some(some).unwrap();
                        let some = s_read!(some);
                        let value = s_read!(some.r23_x_value(&s_read!(lu_dog))[0]).clone();
                        let ty = value.r24_value_type(&s_read!(lu_dog))[0].clone();
                        write!(
                            f,
                            "{}",
                            TY_CLR
                                .italic()
                                .paint(format!("Some({})", PrintableValueType(&ty, context)))
                        )
                    }
                }
            }
            ValueTypeEnum::WoogStruct(ref woog_struct) => {
                let woog_struct = s_read!(lu_dog).exhume_woog_struct(woog_struct).unwrap();
                debug!("woog_struct {woog_struct:?}");
                let woog_struct = s_read!(woog_struct);
                write!(f, "{}", TY_CLR.italic().paint(&woog_struct.name))
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
}
