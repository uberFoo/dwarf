use std::fmt;

use ansi_term::Colour;
use heck::ToUpperCamelCase;
use log::debug;
use log::error;

use crate::{
    dwarf::extruder::Context,
    lu_dog::{store::ObjectStore as LuDogStore, ValueType, ValueTypeEnum, WoogOptionEnum},
    s_read,
    sarzak::Ty,
    RefType,
};

// #[derive(Debug)]
pub(super) struct PrintableValueType<'d, 'a, 'b>(
    pub &'d RefType<ValueType>,
    pub &'a Context<'a>,
    pub &'b LuDogStore,
);

impl<'d, 'a, 'b> fmt::Display for PrintableValueType<'d, 'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const TY_CLR: Colour = Colour::Purple;
        const _TY_WARN_CLR: Colour = Colour::Yellow;
        const _TY_ERR_CLR: Colour = Colour::Red;

        let value = s_read!(self.0);
        let context = self.1;
        let lu_dog = self.2;

        match value.subtype {
            ValueTypeEnum::Char(_) => write!(f, "{}", TY_CLR.italic().paint("char")),
            ValueTypeEnum::Enumeration(ref enumeration) => {
                debug!("enumeration {:?}", enumeration);
                let enumeration = lu_dog.exhume_enumeration(enumeration).unwrap();
                let enumeration = s_read!(enumeration);
                write!(f, "{}", enumeration.name)
            }
            ValueTypeEnum::Empty(_) => write!(f, "{}", TY_CLR.italic().paint("()")),
            ValueTypeEnum::Error(_) => write!(f, "{}", TY_CLR.italic().paint("<error>")),
            ValueTypeEnum::Function(_) => write!(f, "{}", TY_CLR.italic().paint("<function>")),
            ValueTypeEnum::Generic(ref g) => {
                let g = lu_dog.exhume_generic(g).unwrap();
                let g = s_read!(g);
                write!(f, "<{}>", g.name)
            }
            ValueTypeEnum::Import(ref import) => {
                let import = lu_dog.exhume_import(import).unwrap();
                let import = s_read!(import);
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueTypeEnum::Lambda(_) => write!(f, "{}", TY_CLR.italic().paint("<lambda>")),
            ValueTypeEnum::List(ref list) => {
                let list = lu_dog.exhume_list(list).unwrap();
                let list = s_read!(list);
                let ty = list.r36_value_type(lu_dog)[0].clone();
                write!(f, "[{}]", PrintableValueType(&ty, context, lu_dog))
            }
            ValueTypeEnum::Range(_) => write!(f, "<range>"),
            ValueTypeEnum::Reference(ref reference) => {
                let reference = lu_dog.exhume_reference(reference).unwrap();
                let reference = s_read!(reference);
                let ty = reference.r35_value_type(lu_dog)[0].clone();
                write!(f, "&{}", PrintableValueType(&ty, context, lu_dog))
            }
            ValueTypeEnum::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                if let Some(ty) = context.sarzak.exhume_ty(ty) {
                    match &*ty.borrow() {
                        Ty::Boolean(_) => write!(f, "{}", TY_CLR.italic().paint("bool")),
                        Ty::Float(_) => write!(f, "{}", TY_CLR.italic().paint("float")),
                        Ty::Integer(_) => write!(f, "{}", TY_CLR.italic().paint("int")),
                        Ty::Object(ref object) => {
                            error!("um, check this out?");
                            // This should probably just be an unwrap().
                            if let Some(object) = context.sarzak.exhume_object(object) {
                                write!(f, "{}", object.borrow().name)
                            } else {
                                write!(f, "<unknown object>")
                            }
                        }
                        Ty::SString(_) => write!(f, "{}", TY_CLR.italic().paint("string")),
                        Ty::SUuid(_) => write!(f, "{}", TY_CLR.italic().paint("uuid")),
                        #[allow(non_snake_case)]
                        Γ => {
                            error!("deal with sarzak type {:?}", Γ);
                            write!(f, "todo")
                        }
                    }
                } else {
                    // It's not a sarzak type, so it must be an object imported from
                    // one of the model domains.
                    // 🚧 HashMapFix
                    for model in context.models.values() {
                        if let Some(ty) = model.0.exhume_ty(ty) {
                            if let Ty::Object(ref object) = &*ty.borrow() {
                                if let Some(object) = model.0.exhume_object(object) {
                                    return write!(f, "{}Proxy", object.borrow().name);
                                }
                            }
                        }
                    }
                    write!(f, "<unknown object>")
                }
            }
            ValueTypeEnum::Unknown(_) => write!(f, "<unknown>"),
            ValueTypeEnum::WoogOption(ref option) => {
                let option = lu_dog.exhume_woog_option(option).unwrap();
                let option = s_read!(option);
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = lu_dog.exhume_z_some(some).unwrap();
                        let some = s_read!(some);
                        let value = s_read!(some.r23_x_value(lu_dog)[0]).clone();
                        let ty = value.r24_value_type(lu_dog)[0].clone();
                        write!(f, "Some({})", PrintableValueType(&ty, context, lu_dog))
                    }
                }
            }
            ValueTypeEnum::WoogStruct(ref woog_struct) => {
                debug!("woog_struct {:?}", woog_struct);
                let woog_struct = lu_dog.exhume_woog_struct(woog_struct).unwrap();
                let woog_struct = s_read!(woog_struct);
                write!(f, "{}", woog_struct.name)
            }
            ValueTypeEnum::ZObjectStore(ref id) => {
                let zobject_store = lu_dog.exhume_z_object_store(id).unwrap();
                let zobject_store = s_read!(zobject_store);
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}
