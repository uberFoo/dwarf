use std::fmt;

use ansi_term::Colour;
use heck::ToUpperCamelCase;
use log::debug;
use log::error;

use crate::{
    dwarf::extruder::Context,
    lu_dog::{store::ObjectStore as LuDogStore, ValueType, ValueTypeEnum},
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
        const TY_WARN_CLR: Colour = Colour::Yellow;
        const TY_ERR_CLR: Colour = Colour::Red;

        let value = s_read!(self.0);
        let context = self.1;
        let lu_dog = self.2;

        match value.subtype {
            ValueTypeEnum::Char(_) => write!(f, "{}", TY_CLR.italic().paint("char")),
            ValueTypeEnum::Enumeration(ref enumeration) => {
                debug!("enumeration {:?}", enumeration);
                let enumeration = lu_dog.exhume_enumeration(enumeration).unwrap();
                let enumeration = s_read!(enumeration);
                let mut generics = Vec::new();
                if let Some(first) = enumeration.r105_enum_generic(lu_dog).first() {
                    let first = s_read!(first);
                    generics.push(first.name.clone());
                    let mut next = first.next;
                    while let Some(id) = next {
                        let generic = lu_dog.exhume_enum_generic(&id).unwrap();
                        let generic = s_read!(generic);
                        generics.push(generic.name.clone());
                        next = generic.next;
                    }
                }
                let mut name = enumeration.name.clone();
                if !generics.is_empty() {
                    name.push('<');
                    name.push_str(generics.join(", ").as_str());
                    name.push('>');
                }
                write!(f, "{}", TY_WARN_CLR.paint(name))
            }
            ValueTypeEnum::EnumGeneric(ref id) => {
                let enum_generic = lu_dog.exhume_enum_generic(id).unwrap();
                let enum_generic = s_read!(enum_generic);
                write!(f, "{}", enum_generic.name)
            }
            ValueTypeEnum::Empty(_) => write!(f, "{}", TY_CLR.italic().paint("()")),
            ValueTypeEnum::FuncGeneric(ref id) => {
                let func_generic = lu_dog.exhume_func_generic(id).unwrap();
                let func_generic = s_read!(func_generic);
                write!(f, "{}", func_generic.name)
            }
            ValueTypeEnum::Function(_) => write!(f, "{}", TY_CLR.italic().paint("<function>")),
            ValueTypeEnum::XFuture(ref id) => {
                let future = lu_dog.exhume_x_future(id).unwrap();
                let inner = s_read!(future).r2_value_type(lu_dog)[0].clone();
                let inner = PrintableValueType(&inner, context, lu_dog);

                write!(f, "{}<{inner}>", TY_WARN_CLR.paint("Future"))
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
            ValueTypeEnum::XPlugin(ref plugin) => {
                let plugin = lu_dog.exhume_x_plugin(plugin).unwrap();
                let plugin = s_read!(plugin);
                write!(f, "plugin::{}", plugin.name)
            }
            ValueTypeEnum::Range(_) => write!(f, "<range>"),
            ValueTypeEnum::StructGeneric(ref id) => {
                let struct_generic = lu_dog.exhume_struct_generic(id).unwrap();
                let struct_generic = s_read!(struct_generic);
                write!(f, "{}", struct_generic.name)
            }
            ValueTypeEnum::Task(_) => write!(f, "{}", TY_CLR.italic().paint("<task>")),
            ValueTypeEnum::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                if let Some(ty) = context.sarzak.exhume_ty(ty) {
                    match &*ty.read().unwrap() {
                        Ty::Boolean(_) => write!(f, "{}", TY_CLR.italic().paint("bool")),
                        Ty::Float(_) => write!(f, "{}", TY_CLR.italic().paint("float")),
                        Ty::Integer(_) => write!(f, "{}", TY_CLR.italic().paint("int")),
                        Ty::Object(ref object) => {
                            // This could probably just be an unwrap().
                            if let Some(object) = context.sarzak.exhume_object(object) {
                                write!(f, "{}", object.read().unwrap().name)
                            } else {
                                write!(f, "<unknown object>")
                            }
                        }
                        Ty::ZString(_) => write!(f, "{}", TY_CLR.italic().paint("string")),
                        Ty::ZUuid(_) => write!(f, "{}", TY_CLR.italic().paint("Uuid")),
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
            ValueTypeEnum::Unknown(_) => write!(f, "{}", TY_ERR_CLR.italic().paint("unknown")),
            ValueTypeEnum::WoogStruct(ref woog_struct) => {
                debug!("woog_struct {:?}", woog_struct);
                let woog_struct = lu_dog.exhume_woog_struct(woog_struct).unwrap();
                let woog_struct = s_read!(woog_struct);
                write!(f, "{}", TY_WARN_CLR.paint(&woog_struct.name))
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
