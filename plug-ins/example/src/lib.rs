use std::{
    collections::VecDeque,
    fmt::{self, Display},
};

use abi_stable::{
    erased_types::TypeInfo,
    export_root_module,
    prefix_type::PrefixTypeTrait,
    sabi_extern_fn,
    sabi_trait::prelude::TD_Opaque,
    std_types::{RBoxError, ROk, RResult, RStr, RString, RVec},
    DynTrait,
};
use dwarf::{
    chacha::value::FfiValue,
    plug_in::{
        Appender, AppenderBox, Appender_TO, BoxedInterface, Error as AppError, ExampleLib,
        ExampleLib_Ref, Plugin, PluginId, PluginMod, PluginMod_Ref, PluginType, Plugin_TO,
        TheInterface, Unsupported,
    },
    Value,
};
use sarzak::sarzak::ObjectStore;
use serde::{Deserialize, Serialize};

///////////////////////////////////////////////////////////////////////////////////

/// Exports the root module of this library.
///
/// This code isn't run until the layout of the type it returns is checked.
#[export_root_module]
fn instantiate_root_module() -> PluginMod_Ref {
    PluginMod { new, load }.leak_into_prefix()
}

//////////////////////////////////////////////////////////////////////////////////////

/// Instantiates the plugin.
#[sabi_extern_fn]
pub fn new(plugin_id: PluginId) -> RResult<PluginType, AppError> {
    let this = SarzakStore {
        plugin_id,
        store: ObjectStore::new(),
    };
    ROk(Plugin_TO::from_value(this, TD_Opaque))
}

#[sabi_extern_fn]
pub fn load(plugin_id: PluginId, path: RString) -> RResult<PluginType, AppError> {
    let store = ObjectStore::load(path.as_str())
        .map_err(|e| {
            AppError::unsupported_command(Unsupported {
                plugin_name: plugin_id.named.clone().into_owned(),
                command_name: "load".into(),
                error: RBoxError::new(e),
            })
        })
        .unwrap();

    // dbg!(&store);
    let this = SarzakStore { plugin_id, store };
    ROk(Plugin_TO::from_value(this, TD_Opaque))
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Command {
    IterObject,
}

struct SarzakStore {
    plugin_id: PluginId,
    store: ObjectStore,
}

impl Plugin for SarzakStore {
    fn json_command(
        &mut self,
        command: RStr<'_>,
        // app: ApplicationMut<'_>,
    ) -> RResult<RString, AppError> {
        (|| -> Result<RString, AppError> {
            let command = serde_json::from_str::<Command>(command.as_str()).map_err(|e| {
                AppError::unsupported_command(Unsupported {
                    plugin_name: self.plugin_id().named.clone().into_owned(),
                    command_name: command.into(),
                    error: RBoxError::new(e),
                    // supported_commands: this.list_commands(),
                })
            })?;

            match command {
                Command::IterObject => {
                    let mut result = String::new();
                    let objects = self.store.iter_object();
                    for obj in objects {
                        result.push_str(&format!("{:?}", obj));
                    }
                    Ok(result.into())
                }
            }
        })()
        .into()
    }

    fn invoke_func(&mut self, name: RStr<'_>, args: RVec<RString>) -> RResult<FfiValue, AppError> {
        (|| -> Result<FfiValue, AppError> {
            let name = name.as_str();
            let args: VecDeque<Value> = args
                .into_iter()
                .map(|s| <RString as Into<String>>::into(s).try_into().unwrap())
                .collect::<VecDeque<_>>();
            dbg!(&name, &args);
            // let result = self.store.invoke_func(name, args)?;
            // Ok(result.into())
            let value: Value = 42.into();
            Ok(value.into())
        })()
        .into()
    }

    // fn invoke_method(
    //     &mut self,
    //     id: RStr<'_>,
    //     name: RStr<'_>,
    //     args: RVec<RString>,
    // ) -> RResult<RString, AppError> {
    //     (|| -> Result<RString, AppError> {
    //         let name = name.as_str();
    //         let args = args.into_iter().map(|s| s.into()).collect::<Vec<_>>();
    //         // let result = self.store.invoke_func(name, args)?;
    //         // Ok(result.into())
    //         Ok("".into())
    //     })()
    //     .into()
    // }

    fn plugin_id(&self) -> &PluginId {
        &self.plugin_id
    }

    fn close(self) {}
}
