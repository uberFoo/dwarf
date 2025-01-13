use crate::{
    lu_dog::ObjectStore as LuDogStore, sarzak::ObjectStore as SarzakStore, ModelStore, RefType,
};

#[derive(Clone, Debug)]
pub struct ModelContext {
    lu_dog: RefType<LuDogStore>,
    sarzak: RefType<SarzakStore>,
    models: RefType<ModelStore>,
}

impl ModelContext {
    pub fn new(
        lu_dog: RefType<LuDogStore>,
        sarzak: RefType<SarzakStore>,
        models: RefType<ModelStore>,
    ) -> Self {
        Self {
            lu_dog,
            sarzak,
            models,
        }
    }

    pub fn lu_dog(&self) -> &RefType<LuDogStore> {
        &self.lu_dog
    }

    pub fn sarzak(&self) -> &RefType<SarzakStore> {
        &self.sarzak
    }

    pub fn models(&self) -> &RefType<ModelStore> {
        &self.models
    }
}
