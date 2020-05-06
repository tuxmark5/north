use {
  std::{
    any::{TypeId},
    marker::{PhantomData},
    mem::{self},
    raw::{TraitObject},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct OpaqueObjectRef<'a> {
  trait_object: TraitObject,
  type_id: TypeId,
  lifetime: PhantomData<&'a ()>
}

impl<'a> OpaqueObjectRef<'a> {
  pub fn new<T>(object_ptr: &'a T) -> Self where
    T: ?Sized + 'static
  {
    let trait_object = unsafe {
      mem::transmute_copy::<_, TraitObject>(&object_ptr)
    };

    Self {
      trait_object,
      type_id: TypeId::of::<T>(),
      lifetime: PhantomData,
    }
  }

  pub fn downcast<T>(&self) -> Option<&'a T> where
    T: ?Sized + 'static
  {
    if self.type_id == TypeId::of::<T>() {
      Some(unsafe { mem::transmute_copy::<_, &T>(&self.trait_object) })
    } else {
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
