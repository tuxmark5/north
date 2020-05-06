use {
  crate::{
    util::{trait_object},
  },
  std::{
    any::TypeId,
    marker::Unsize
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! impl_downcast { // impl_cast_self
  ($($targets:ty),*) => {
    fn query_vtable(&self, id: TypeId) -> Option<DowncastEntry> {
      $(if id == TypeId::of::<$targets>() {
        return Some(DowncastEntry::new_trait::<Self, $targets>());
      })*
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Downcast {
  fn query_vtable(&self, id: TypeId) -> Option<DowncastEntry>;
}

/*default impl<A> Downcast for A {
  default fn query_vtable(&self, id: TypeId) -> Option<DowncastEntry> {
    println!("DEFAUTL Q");
    None
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait DowncastExt {
  fn downcast_ref<T>(&self) -> Option<&T> where
    T: ?Sized + 'static;
}

impl<A: Downcast + ?Sized> DowncastExt for A {
  fn downcast_ref<T>(&self) -> Option<&T> where
    T: ?Sized + 'static
  {
    let data_ptr = trait_object::extract_data(self);
    let target_type = TypeId::of::<T>();
    let vtable_entry = self.query_vtable(target_type)?;
    if vtable_entry.target_type != target_type { return None; }

    match vtable_entry.action {
      DowncastAction::Direct => None,
      DowncastAction::Object(vtable_ptr) => {
        let result = unsafe { trait_object::make_object::<T>(data_ptr, vtable_ptr) };
        Some(result)
      },
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum DowncastAction {
  Direct,
  Object(*mut ())
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DowncastEntry {
  target_type: TypeId,
  action: DowncastAction
}

impl DowncastEntry {
  pub fn new_trait<A, T>() -> Self where
    A: Unsize<T> + 'static,
    T: ?Sized + 'static
  {
    let vtable = trait_object::vtable_of::<A, T>();
    Self {
      target_type: TypeId::of::<T>(),
      action: DowncastAction::Object(vtable),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
