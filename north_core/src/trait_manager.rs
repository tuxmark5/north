use {
  crate::{
    util::cast::{
      Cast, CastKind, CastRaw, DefaultCaster
    }
  },
  std::{
    any::{Any, TypeId},
    collections::HashMap,
    marker::Unsize,
    mem::{transmute_copy},
    ptr,
    raw::{TraitObject},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct Signature {
  object_id: TypeId,
  interface_id: TypeId,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct TraitManager {
  impls: HashMap<Signature, *mut ()>
}

impl TraitManager {
  pub fn new() -> TraitManager {
    TraitManager {
      impls: HashMap::new(),
    }
  }

  pub fn register<O, I>(&mut self) where
    O: Unsize<I> + 'static,
    I: ?Sized + 'static
  {
    let signature = Signature {
      object_id: TypeId::of::<O>(),
      interface_id: TypeId::of::<I>(),
    };

    let object: *const O = ptr::null();
    let fat_object: *const I = object;
    let raw_object = unsafe { transmute_copy::<_, TraitObject>(&fat_object) };
    let vtable = raw_object.vtable;

    self.impls.insert(signature, vtable);
  }
}

impl<A, B> CastRaw<A, B> for TraitManager where
  A: Any + ?Sized, B: 'static + ?Sized
{
  fn cast_raw<'a>(&self, value: &'a A) -> Option<CastKind> {
    if let Some(kind) = CastRaw::<A, B>::cast_raw(&DefaultCaster, value) {
      return Some(kind);
    }

    let signature = Signature {
      object_id: (*value).type_id(),
      interface_id: TypeId::of::<B>(),
    };

    self.impls.get(&signature).map(|vtable| {
      CastKind::VTableCast(*vtable)
    })
  }
}

impl TraitManagerExt for TraitManager {
  fn trait_manager(&self) -> &TraitManager { self }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait TraitManagerExt {
  fn trait_manager(&self) -> &TraitManager;

  fn cast<A, B>(&self, value: A) -> Result<B, A> where
    TraitManager: Cast<A, B>
  {
    Cast::cast(self.trait_manager(), value)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
