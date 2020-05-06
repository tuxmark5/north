use {
  crate::{
    util::trait_object,
  },
  std::{
    any::{Any, TypeId},
    cell::{Ref, RefCell, RefMut},
    marker::Unsize,
    mem,
    ops::Deref,
    raw::TraitObject,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Cast<A, B> {
  fn cast(&self, value: A) -> Result<B, A>;
}

impl<'a, A, B, C> Cast<&'a A, &'a B> for C where
  A: ?Sized, B: ?Sized, C: CastRef<A, B>
{
  fn cast(&self, value: &'a A) -> Result<&'a B, &'a A> {
    cast_ref_map(self, value, |value, kind| unsafe {
      let raw_a = value as *const A;
      let raw_b = kind.apply(raw_a);
      &*raw_b
    })
  }
}

impl<'a, A, B, C> Cast<&'a mut A, &'a mut B> for C where
  A: ?Sized, B: ?Sized, C: CastRef<A, B>
{
  fn cast(&self, value: &'a mut A) -> Result<&'a mut B, &'a mut A> {
    cast_ref_map(self, value, |value, kind| unsafe {
      let raw_a = value as *mut A;
      let raw_b = kind.apply_mut(raw_a);
      &mut *raw_b
    })
  }
}

impl<A, B, C> Cast<Box<A>, Box<B>> for C where
  A: ?Sized, B: ?Sized, C: CastRef<A, B>
{
  fn cast(&self, value: Box<A>) -> Result<Box<B>, Box<A>> {
    cast_ref_map(self, value, |value, kind| unsafe {
      let raw_a = Box::into_raw(value);
      let raw_b = kind.apply_mut(raw_a);
      Box::from_raw(raw_b)
    })
  }
}

impl<A, B, C> Cast<Rc<A>, Rc<B>> for C where
  A: ?Sized, B: ?Sized, C: CastRef<A, B>
{
  fn cast(&self, value: Rc<A>) -> Result<Rc<B>, Rc<A>> {
    cast_ref_map(self, value, |value, kind| unsafe {
      let raw_a = Rc::into_raw(value);
      let raw_b = kind.apply(raw_a);
      Rc::from_raw(raw_b)
    })
  }
}

impl<'a, A, B, C> Cast<Ref<'a, A>, Ref<'a, B>> for C where
  A: ?Sized, B: ?Sized, C: CastRef<A, B>
{
  fn cast(&self, value: Ref<'a, A>) -> Result<Ref<'a, B>, Ref<'a, A>> {
    cast_ref_map(self, value, |value, kind| {
      Ref::map(value, |ptr| unsafe {
        let raw_a = ptr as *const A;
        let raw_b = kind.apply(raw_a);
        &*raw_b
      })
    })
  }
}

impl<'a, A, B, C> Cast<RefMut<'a, A>, RefMut<'a, B>> for C where
  A: ?Sized, B: ?Sized, C: CastRef<A, B>
{
  fn cast(&self, value: RefMut<'a, A>) -> Result<RefMut<'a, B>, RefMut<'a, A>> {
    cast_ref_map(self, value, |value, kind| {
      RefMut::map(value, |ptr| unsafe {
        let raw_a = ptr as *mut A;
        let raw_b = kind.apply_mut(raw_a);
        &mut *raw_b
      })
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait CastRaw<A, B> where // Unsafe?
  A: ?Sized, B: ?Sized
{
  fn cast_raw<'a>(&self, value: &'a A) -> Option<CastKind>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait CastRef<A, B> where // Unsafe?
  A: ?Sized, B: ?Sized
{
  fn cast_ref<'a>(&self, value: &'a A) -> Option<CastKind>;
}

impl<A, B, C> CastRef<A, B> for C where
  A: ?Sized, B: ?Sized, C: CastRaw<A, B>
{
  default fn cast_ref<'a>(&self, value: &'a A) -> Option<CastKind> {
    self.cast_raw(value)
  }
}

impl<A, B, C> CastRef<RefCell<A>, RefCell<B>> for C where
  A: ?Sized, B: ?Sized, C: CastRaw<A, B> + CastRaw<RefCell<A>, RefCell<B>>
{
  fn cast_ref<'a>(&self, value: &'a RefCell<A>) -> Option<CastKind> {
    let inner = value.borrow();
    self.cast_raw(&*inner)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait CastSelf {
  fn cast_self(&self, _id: TypeId) -> Option<CastKind> { None }
}

impl<A> CastSelf for A {
  default fn cast_self(&self, _id: TypeId) -> Option<CastKind> { None }
}

#[macro_export]
macro_rules! impl_cast_self {
  ($trait_type:ty) => {
    fn cast_self(&self, id: TypeId) -> Option<CastKind> {
      if id == TypeId::of::<$trait_type>() {
        return Some(CastKind::new_vtable_cast::<Self, $trait_type>());
      }
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum CastKind {
  CompleteCast(TraitObject),
  DirectCast,
  VTableCast(*mut ()),
}

impl CastKind {
  pub fn new_vtable_cast<A, T>() -> Self where
    A: 'static + Unsize<T>, T: 'static + ?Sized
  {
    let vtable = trait_object::vtable_of::<A, T>();
    CastKind::VTableCast(vtable)
  }

  pub unsafe fn apply<A, B>(&self, value: *const A) -> *const B where
    A: ?Sized, B: ?Sized
  {
    let fat_ptr = self.make_trait_object::<A>(value);
    mem::transmute_copy::<_, *const B>(&fat_ptr)
  }

  pub unsafe fn apply_mut<A, B>(&self, value: *mut A) -> *mut B where
    A: ?Sized, B: ?Sized
  {
    let fat_ptr = self.make_trait_object::<A>(value);
    mem::transmute_copy::<_, *mut B>(&fat_ptr)
  }

  fn extract_data<T: ?Sized>(fat_ptr: *const T) -> *mut () {
    let trait_object = unsafe { mem::transmute_copy::<_, TraitObject>(&fat_ptr) };
    trait_object.data
  }

  fn extract_trait_object<T: ?Sized>(fat_ptr: *const T) -> TraitObject {
    unsafe { mem::transmute_copy::<_, TraitObject>(&fat_ptr) }
  }

  fn make_trait_object<A: ?Sized>(&self, value: *const A) -> TraitObject {
    match self {
      CastKind::CompleteCast(_result) => {
        panic!("invalid complete cast")
      },

      CastKind::DirectCast => {
        Self::extract_trait_object(value)
      },

      CastKind::VTableCast(vtable) => TraitObject {
        data: Self::extract_data(value),
        vtable: *vtable,
      },
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DefaultCaster;

impl<A, B> CastRaw<A, B> for DefaultCaster where
  A: Any + ?Sized,
  B: 'static + ?Sized
{
  default fn cast_raw<'a>(&self, value: &'a A) -> Option<CastKind> {
    if TypeId::of::<A>() == TypeId::of::<B>() {
      Some(CastKind::DirectCast)
    } else if (*value).type_id() == TypeId::of::<B>() {
      Some(CastKind::DirectCast)
    } else {
      None
    }
  }
}

impl<A, B> CastRaw<A, B> for DefaultCaster where
  A: Any + CastSelf + ?Sized,
  B: 'static + ?Sized
{
  default fn cast_raw<'a>(&self, value: &'a A) -> Option<CastKind> {
    let target_type = TypeId::of::<B>();
    if TypeId::of::<A>() == target_type {
      Some(CastKind::DirectCast)
    } else if (*value).type_id() == target_type {
      Some(CastKind::DirectCast)
    } else {
      value.cast_self(target_type)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn cast_ref_map<A, B, C, F, X, Y>(caster: &C, value: A, f: F) -> Result<B, A> where
  A: Deref<Target=X>,
  B: Deref<Target=Y>,
  C: CastRef<X, Y>,
  F: FnOnce(A, CastKind) -> B,
  X: ?Sized, Y: ?Sized
{
  match caster.cast_ref(value.deref()) {
    Some(kind) => Ok(f(value, kind)),
    None => Err(value),
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn dynamic_cast<A, B>(a: A) -> Result<B, A> where
  DefaultCaster: Cast<A, B>
{
  DefaultCaster.cast(a)
}

////////////////////////////////////////////////////////////////////////////////////////////////
