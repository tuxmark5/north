use {
  crate::{
    cell::{
      macros::{cell_get_mut},
      prelude::*,
    },
    quote::{
      rust_type::*,
    },
    util::{
      unique_map::UniqueMap,
    }
  },
  north_derive::cell,
  std::{
    any::TypeId as NativeId,
    collections::HashMap,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[cell(borrow, default, scope(compiler))]
pub static RUST_TYPE_QUOTER: RustQuoter = ();

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct RustQuoter {
  native_map: HashMap<NativeId, TypeId>,
  storage: UniqueMap<TypeId, Type>
}

impl RustQuoter {
  pub fn new() -> Self {
    Self {
      native_map: HashMap::new(),
      storage: UniqueMap::new(),
    }
  }

  pub fn get(&self, type_id: TypeId) -> &Type {
    &**self.storage.get(type_id)
  }

  pub fn len(&self) -> usize {
    self.storage.len()
  }

  pub fn quote<T: QuoteType + ?Sized>(&mut self) -> TypeId {
    let native_id = NativeId::of::<T>();
    if let Some(type_id) = self.native_map.get(&native_id) {
      return *type_id;
    }

    let type_def = T::quote(self);
    let type_id = self.storage.insert_unique(type_def);

    type_id
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait QuoteType: 'static {
  fn quote(q: &mut RustQuoter) -> Type;
  fn quote_complete(_q: &mut RustQuoter, _ty: &mut Type) { }
}

impl QuoteType for i8 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::I8) }
}

impl QuoteType for i16 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::I16) }
}

impl QuoteType for i32 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::I32) }
}

impl QuoteType for i64 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::I64) }
}

impl QuoteType for u8 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::U8) }
}

impl QuoteType for u16 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::U16) }
}

impl QuoteType for u32 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::U32) }
}

impl QuoteType for u64 {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::U64) }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl QuoteType for bool {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::U8) }
}

impl QuoteType for isize {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::I64) }
}

impl QuoteType for usize {
  fn quote(_q: &mut RustQuoter) -> Type { Type::Prim(TypePrim::U64) }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<T: QuoteType + ?Sized> QuoteType for *const T {
  fn quote(q: &mut RustQuoter) -> Type {
    let inner = q.quote::<T>();
    Type::Ptr(TypePtr { mutable: false, inner })
  }
}

impl<T: QuoteType + ?Sized> QuoteType for *mut T {
  fn quote(q: &mut RustQuoter) -> Type {
    let inner = q.quote::<T>();
    Type::Ptr(TypePtr { mutable: true, inner })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<T: QuoteType + ?Sized> QuoteType for &'static T {
  fn quote(q: &mut RustQuoter) -> Type {
    let inner = q.quote::<T>();
    Type::Ptr(TypePtr { mutable: false, inner })
  }
}

impl<T: QuoteType + ?Sized> QuoteType for &'static mut T {
  fn quote(q: &mut RustQuoter) -> Type {
    let inner = q.quote::<T>();
    Type::Ptr(TypePtr { mutable: true, inner })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<T: QuoteType> QuoteType for &'static [T] {
  fn quote(q: &mut RustQuoter) -> Type {
    <(*const T, usize)>::quote(q)
  }
}

impl<T: QuoteType> QuoteType for &'static mut [T] {
  fn quote(q: &mut RustQuoter) -> Type {
    <(*mut T, usize)>::quote(q)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl QuoteType for () {
  fn quote(_q: &mut RustQuoter) -> Type {
    Type::Unit
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! impl_quote_tuple {
  ($($param:ident),*) => {
    impl<$($param),*> QuoteType for ($($param),*) where
      $($param: QuoteType),*
    {
      fn quote(q: &mut RustQuoter) -> Type {
        let elements = vec![$(q.quote::<$param>()),*];
        Type::Tuple(TypeTuple { elements })
      }
    }
  }
}

impl_quote_tuple!(A, B);
impl_quote_tuple!(A, B, C);
impl_quote_tuple!(A, B, C, D);
impl_quote_tuple!(A, B, C, D, E);
impl_quote_tuple!(A, B, C, D, E, F);
impl_quote_tuple!(A, B, C, D, E, F, G);

////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! impl_quote_fn {
  ($($param:ident),*) => {
    impl<$($param,)* R> QuoteType for extern fn($($param),*) -> R where
      $($param: QuoteType,)* R: QuoteType
    {
      fn quote(q: &mut RustQuoter) -> Type {
        let params = vec![$(q.quote::<$param>()),*];
        let result = q.quote::<R>();
        Type::Fn(TypeFn { params, result })
      }
    }
  }
}

impl_quote_fn!();
impl_quote_fn!(A);
impl_quote_fn!(A, B);
impl_quote_fn!(A, B, C);
impl_quote_fn!(A, B, C, D);
impl_quote_fn!(A, B, C, D, E);
impl_quote_fn!(A, B, C, D, E, F);

////////////////////////////////////////////////////////////////////////////////////////////////

/*impl<T: QuoteType> QuoteType for raw::Slice<T> {
  fn quote(q: &mut RustQuoter) -> Type {
    <(*const T, usize)>::quote(q)
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn quote_type<T: QuoteType>() -> TypeId {
  let mut quoter = cell_get_mut!(RUST_TYPE_QUOTER);
  quoter.quote::<T>()
}

pub fn quote_type_val<T: QuoteType>(_v: &T) -> TypeId {
  let mut quoter = cell_get_mut!(RUST_TYPE_QUOTER);
  quoter.quote::<T>()
}

////////////////////////////////////////////////////////////////////////////////////////////////
