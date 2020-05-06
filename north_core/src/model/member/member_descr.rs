use {
  crate::{
    Node,
    model::{
      element::Element,
      member::Member
    },
    util::{
      downcast::{Downcast, DowncastEntry},
      trait_object::{self, vtable_of},
    }
  },
  std::{
    any::{Any, TypeId},
    fmt::Debug,
    marker::{PhantomData, Unsize},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct MemberDescr<O, T> {
  pub name: &'static str,
  pub field_offset: fn() -> usize,
  pub object_type: PhantomData<O>,
  pub target_type: PhantomData<T>,
}

impl <O, T> MemberDescr<O, T> where
  O: 'static,
  T: 'static
{
  /*pub const fn new(name: &'static str, delta: &'static T) -> Self {
    Self {
      name,
      field_offset: union_cast::<&'static T, usize>(delta),
      object_type: PhantomData,
      target_type: PhantomData,
    }
  }*/

  fn assert_object_type<'a>(&self, object: &'a dyn Node) {
    if TypeId::of::<O>() != (*object).type_id() {
      panic!("object member type mismatch")
    }
  }

  pub fn data<'a>(&self, object: &'a dyn Node) -> &'a T {
    self.assert_object_type(object);
    unsafe {
      &*(self.data_ptr(object) as *const T)
    }
  }

  pub fn data_mut<'a>(&self, object: &'a mut dyn Node) -> &'a mut T {
    self.assert_object_type(object);
    unsafe {
      &mut *(self.data_ptr(object) as *mut T)
    }
  }

  fn data_ptr<'a>(&self, object: &'a dyn Node) -> *mut () {
    let object_ptr = trait_object::extract_data(object);
    let field_ptr = (object_ptr as usize) + (self.field_offset)();
    field_ptr as *mut ()
  }

  fn make_object<'a, Q>(&self, node: &'a dyn Node) -> &'a Q where
    T: 'static + Unsize<Q>, Q: 'static + ?Sized
  {
    self.assert_object_type(node);

    unsafe {
      let vtable = vtable_of::<T, Q>();
      let data = self.data_ptr(node);
      trait_object::make_object::<Q>(data, vtable)
    }
  }

  fn make_object_mut<'a, Q>(&self, node: &'a mut dyn Node) -> &'a mut Q where
    T: 'static + Unsize<Q>, Q: 'static + ?Sized
  {
    self.assert_object_type(node);

    unsafe {
      let vtable = vtable_of::<T, Q>();
      let data = self.data_ptr(node);
      trait_object::make_object_mut::<Q>(data, vtable)
    }
  }
}

impl<O: 'static, N: 'static> Downcast for MemberDescr<O, N> {
  default fn query_vtable(&self, _id: TypeId) -> Option<DowncastEntry> {
    None
  }
}

impl<O, T> Member for MemberDescr<O, T> where
  O: 'static,
  T: Debug + Element + 'static
{
  fn debug<'a>(&self, node: &'a dyn Node) -> &'a dyn Debug {
    self.make_object::<dyn Debug>(node)
  }

  fn element<'a>(&self, node: &'a dyn Node) -> &'a dyn Element {
    self.make_object::<dyn Element>(node)
  }

  fn element_mut<'a>(&self, node: &'a mut dyn Node) -> &'a mut dyn Element {
    self.make_object_mut::<dyn Element>(node)
  }

  fn get<'a>(&self, node: &'a dyn Node) -> &'a dyn Any {
    self.make_object::<dyn Any>(node)
  }

  fn get_mut<'a>(&self, node: &'a mut dyn Node) -> &'a mut dyn Any {
    self.make_object_mut::<dyn Any>(node)
  }

  fn name(&self) -> &'static str {
    self.name
  }

  fn object_type(&self) -> TypeId {
    TypeId::of::<O>()
  }

  fn target_type(&self) -> TypeId {
    TypeId::of::<T>()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

union Transmute<S: Copy, D: Copy> {
  src: S,
  dst: D
}

pub const fn phantom<T: 'static>() -> &'static T {
  union_cast::<usize, &'static T>(0)
}

pub const fn phantom2<T: 'static + Sized>() -> &'static T {
  union_cast::<usize, &'static T>(0)
}

pub const fn union_cast<S, D>(src: S) -> D where
  S: Copy,
  D: Copy
{
  unsafe {
    Transmute::<S, D> { src }.dst
  }
}

// #[macro_export]
// macro_rules! field_offset_of {
//   ($parent:ty, $inner:tt) => {
//     $crate::union_cast::<_, usize>(&$crate::phantom::<$parent>().$inner)
//   }
// }

#[macro_export]
macro_rules! field_offset_of {
  ($parent:ty, $field:tt) => { 
    || -> usize {
      let val = std::mem::MaybeUninit::<$parent>::uninit();
      let base_ptr = val.as_ptr();
      #[allow(unused_unsafe)] // for when the macro is used in an unsafe block
      let field_ptr = unsafe { &(*base_ptr).$field as *const _ };
      let offset = (field_ptr as usize) - (base_ptr as usize);
      offset
    }
  }
}

// struct Foo { x: u32, y: u32, z: u32 }
// //static xxx: usize = field_offset_of!(Foo, y);

// static ZZZ: fn() -> usize = || field_offset_of!(Foo, y);

#[macro_export]
macro_rules! field_offset_of4 {
  ($parent:ty, $field:tt) => {{
    // Make sure the field actually exists. This line ensures that a
    // compile-time error is generated if $field is accessed through a
    // Deref impl.
    // let $parent { $field: _, .. };

    // Create an instance of the container and calculate the offset to its field.
    // Here we're using an uninitialized instance of $parent. We avoid UB
    // by only using raw pointers that point to real (allocated, albeit uninitialized) memory.
    let val = std::mem::MaybeUninit::<$parent>::uninit();
    let base_ptr = val.as_ptr();
    #[allow(unused_unsafe)] // for when the macro is used in an unsafe block
    let field_ptr = unsafe { &(*base_ptr).$field as *const _ };
    let offset = (field_ptr as usize) - (base_ptr as usize);
    offset
  }}
}

#[macro_export]
macro_rules! field_offset_of3 {
  ($parent:ty, $field:tt) => {{ 0 }}
}

////////////////////////////////////////////////////////////////////////////////////////////////
