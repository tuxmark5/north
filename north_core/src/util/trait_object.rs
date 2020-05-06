use {
  std::{
    mem,
    marker::Unsize,
    ptr,
    raw::TraitObject,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn extract_data<T: ?Sized>(fat_ptr: &T) -> *mut () {
  let trait_object = unsafe { mem::transmute_copy::<_, TraitObject>(&fat_ptr) };
  trait_object.data
}

pub fn extract_vtable<T: ?Sized>(fat_ptr: *const T) -> *mut () {
  let trait_object = unsafe { mem::transmute_copy::<_, TraitObject>(&fat_ptr) };
  trait_object.vtable
}

pub unsafe fn make_object<'a, T: ?Sized>(data: *mut (), vtable: *mut ()) -> &'a T {
  let trait_object = TraitObject { data, vtable };
  mem::transmute_copy::<_, &T>(&trait_object)
}

pub unsafe fn make_object_mut<'a, T: ?Sized>(data: *mut (), vtable: *mut ()) -> &'a mut T {
  let trait_object = TraitObject { data, vtable };
  mem::transmute_copy::<_, &mut T>(&trait_object)
}

pub fn vtable_of<A, T>() -> *mut () where
  A: Unsize<T> + 'static,
  T: ?Sized + 'static
{
  let object: *const A = ptr::null();
  let dyn_object: *const T = object;
  extract_vtable(dyn_object)
}

////////////////////////////////////////////////////////////////////////////////////////////////
