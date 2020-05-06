use {
  std::ptr
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(unions_with_drop_fields)]
pub union ManualInit<T> {
  empty: (),
  value: T,
}

impl<T> ManualInit<T> {
  pub unsafe fn drop(&mut self) {
    ptr::drop_in_place(&mut self.value)
  }

  pub unsafe fn get_mut(&mut self) -> &mut T {
    &mut self.value
  }

  pub unsafe fn init(&mut self, value: T) -> &mut T {
    ptr::write(&mut self.value, value);
    &mut self.value
  }

  pub unsafe fn take(&mut self) -> T {
    ptr::read(&mut self.value)
  }
}

impl<T> Default for ManualInit<T> {
  fn default() -> Self {
    Self { empty: () }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
