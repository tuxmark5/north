use {
  crate::{
    cell::Cell,
    util::chain::ChainRef,
  },
  std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ValueRef<T> = ChainRef<Rc<RefCell<T>>, Ref<'static, T>>;
pub type ValueRefMut<T> = ChainRef<Rc<RefCell<T>>, RefMut<'static, T>>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BorrowCell<C> {
  pub inner: C
}

impl<C, V> BorrowCell<C> where
  C: Cell<Value=Rc<RefCell<V>>>
{
  pub const fn new(inner: C) -> Self {
    Self { inner }
  }

  pub fn cell(&self, key: &C::Key) -> C::Value {
    self.inner.get(key)
  }

  pub fn get(&self, key: &C::Key) -> ValueRef<V> {
    let cell = self.inner.get(key);
    ChainRef::new(cell, |c| c.borrow())
  }

  pub fn get_mut(&self, key: &C::Key) -> ValueRefMut<V> {
    let cell = self.inner.get(key);
    ChainRef::new(cell, |c| c.borrow_mut())
  }

  pub fn get_mut_or<F>(&self, key: &C::Key, init: F) -> ValueRefMut<V> where
    F: FnOnce() -> V
  {
    let cell = match self.inner.try_get(key) {
      Some(cell) => cell,
      None => {
        self.set(key, init());
        self.inner.get(key)
      },
    };
    ChainRef::new(cell, |c| c.borrow_mut())
  }

  pub fn key_b(&self) -> C::KeyBuilder {
    self.inner.key_b()
  }

  pub fn set(&self, key: &C::Key, value: V) {
    let value = Rc::new(RefCell::new(value));
    self.inner.set(key, value);
  }

  pub fn try_get(&self, key: &C::Key) -> Option<ValueRef<V>> {
    let cell = self.inner.try_get(key)?;
    let result = ChainRef::new(cell, |c| c.borrow());
    Some(result)
  }

  pub fn try_get_mut(&self, key: &C::Key) -> Option<ValueRefMut<V>> {
    let cell = self.inner.try_get(key)?;
    let result = ChainRef::new(cell, |c| c.borrow_mut());
    Some(result)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
