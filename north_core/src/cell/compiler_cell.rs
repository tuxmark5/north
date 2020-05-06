use {
  crate::{
    cell::{Cell, DefaultInit, Key, KeyBuilder, ManualInit, ToScope},
    compiler::COMPILER,
  },
  std::{
    any::TypeId,
    marker::PhantomData
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct CompilerCell<K, V, P = ManualInit> {
  key_id: TypeId,
  key_type: PhantomData<K>,
  value_type: PhantomData<V>,
  init_policy: PhantomData<P>,
}

impl<K, V, P> CompilerCell<K, V, P> where
  K: ToScope
{
  pub const fn new<M: 'static>() -> Self {
    Self {
      key_id: TypeId::of::<M>(),
      key_type: PhantomData,
      value_type: PhantomData,
      init_policy: PhantomData,
    }
  }

  pub fn map_key(&self, key: &K) -> Key {
    Key {
      key_id: self.key_id,
      scope: key.to_scope(),
    }
  }
}

impl<K, V, P> Cell for CompilerCell<K, V, P> where
  K: ToScope, V: 'static + Clone
{
  type Key = K;
  type KeyBuilder = KeyBuilder<K>;
  type Value = V;

  fn key_b(&self) -> Self::KeyBuilder {
    KeyBuilder(PhantomData)
  }

  fn set(&self, key: &Self::Key, value: Self::Value) {
    COMPILER.with(|comp| {
      let mut prop_map = comp.prop_map.borrow_mut();
      let key = self.map_key(key);
      prop_map.set(key, value);
    })
  }

  default fn try_get(&self, _key: &Self::Key) -> Option<V> {
    unimplemented!()
  }
}

impl<K, V> Cell for CompilerCell<K, V, DefaultInit> where
  K: ToScope, V: 'static + Clone + Default
{
  fn try_get(&self, key: &Self::Key) -> Option<V> {
    COMPILER.with(|comp| {
      let mut prop_map = comp.prop_map.borrow_mut();
      let key = self.map_key(key);
      unsafe { Some(prop_map.get_or_default::<V>(key)) }
    })
  }
}

impl<K, V> Cell for CompilerCell<K, V, ManualInit> where
  K: ToScope, V: 'static + Clone
{
  fn try_get(&self, key: &Self::Key) -> Option<V> {
    COMPILER.with(|comp| {
      let prop_map = comp.prop_map.borrow();
      let key = self.map_key(key);
      unsafe { prop_map.get::<V>(key) }
    })
  }
}

unsafe impl<K, V, P> Send for CompilerCell<K, V, P> { }
unsafe impl<K, V, P> Sync for CompilerCell<K, V, P> { }

////////////////////////////////////////////////////////////////////////////////////////////////
