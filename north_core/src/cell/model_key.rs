use {
  crate::{
    cell::{Cell, DefaultInit, Key, KeyBuilder, ManualInit, ToScope},
    model::ModelCell,
  },
  std::{
    any::TypeId,
    marker::PhantomData
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ModelKey<K, V, P = ManualInit> {
  key_id: TypeId,
  key_type: PhantomData<K>,
  value_type: PhantomData<V>,
  init_policy: PhantomData<P>,
}

impl<K, V, P> ModelKey<K, V, P> where
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

impl<K, V, P> Cell for ModelKey<K, V, P> where
  K: ToScope, V: 'static + Clone
{
  type Key = (ModelCell, K);
  type KeyBuilder = KeyBuilder<(ModelCell, K)>;
  type Value = V;

  fn key_b(&self) -> Self::KeyBuilder {
    KeyBuilder(PhantomData)
  }

  fn set(&self, (model, key): &Self::Key, value: Self::Value) {
    let mut model = model.borrow_mut();
    let key = self.map_key(&key);
    model.cell_map.set(key, value);
  }

  default fn try_get(&self, _key: &Self::Key) -> Option<V> {
    unimplemented!()
  }
}

impl<K, V> Cell for ModelKey<K, V, DefaultInit> where
  K: ToScope, V: 'static + Clone + Default
{
  fn try_get(&self, (model, key): &Self::Key) -> Option<V> {
    let mut model = model.borrow_mut();
    let key = self.map_key(&key);
    unsafe { Some(model.cell_map.get_or_default::<V>(key)) }
  }
}

impl<K, V> Cell for ModelKey<K, V, ManualInit> where
  K: ToScope, V: 'static + Clone
{
  fn try_get(&self, (model, key): &Self::Key) -> Option<V> {
    let model = model.borrow();
    let key = self.map_key(&key);
    unsafe { model.cell_map.get::<V>(key) }
  }
}

unsafe impl<K, V, P> Send for ModelKey<K, V, P> { }
unsafe impl<K, V, P> Sync for ModelKey<K, V, P> { }

////////////////////////////////////////////////////////////////////////////////////////////////
