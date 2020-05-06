use {
  crate::{
    Model,
    iter::ModelIterator,
    trait_manager::{TraitManager, TraitManagerExt},
    util::cast::Cast,
  },
  std::{
    marker::PhantomData,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct FilterCast<'m, I, T> {
  inner: I,
  model_life: PhantomData<&'m ()>,
  target_type: PhantomData<T>,
}

impl<'m, I, T> FilterCast<'m, I, T> where
  I: ModelIterator<'m>, TraitManager: Cast<I::Item, T>
{
  pub fn new(inner: I) -> Self {
    Self { inner, model_life: PhantomData, target_type: PhantomData }
  }
}

impl<'m, I, T> Iterator for FilterCast<'m, I, T> where
  I: ModelIterator<'m>, TraitManager: Cast<I::Item, T>
{
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      let value = self.inner.next()?;
      let value = self.inner.model().cast(value).ok();
      if value.is_some() { return value; }
    }
  }
}

impl<'m, I, T> ModelIterator<'m> for FilterCast<'m, I, T> where
  I: ModelIterator<'m>, TraitManager: Cast<I::Item, T>
{
  fn model(&self) -> &'m Model {
    self.inner.model()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
