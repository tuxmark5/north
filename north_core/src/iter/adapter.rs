use {
  crate::{
    Model,
    iter::ModelIterator,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Adapter<'m, I> {
  model: &'m Model,
  inner: I,
}

impl<'m, I> Adapter<'m, I> {
  pub fn new(model: &'m Model, inner: I) -> Self {
    Self { model, inner }
  }
}

impl<'m, I> Iterator for Adapter<'m, I> where
  I: Iterator
{
  type Item = I::Item;

  fn next(&mut self) -> Option<Self::Item> {
    self.inner.next()
  }
}

impl<'m, I> ModelIterator<'m> for Adapter<'m, I> where
  I: Iterator
{
  fn model(&self) -> &'m Model {
    self.model
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
