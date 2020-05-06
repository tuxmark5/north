
use {
  crate::{
    Model, Node,
    iter::ModelIterator,
    node_id::ToNodeId,
    structure::storage::StorageExt,
  },
  std::{
    cell::Ref,
    marker::PhantomData,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BorrowNodes<'m, I> {
  inner: I,
  model_life: PhantomData<&'m ()>,
}

impl<'m, I> BorrowNodes<'m, I> where
  I: ModelIterator<'m>, I::Item: ToNodeId
{
  pub fn new(inner: I) -> Self {
    Self { inner, model_life: PhantomData }
  }
}

impl<'m, I> Iterator for BorrowNodes<'m, I> where
  I: ModelIterator<'m>, I::Item: ToNodeId
{
  type Item = Ref<'m, dyn Node>;

  fn next(&mut self) -> Option<Self::Item> {
    let id = self.inner.next()?.to_top();
    let node = self.inner.model().data(id);
    Some(node)
  }
}

impl<'m, I> ModelIterator<'m> for BorrowNodes<'m, I> where
  I: ModelIterator<'m>, I::Item: ToNodeId
{
  fn model(&self) -> &'m Model {
    self.inner.model()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
