use {
  crate::{
    Model, Node,
    node_id::{NodeId, ToNodeId},
    iter::ModelIterator,
  },
  std::marker::PhantomData,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct CastId<'m, I, N: ?Sized> {
  inner: I,
  model_life: PhantomData<&'m ()>,
  node_type: PhantomData<N>,
}

impl<'m, I, N> CastId<'m, I, N> where
  I: ModelIterator<'m>, I::Item: ToNodeId, N: Node + ?Sized
{
  pub fn new(inner: I) -> Self {
    Self { inner, model_life: PhantomData, node_type: PhantomData }
  }
}

impl<'m, I, N> Iterator for CastId<'m, I, N> where
  I: ModelIterator<'m>, I::Item: ToNodeId, N: Node + ?Sized
{
  type Item = NodeId<N>;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      let id = self.inner.next()?.to_top();
      let id = self.inner.model().cast_id(id);
      if id.is_some() { return id; }
    }
  }
}

impl<'m, I, N> ModelIterator<'m> for CastId<'m, I, N> where
  I: ModelIterator<'m>, I::Item: ToNodeId, N: Node + ?Sized
{
  fn model(&self) -> &'m Model {
    self.inner.model()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
