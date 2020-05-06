use {
  crate::{
    model::{Model, Node},
    node_id::{NodeId, ToNodeId},
  },
  std::{
    cell::Ref,
    ops::Deref,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type MRef<'a, N> = M<'a, Ref<'a, N>>;
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct M<'a, T> {
  model: &'a Model,
  value: T,
}

impl<'a, A: 'a> M<'a, A> {
  pub fn new(model: &'a Model, value: A) -> Self {
    Self { model, value }
  }

  pub fn map<F, B>(&'a self, f: F) -> M<'a, B> where
    F: FnOnce(&'a A) -> B, B: 'a
  {
    M {
      model: self.model,
      value: f(&self.value),
    }
  }

  pub fn map_opt<F, B>(&self, f: F) -> Option<M<'a, B>> where
    F: FnOnce(&A) -> Option<B>
  {
    match f(&self.value) {
      Some(value) => Some(M { model: self.model, value }),
      None => None,
    }
  }

  pub fn replace_value<B>(self, value: B) -> M<'a, B> {
    M { model: self.model, value }
  }
}

impl<'a, A: Iterator> M<'a, A> where
  A::Item: 'a
{
  pub fn find_map<F, B>(mut self, f: F) -> Option<M<'a, B>> where
    F: Fn(M<'a, A::Item>) -> Option<B>
  {
    let model = self.model;
    let f2 = |a| f(M::new(model, a));
    match self.value.find_map(f2) {
      Some(value) => Some(M { model, value }),
      None => None,
    }
  }
}

impl<'a, A: Node + ?Sized> M<'a, NodeId<A>> {
  pub fn ancestor_id<B: Node>(self) -> Option<M<'a, NodeId<B>>> {
    match self.model.find_ancestor::<_, B>(self.value) {
      Some(ancestor_id) => Some(self.replace_value(ancestor_id)),
      None => None,
    }
  }

  pub fn data(self) -> M<'a, Ref<'a, A>> {
    M {
      value: self.model.node(self.value),
      model: self.model,
    }
  }
}

impl<'a, A: ToNodeId> M<'a, A> {
  pub fn try_as<B: Node + ?Sized>(self) -> Option<M<'a, Ref<'a, B>>> {
    match self.model.try_get::<_, B>(self.value) {
      Some(value) => Some(M { model: self.model, value }),
      None => None,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<'a, A> Deref for M<'a, A> {
  type Target = A;

  fn deref(&self) -> &A {
    &self.value
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ModelValue<'a, T> {
  model: Ref<'a, Model>,
  value: T,
}

impl<'a, A> ModelValue<'a, A> {
  pub fn new(model: Ref<'a, Model>, value: A) -> Self {
    Self { model, value }
  }

  pub fn map<F, B>(self, f: F) -> ModelValue<'a, B> where
    F: FnOnce(A) -> B
  {
    ModelValue { 
      model: self.model, 
      value: f(self.value),
    }
  }

  pub fn replace_value<B>(self, value: B) -> ModelValue<'a, B> {
    ModelValue { model: self.model, value }
  }
}

impl<'a, A: Node> ModelValue<'a, NodeId<A>> {
  pub fn ancestor_id<B: Node>(self) -> Option<ModelValue<'a, NodeId<B>>> {
    match self.model.find_ancestor::<_, B>(self.value) {
      Some(ancestor_id) => Some(self.replace_value(ancestor_id)),
      None => None,
    }
  }

  // pub fn data(self) -> ModelValue<'a, Ref<'a, A>> {
  //   ModelValue {
  //     value: self.model.node(self.value),
  //     model: self.model,
  //   }
  //   // let new_value = self.model.node(self.value);
  //   // self.replace_value(new_value)
  // }

  pub fn tap<F, B>(self, f: F) -> ModelValue<'a, B> where
    F: FnOnce(&A) -> B
  {
    let new_value = {
      let node_id = self.value;
      let node = self.model.node(node_id);
      f(&*node)
    };
    self.replace_value(new_value)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
