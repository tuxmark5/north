use {
  crate::{
    model::{
      concept::Concept,
      member::Member,
    },
  },
  std::{
    marker::PhantomData
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ConceptDescr<N> {
  pub members: &'static [&'static dyn Member],
  pub name: &'static str,
  pub node_type: PhantomData<N>
}

impl<N> ConceptDescr<N> {
  pub const fn new(name: &'static str, members: &'static [&'static dyn Member]) -> Self {
    Self {
      members,
      name,
      node_type: PhantomData
    }
  }
}

impl<N> Concept for ConceptDescr<N> {
  fn members(&self) -> &[&dyn Member] {
    self.members
  }

  fn name(&self) -> &str {
    self.name
  }
}

unsafe impl<N> Send for ConceptDescr<N> { }
unsafe impl<N> Sync for ConceptDescr<N> { }

////////////////////////////////////////////////////////////////////////////////////////////////
