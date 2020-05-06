use {
  crate::{
    Model, NodeId,
    iter::ModelIterator,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Ancestors<'m> {
  model: &'m Model,
  current_node: NodeId,
}

impl<'m> Ancestors<'m> {
  pub fn new(model: &'m Model, node: NodeId) -> Self {
    Self {
      model,
      current_node: node
    }
  }
}

impl<'m> Iterator for Ancestors<'m> {
  type Item = NodeId;

  fn next(&mut self) -> Option<Self::Item> {
    let result = self.model.parent_id(self.current_node);

    if let Some(parent) = result {
      self.current_node = parent;
    }

    result
  }
}

impl<'m> ModelIterator<'m> for Ancestors<'m> {
  fn model(&self) -> &'m Model {
    self.model
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
