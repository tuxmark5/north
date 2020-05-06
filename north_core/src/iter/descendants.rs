use {
  crate::{
    Model, NodeId,
    iter::{Children, ModelIterator},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Descendants<'m> {
  model: &'m Model,
  traversal_stack: Vec<Children<'m>>,
}

impl<'m> Descendants<'m> {
  pub fn new(model: &'m Model, parent: NodeId) -> Self {
    let children = model.children_ids(parent);

    Self {
      model,
      traversal_stack: vec![children]
    }
  }
}

impl<'m> Iterator for Descendants<'m> {
  type Item = NodeId;

  fn next(&mut self) -> Option<Self::Item> {
    let child_id = loop {
      let child = match self.traversal_stack.last_mut() {
        Some(ref mut children) => children.next(),
        None => return None,
      };

      match child {
        Some(child_id) => break child_id,
        None => self.traversal_stack.pop(),
      };
    };

    let children_inner = self.model.children_ids(child_id);
    self.traversal_stack.push(children_inner);

    Some(child_id)
  }
}

impl<'m> ModelIterator<'m> for Descendants<'m> {
  fn model(&self) -> &'m Model {
    self.model
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
