use {
  crate::{
    Model, Node, NodeId,
    iter::ModelIterator,
    model::{
      element::{ChildElem, Descendants},
      member::{Member}
    },
    structure::StorageExt,
    util::{
      chain::{Chain, ChainExt},
      dynamic_cast,
    }
  },
  std::{
    cell::Ref,
    iter::Enumerate,
    slice
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Children<'m> {
  pub model: &'m Model,
  pub inner: Chain<Ref<'m, dyn Node>, ChildrenRaw<'m>>
}

impl<'m> Children<'m> {
  pub fn new(model: &'m Model, parent: NodeId) -> Self {
    let concept_instance = model.data(parent);
    Self {
      model,
      inner: concept_instance.chain_map(|r| ChildrenRaw::new(r))
    }
  }
}

impl<'m> Iterator for Children<'m> {
  type Item = NodeId;

  fn next(&mut self) -> Option<Self::Item> {
    self.inner.next().map(|e| e.1)
  }
}

impl<'m> ModelIterator<'m> for Children<'m> {
  fn model(&self) -> &'m Model {
    self.model
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ChildrenRaw<'a> {
  pub node_data: &'a dyn Node,
  pub member_iter: Enumerate<slice::Iter<'a, &'a dyn Member>>,
  pub current_iter: Option<(usize, Descendants<'a>)>,
}

impl<'a> ChildrenRaw<'a> {
  pub fn new(node: &'a dyn Node) -> Self {
    let concept = node.instance_concept();
    let members = concept.members();

    Self {
      node_data: node,
      member_iter: members.iter().enumerate(),
      current_iter: None,
    }
  }
}

impl<'a> Iterator for ChildrenRaw<'a> {
  type Item = (usize, NodeId);

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      // Check current member
      let (link_id, elem) = match self.current_iter {
        Some((link_id, ref mut iter)) => (link_id, iter.next()),
        None => (0, None),
      };

      // Check if current member is exhausted
      match elem {
        Some(elem) => {
          if let Ok(child) = dynamic_cast::<_, &dyn ChildElem>(elem) {
            let node_id = child.as_node_id();
            break Some((link_id, *node_id));
          } else {
            continue;
          }
        },

        None => {
          self.current_iter = None;
        },
      }

      // Check next member
      match self.member_iter.next() {
        Some((link_id, member)) => {
          let element = member.element(self.node_data);
          self.current_iter = Some((link_id, Descendants::new(element)));
        },

        None => {
          break None
        },
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
