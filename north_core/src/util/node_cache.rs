use {
  crate::{
    Node, NodeId,
    model::ModelCell,
    util::dyn_traits::{EqDyn, HashDyn, PartialEqDyn},
  },
  std::{
    any::Any,
    collections::HashMap,
    cmp::{Eq, PartialEq},
    hash::{Hash, Hasher},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct NodeCache {
  model_cell: ModelCell,
  nodes: HashMap<Box<dyn NodeWrapper>, NodeId>
}

impl NodeCache {
  pub fn new(model_cell: ModelCell) -> Self {
    Self {
      model_cell,
      nodes: HashMap::new(),
    }
  }

  pub fn get<N>(&mut self, node: N) -> NodeId<N> where
    N: Clone + Eq + Hash + Node + PartialEq
  {
    if let Some(node_id) = self.get_cached(&node) {
      return node_id.cast();
    }

    let node_id = {
      let mut model = self.model_cell.borrow_mut();
      model.build_node(node.clone())
    };

    self.insert::<N>(node, node_id.cast());

    node_id
  }

  pub fn get_cached<N>(&self, node: &N) -> Option<NodeId> where
    N: Eq + Hash + Node + PartialEq
  {
    let key = node as &dyn NodeWrapper;
    self.nodes.get(key).cloned()
  }

  pub fn insert<N>(&mut self, node: N, node_id: NodeId) where
    N: Eq + Hash + Node + PartialEq
  {
    let key = box NodeWrapperCell(node);
    self.nodes.insert(key, node_id);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait NodeWrapper: EqDyn + HashDyn + PartialEqDyn {
  fn inner(&self) -> &dyn Any;
}

impl<T> NodeWrapper for T where
  T: 'static + EqDyn + HashDyn + PartialEqDyn
{
  default fn inner(&self) -> &dyn Any { self }
}

impl Eq for dyn NodeWrapper { }

impl Hash for dyn NodeWrapper {
  fn hash<H: Hasher>(&self, state: &mut H) {
    HashDyn::hash_dyn(self, state);
  }
}

impl PartialEq for dyn NodeWrapper {
  fn eq(&self, other: &dyn NodeWrapper) -> bool {
    PartialEqDyn::eq_dyn(self, other.inner())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct NodeWrapperCell<T>(T) where
  T: 'static + Eq + Hash + Node + PartialEq;

impl<T> NodeWrapper for NodeWrapperCell<T> where
  T: 'static + Eq + Hash + Node + PartialEq
{
  fn inner(&self) -> &dyn Any { &self.0 }
}

////////////////////////////////////////////////////////////////////////////////////////////////
