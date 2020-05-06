use {
  crate::{
    Node,
    node_id::{NodeId, ToNodeId},
  },
  std::{
    hash::{Hash, Hasher},
    fmt::{self, Debug},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Child<N: Node + ?Sized>(pub NodeId<N>);

impl<N: Node + ?Sized> Child<N> {
  pub fn idx(&self) -> usize {
    self.0.idx()
  }
}

impl<N: Node + ?Sized> Debug for Child<N> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "C#{:?}", self.0)
  }
}

impl<N: Node + ?Sized> Clone for Child<N> {
  fn clone(&self) -> Self {
    Child(self.0)
  }
}

impl<N: Node + ?Sized> Eq for Child<N> { }

impl<N: Node + ?Sized> From<NodeId<N>> for Child<N> {
  fn from(node_id: NodeId<N>) -> Self {
    Child(node_id)
  }
}

impl<N: Node + ?Sized> Hash for Child<N> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    Hash::hash(&self.0, state)
  }
}

impl<N: Node + ?Sized> PartialEq for Child<N> {
  fn eq(&self, other: &Child<N>) -> bool {
    PartialEq::eq(&self.0, &other.0)
  }
}

impl<N: Node + ?Sized> ToNodeId for Child<N> {
  type Node = N;
  fn to_node_id(&self) -> NodeId<N> { self.0 }
}

////////////////////////////////////////////////////////////////////////////////////////////////
