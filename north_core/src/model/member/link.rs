use {
  crate::{
    Node,
    node_id::{NodeId, ToNodeId},
  },
  std::{
    hash::{Hash, Hasher},
    fmt::{self, Debug},
    marker::Unsize,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy)]
pub struct Link<N: Node + ?Sized> {
  pub node_id: NodeId<N>,
  pub owned: bool,
}

impl<A: Node + ?Sized> Link<A> {
  pub fn cast<B>(self) -> Link<B> where
    B: Node + ?Sized
  {
    Link {
      node_id: self.node_id.cast(),
      owned: self.owned,
    }
  }

  pub fn idx(&self) -> usize {
    self.node_id.idx()
  }

  pub fn up<B>(self) -> Link<B> where
    A: Unsize<B>, B: Node + ?Sized
  {
    Link {
      node_id: self.node_id.up(),
      owned: self.owned,
    }
  }
}

impl<N: Node + ?Sized> Clone for Link<N> {
  fn clone(&self) -> Self {
    Self {
      node_id: self.node_id,
      owned: self.owned
    }
  }
}

impl<N: Node + ?Sized> Debug for Link<N> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "L#{:?}", self.node_id)
  }
}

impl<N: Node + ?Sized> Eq for Link<N> { }

impl<A: Node + ?Sized> From<NodeId<A>> for Link<A> {
  fn from(node_id: NodeId<A>) -> Self {
    Link { node_id, owned: false }
  }
}

impl<N: Node + ?Sized> Hash for Link<N> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    Hash::hash(&self.node_id, state)
  }
}

impl<N: Node + ?Sized> PartialEq for Link<N> {
  fn eq(&self, other: &Link<N>) -> bool {
    PartialEq::eq(&self.node_id, &other.node_id)
  }
}

impl<N: Node + ?Sized> ToNodeId for Link<N> {
  type Node = N;
  fn to_node_id(&self) -> NodeId<N> { self.node_id }
}

////////////////////////////////////////////////////////////////////////////////////////////////
