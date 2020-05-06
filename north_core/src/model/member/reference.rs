use {
  crate::{
    Node,
    node_id::{NodeId, ToNodeId},
  },
  std::{
    fmt::{self, Debug},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Reference<N, T = ()> where
  N: Node + ?Sized
{
  pub target_node: Option<NodeId<N>>,
  pub ref_object: T,
}

impl<N, T> Reference<N, T> where
  N: Node + ?Sized
{
  pub fn idx(&self) -> usize {
    self.target().idx()
  }

  pub fn target(&self) -> NodeId<N> {
    self.target_node.unwrap()
  }
}

impl<N, T> Clone for Reference<N, T> where
  N: Node + ?Sized, T: Clone,
{
  fn clone(&self) -> Self {
    Self {
      target_node: self.target_node.clone(),
      ref_object: self.ref_object.clone(),
    }
  }
}

impl<N, T> Debug for Reference<N, T> where
  N: Node + ?Sized
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self.target_node {
      Some(node_id) => write!(f, "REF#{:?}", node_id),
      None => write!(f, "REF"),
    }
  }
}

impl<N, T> Eq for Reference<N, T> where
  N: Node + ?Sized, T: Eq { }

impl<N, A> From<A> for Reference<N, A> where
  N: Node + ?Sized
{
  fn from(ref_object: A) -> Self {
    Self {
      target_node: None,
      ref_object,
    }
  }
}

impl<A> From<NodeId<A>> for Reference<A, ()> where
  A: Node + ?Sized
{
  fn from(node_id: NodeId<A>) -> Self {
    Self {
      target_node: Some(node_id),
      ref_object: (),
    }
  }
}

impl<N, T> PartialEq for Reference<N, T> where
  N: Node + ?Sized, T: PartialEq
{
  fn eq(&self, other: &Self) -> bool {
    self.target_node.eq(&other.target_node) &&
      self.ref_object.eq(&other.ref_object)
  }
}

impl<N, T> ToNodeId for Reference<N, T> where
  N: Node + ?Sized
{
  type Node = N;
  fn to_node_id(&self) -> NodeId<N> { self.target_node.unwrap() }
}

////////////////////////////////////////////////////////////////////////////////////////////////
