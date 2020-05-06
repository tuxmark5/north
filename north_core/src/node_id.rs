use {
  crate::{
    Node,
    model::Model,
  },
  std::{
    cell::Ref,
    cmp::Ordering,
    hash::{Hash, Hasher},
    fmt::{self, Debug},
    marker::{PhantomData, Unsize},
    mem::transmute,
    num::NonZeroU32,
    //ops::CoerceUnsized,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ToNodeId: Sized {
  type Node: Node + ?Sized;

  fn to_node_id(&self) -> NodeId<Self::Node>;

  fn get<'m>(&self, model: &'m Model) -> Ref<'m, Self::Node> {
    model.node(self)
  }

  fn to_top(&self) -> NodeId {
    self.to_node_id().cast()
  }
}

impl<'a, A> ToNodeId for &'a A where
  A: ToNodeId
{
  type Node = A::Node;

  fn to_node_id(&self) -> NodeId<Self::Node> { (*self).to_node_id() }
}

////////////////////////////////////////////////////////////////////////////////////////////////

//#[fundamental]
pub struct NodeId<A: Node + ?Sized = dyn Node> {
  pub id: NonZeroU32,
  pub type_: PhantomData<A>
}

impl<A: Node + ?Sized> NodeId<A> {
  // TODO: get rid of this / replace invocations to `up` where possible
  pub fn cast<T: Node + ?Sized>(self) -> NodeId<T> {
    NodeId::<T> {
      id: self.id,
      type_: PhantomData
    }
  }

  pub fn cast_ref<T: Node + ?Sized>(&self) -> &NodeId<T> {
    unsafe { transmute(self) }
  }

  pub fn cast_ref_mut<T: Node + ?Sized>(&mut self) -> &mut NodeId<T> {
    unsafe { transmute(self) }
  }

  pub fn from_id(id: u32) -> NodeId<A> {
    assert_ne!(id, 0);
    NodeId {
      id: unsafe { NonZeroU32::new_unchecked(id) },
      type_: PhantomData
    }
  }

  pub fn idx(&self) -> usize {
    self.id.get() as usize
  }

  pub fn up<B>(self) -> NodeId<B> where
    A: Unsize<B>, B: Node + ?Sized
  {
    NodeId::<B> {
      id: self.id,
      type_: PhantomData
    }
  }
}

impl<A: Node + ?Sized> Clone for NodeId<A> {
  fn clone(&self) -> Self {
    Self {
      id: self.id,
      type_: PhantomData
    }
  }
}

//impl<A, B> CoerceUnsized<NodeId<B>> for NodeId<A> where
//  A: Unsize<B>, B: ?Sized { }

impl<A: Node + ?Sized> Copy for NodeId<A> { }

impl<A: Node + ?Sized> Debug for NodeId<A> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.id)
  }
}

impl<A: Node + ?Sized> Eq for NodeId<A> { }

impl<A: Node + ?Sized> From<NodeId<A>> for usize {
  fn from(id: NodeId<A>) -> Self {
    id.id.get() as usize
  }
}

/*impl<A, B> From<NodeId<A>> for NodeId<B> where
  A: Node + Unsize<B>, B: Node + ?Sized
{
  fn from(id: NodeId<A>) -> Self { id.cast() }
}*/

impl<A: Node + ?Sized> Hash for NodeId<A> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.id.hash(state);
  }
}

impl<A: Node + ?Sized> Ord for NodeId<A> {
  fn cmp(&self, other: &Self) -> Ordering {
    self.id.cmp(&other.id)
  }
}

impl<A: Node + ?Sized, B: Node + ?Sized> PartialEq<NodeId<B>> for NodeId<A> {
  fn eq(&self, other: &NodeId<B>) -> bool {
    self.id == other.id
  }
}

impl<A: Node + ?Sized> PartialOrd<NodeId<A>> for NodeId<A> {
  fn partial_cmp(&self, other: &NodeId<A>) -> Option<Ordering> {
    self.id.partial_cmp(&other.id)
  }
}

impl<N: Node + ?Sized> ToNodeId for NodeId<N> {
  type Node = N;
  fn to_node_id(&self) -> NodeId<N> { *self }
}

////////////////////////////////////////////////////////////////////////////////////////////////
