use {
  crate::{
    Node, NodeId,
    model::member::{Member, MemberDescr},
    util::downcast::*,
  },
  std::{
    any::{TypeId},
    iter,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ChildLink: Member {
  fn link_nodes<'a>(&self, object: &'a dyn Node)
    -> Box<dyn Iterator<Item=NodeId> + 'a>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O, N> Downcast for MemberDescr<O, NodeId<N>> where
  O: 'static, N: Node + ?Sized
{
  impl_downcast!(dyn ChildLink);
}

impl<O, N> ChildLink for MemberDescr<O, NodeId<N>> where
  O: 'static, N: Node + ?Sized,
{
  fn link_nodes<'a>(&self, object: &'a dyn Node)
    -> Box<dyn Iterator<Item=NodeId> + 'a>
  {
    let node_id: NodeId = self.data(object).cast();
    box iter::once(node_id)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O, N> Downcast for MemberDescr<O, Option<NodeId<N>>> where
  O: 'static, N: Node + ?Sized
{
  impl_downcast!(dyn ChildLink);
}

impl<O, N> ChildLink for MemberDescr<O, Option<NodeId<N>>> where
  O: 'static, N: Node + ?Sized,
{
  fn link_nodes<'a>(&self, parent: &'a dyn Node)
    -> Box<dyn Iterator<Item=NodeId> + 'a>
  {
    let node_id = *self.data(parent);
    box node_id.map(|n| n.cast()).into_iter()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O, N> Downcast for MemberDescr<O, Vec<NodeId<N>>> where
  O: 'static, N: Node + ?Sized
{
  impl_downcast!(dyn ChildLink);
}

impl<O, N> ChildLink for MemberDescr<O, Vec<NodeId<N>>> where
  O: 'static, N: Node + ?Sized,
{
  fn link_nodes<'a>(&self, object: &'a dyn Node)
    -> Box<dyn Iterator<Item=NodeId> + 'a>
  {
    box self.data(object).iter()
      .map(|x| x.cast())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
