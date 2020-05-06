use {
  crate::{
    Node, NodeId,
    model::member::{
      Member, Reference,
      member_descr::MemberDescr
    },
    util::downcast::{
      Downcast, DowncastEntry
    }
  },
  std::{
    any::{Any, TypeId},
    fmt::{Debug},
    mem
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ReferenceLink: Member {
  fn reference_obj<'a>(&self, object: &'a dyn Node) -> &'a dyn Any;
  fn reference_obj_debug<'a>(&self, object: &'a dyn Node) -> &'a dyn Debug;
  fn reference_obj_mut<'a>(&self, object: &'a mut dyn Node) -> &'a mut dyn Any;
  fn target_node<'a>(&self, object: &'a dyn Node) -> &'a Option<NodeId>;
  fn target_node_mut<'a>(&self, object: &'a mut dyn Node) -> &'a mut Option<NodeId>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O, N, T> Downcast for MemberDescr<O, Reference<N, T>> where
  O: 'static, N: Node + ?Sized, T: 'static + Debug
{
  impl_downcast!(dyn ReferenceLink);
}

impl<O, N, T> ReferenceLink for MemberDescr<O, Reference<N, T>> where
  O: 'static, N: Node + ?Sized, T: 'static + Debug
{
  fn reference_obj<'a>(&self, object: &'a dyn Node) -> &'a dyn Any {
    &self.data(object).ref_object
  }

  fn reference_obj_debug<'a>(&self, object: &'a dyn Node) -> &'a dyn Debug {
    &self.data(object).ref_object
  }

  fn reference_obj_mut<'a>(&self, object: &'a mut dyn Node) -> &'a mut dyn Any {
    &mut self.data_mut(object).ref_object
  }

  fn target_node<'a>(&self, object: &'a dyn Node) -> &'a Option<NodeId> {
    let result = &self.data(object).target_node;
    unsafe { mem::transmute(result) }
  }

  fn target_node_mut<'a>(&self, object: &'a mut dyn Node) -> &'a mut Option<NodeId> {
    let result = &mut self.data_mut(object).target_node;
    unsafe { mem::transmute(result) }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
