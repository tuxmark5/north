use {
  crate::{
    Node, NodeId,
    model::{
      Child,
      element::Element,
    },
    util::{
      cast::{CastKind, CastSelf},
      dynamic_cast
    }
  },
  std::{
    any::TypeId,
    fmt::Debug,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ChildElem: Element {
  fn as_node_id(&self) -> &NodeId;
  fn as_node_id_mut(&mut self) -> &mut NodeId;
  fn child_type(&self) -> TypeId;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<N: Node + ?Sized> CastSelf for Child<N> {
  impl_cast_self!(dyn ChildElem);
}

impl<N: Node + ?Sized> Element for Child<N> {
  fn as_debug(&self) -> &dyn Debug { self }
}

impl<N: Node + ?Sized> ChildElem for Child<N> {
  fn as_node_id(&self) -> &NodeId { self.0.cast_ref() }
  fn as_node_id_mut(&mut self) -> &mut NodeId { self.0.cast_ref_mut() }
  fn child_type(&self) -> TypeId { TypeId::of::<N>() }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<N: Node + ?Sized> CastSelf for NodeId<N> {
  impl_cast_self!(dyn ChildElem);
}

impl<N: Node + ?Sized> Element for NodeId<N> {
  fn as_debug(&self) -> &dyn Debug { self }
}

impl<N: Node + ?Sized> ChildElem for NodeId<N> {
  fn as_node_id(&self) -> &NodeId { self.cast_ref() }
  fn as_node_id_mut(&mut self) -> &mut NodeId { self.cast_ref_mut() }
  fn child_type(&self) -> TypeId { TypeId::of::<N>() }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn map_children<'a, F>(node: &'a mut dyn Node, f: &mut F) where
  F: FnMut(NodeId) -> NodeId
{
  let concept = node.instance_concept();
  for member in concept.members() {
    let element = member.element_mut(node);
    map_children_2(element, f);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn map_children_2<'a, F>(element: &'a mut dyn Element, f: &mut F) where
  F: FnMut(NodeId) -> NodeId
{
  match dynamic_cast::<_, &mut dyn ChildElem>(element) {
    Ok(child_elem) => {
      let node_id = child_elem.as_node_id_mut();
      *node_id = f(*node_id);
    }

    Err(element) => if let Some(children_iter) = element.children_mut() {
      for child in children_iter {
        map_children_2(child, f);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
