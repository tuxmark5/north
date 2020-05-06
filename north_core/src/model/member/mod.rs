use {
  crate::{
    Node,
    model::element::Element,
    util::downcast::{Downcast},
  },
  std::{
    any::{Any, TypeId},
    fmt::{Debug},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Member: Downcast {
  fn debug<'a>(&self, object: &'a dyn Node) -> &'a dyn Debug;
  fn element<'a>(&self, node: &'a dyn Node) -> &'a dyn Element;
  fn element_mut<'a>(&self, node: &'a mut dyn Node) -> &'a mut dyn Element;
  fn get<'a>(&self, object: &'a dyn Node) -> &'a dyn Any;
  fn get_mut<'a>(&self, object: &'a mut dyn Node) -> &'a mut dyn Any;
  fn name(&self) -> &'static str;
  fn object_type(&self) -> TypeId;
  fn target_type(&self) -> TypeId;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use]
pub mod member_descr;

pub mod child;
pub mod child_link;
pub mod link;
pub mod reference;
pub mod reference_link;
pub mod token_member;

pub use self::{
  child_link::ChildLink,
  member_descr::MemberDescr,
  reference::Reference,
  reference_link::ReferenceLink,
  token_member::TokenMember,
};

////////////////////////////////////////////////////////////////////////////////////////////////
