use {
  crate::{
    Node, Token,
    model::member::{Member, MemberDescr},
    util::downcast::{
      Downcast, DowncastEntry
    }
  },
  std::{
    any::TypeId,
    fmt::Debug
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait TokenMember: Member {
  fn debug_token<'a>(&self, node: &'a dyn Node) -> &'a dyn Debug;
  //fn source_range<'a>(&self, object: &'a dyn Any) -> SourceRange;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O, T> Downcast for MemberDescr<O, Token<T>> where
  O: 'static, T: 'static + Debug
{
  impl_downcast!(dyn TokenMember);
}

impl<O, T> TokenMember for MemberDescr<O, Token<T>> where
  O: 'static, T: 'static + Debug
{
  fn debug_token<'a>(&self, node: &'a dyn Node) -> &'a dyn Debug {
    &self.data(node).data
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O, T> Downcast for MemberDescr<O, Option<Token<T>>> where
  O: 'static, T: 'static + Debug
{
  impl_downcast!(dyn TokenMember);
}

impl<O, T> TokenMember for MemberDescr<O, Option<Token<T>>> where
  O: 'static, T: 'static + Debug
{
  fn debug_token<'a>(&self, node: &'a dyn Node) -> &'a dyn Debug {
    self.data(node)
    /*match self.data(node) {
      Some(token) => &token.data,
      None => &(None: Option<()>)
    }*/
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
