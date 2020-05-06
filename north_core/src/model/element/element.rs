use {
  crate::{
    util::cast::CastSelf
  },
  std::{
    any::Any,
    fmt::Debug
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ChildIter<'a> = Box<dyn 'a + Iterator<Item=&'a dyn Element>>;
pub type ChildIterMut<'a> = Box<dyn 'a + Iterator<Item=&'a mut dyn Element>>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Element: 'static + Any + CastSelf + Debug {
  fn as_debug(&self) -> &dyn Debug;
  fn children(&self) -> Option<ChildIter> { None }
  fn children_mut(&mut self) -> Option<ChildIterMut> { None }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<A: 'static + Debug> Element for A {
  default fn as_debug(&self) -> &dyn Debug { self }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<A: Element> Element for Option<A> {
  fn as_debug(&self) -> &dyn Debug { self }

  fn children(&self) -> Option<ChildIter> {
    let iter = self.iter().map(|e| e as &dyn Element);
    Some(box iter)
  }

  fn children_mut(&mut self) -> Option<ChildIterMut> {
    let iter = self.iter_mut().map(|e| e as &mut dyn Element);
    Some(box iter)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<A: Element> Element for Vec<A> {
  fn as_debug(&self) -> &dyn Debug { self }

  fn children(&self) -> Option<ChildIter> {
    let iter = self.iter().map(|e| e as &dyn Element);
    Some(box iter)
  }

  fn children_mut(&mut self) -> Option<ChildIterMut> {
    let iter = self.iter_mut().map(|e| e as &mut dyn Element);
    Some(box iter)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
