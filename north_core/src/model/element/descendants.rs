use {
  crate::{
    model::element::{ChildIter, Element},
  },
  std::iter,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Descendants<'a> {
  stack: Vec<ChildIter<'a>>
}

impl<'a> Descendants<'a> {
  pub fn new(element: &'a dyn Element) -> Self {
    let iter = box iter::once(element);
    Self {
      stack: vec![iter],
    }
  }
}

impl<'a> Iterator for Descendants<'a> {
  type Item = &'a dyn Element;

  fn next(&mut self) -> Option<Self::Item> {
    let elem = loop {
      let elem = match self.stack.last_mut() {
        Some(iter) => iter.next(),
        None => return None,
      };

      match elem {
        Some(elem) => break elem,
        None => self.stack.pop(),
      };
    };

    if let Some(iter) = elem.children() {
      self.stack.push(iter);
    }

    Some(elem)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
