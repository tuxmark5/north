use {
  crate::{
    mir::RValLink,
    quote_value::quote_value,
  },
  north_core::Model,
  serde::Serialize,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ToValue {
  fn to_value(self, model: &mut Model) -> RValLink;
}

impl<'a, A: Serialize> ToValue for &'a A {
  default fn to_value(self, model: &mut Model) -> RValLink {
    quote_value(model, self).unwrap()
  }
}

/*impl<'a> ToValue for &'a ValueRef {
  fn to_value(self, _model: &mut Model) -> ValueRef {
    self.clone()
  }
}*/

impl<'a> ToValue for &'a RValLink where RValLink: Serialize {
  fn to_value(self, _model: &mut Model) -> RValLink {
    self.clone()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
