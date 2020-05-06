use {
  crate::{
    grammar::Grammar,
    parse::Parse,
  },
  north_core::{
    Node,
    compiler::Aspect
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct ParseAspect {
  pub grammar: Grammar,
}

impl ParseAspect {
  pub fn add_rule<A, B, R>(&mut self, precedence: i32, rule: R) where
    A: 'static + ?Sized, B: Node, R: 'static + Parse<B>
  {
    self.grammar.add_rule_ex::<A, B, R>(precedence, rule);
  }
}

impl Aspect for ParseAspect {
  fn name(&self) -> &str {
    "parse"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
