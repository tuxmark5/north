use {
  crate::{
    Parser, ParseAspect,
  },
  north_core::{
    compiler::{Pass, pass},
    prelude::*,
  },
  north_lexer::{
    TokensNode
  },
  std::{
    marker::PhantomData,
    mem
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ParsePass<N> {
  root_node: PhantomData<N>,
}

impl<N> ParsePass<N> {
  pub fn new() -> Self {
    Self { root_node: PhantomData }
  }
}

impl<N> Pass for ParsePass<N> where
  N: Node
{
  fn execute(&self, comp: &Compiler) -> pass::Result {
    let parse_aspect = comp.aspect_mut::<ParseAspect>();
    let mut model = comp.model_cell.borrow_mut();

    model.reduce_nodes::<TokensNode, _>(|model, node| {
      let grammar = &parse_aspect.grammar;
      let tokens = mem::replace(&mut node.tokens, Vec::new());
      let tokens_iter = box tokens.into_iter();
      let mut parser = Parser::new(grammar, model, tokens_iter);
      let mut context = parser.context();
      context.parse::<N>()
        .ok().map(|n| n.cast())
    })
  }

  fn name(&self) -> &str {
    "parse"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
