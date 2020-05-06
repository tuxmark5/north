use {
  crate::{
    Lexer, TokensNode,
  },
  north_core::{
    compiler::{pass, Compiler, Pass},
  },
  north_loader::{
    SourceCodeNode,
  },
  std::{
    marker::PhantomData,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LexerPass<L> {
  lexer_type: PhantomData<L>
}

impl<L> LexerPass<L> {
  pub fn new() -> Self {
    Self {
      lexer_type: PhantomData
    }
  }
}

impl<L: Lexer> Pass for LexerPass<L> {
  fn execute(&self, compiler: &Compiler) -> pass::Result {
    let mut model = compiler.model_mut();

    model.reduce_nodes::<SourceCodeNode, _>(|model, node| {
      let source_code = node.src();
      let (tokens, diags) = L::lex(node.base_pos(), source_code.as_ref());
      let new_node = TokensNode::new(tokens);
      let new_node_id = model.build_node(new_node);
      compiler.emit_diags(diags);
      Some(new_node_id.cast())
    })
  }

  fn name(&self) -> &str {
    "lex"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
