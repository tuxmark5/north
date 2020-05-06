use {
  crate::ast,
  north_core::{
    Node,
    model::Model,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

impl ast::ItemBuiltins {
  pub fn new(model: &mut Model) -> Self {
    let mut node = Self { elems: Vec::new() };
    node.make_builtins(model);
    node
  }

  pub fn add_alias<N, T>(&mut self, model: &mut Model, name: N, type_node: T) where
    N: ToString, T: Node
  {
    let type_id = model.build_node(type_node);
    let def_node = ast::ItemType::new(name, type_id.cast());
    let node_id = model.build_node(def_node);
    self.elems.push(node_id.cast());
  }

  pub fn make_builtins(&mut self, model: &mut Model) {
    self.add_alias(model, "ANY", ast::BuiltinAny { });
    self.add_alias(model, "ANY_UTF8", ast::BuiltinAnyUtf8 { });
    self.add_alias(model, "B", ast::BuiltinBoundary { });
    self.add_alias(model, "O1", ast::BuiltinOperatorEnd { });
    self.add_alias(model, "S1", ast::BuiltinWhitespaceEnd { });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
