use {
  crate::ast,
  north_core::{Model}
};

////////////////////////////////////////////////////////////////////////////////////////////////

impl ast::ItemBuiltins {
  pub fn new(model: &mut Model) -> Self {
    let mut node = Self { nodes: Vec::new() };
    node.make_builtins(model);
    node
  }

  pub fn add_type<N, T>(&mut self, model: &mut Model, name: N, type_node: T) where
    N: ToString, T: ast::Type
  {
    let type_id = model.build_node(type_node);
    let def_node = ast::ItemType::new(name, type_id.cast());
    let node_id = model.build_node(def_node);
    self.nodes.push(node_id.cast());
  }

  pub fn make_builtins(&mut self, model: &mut Model) {
    self.add_type(model, "bool", ast::TypeBool { });
    self.add_type(model, "i32", ast::TypeInt::new(true, 32));
    self.add_type(model, "i64", ast::TypeInt::new(true, 64));
    self.add_type(model, "u32", ast::TypeInt::new(false, 32));
    self.add_type(model, "u64", ast::TypeInt::new(false, 64));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
