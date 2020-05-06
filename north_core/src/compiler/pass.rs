use {
  crate::{
    Node, NodeId,
    compiler::{Compiler},
    context::{ModelNodeId, ModelNodeRef},
    model::ModelCell,
    structure::StorageExt,
  },
  std::result,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Pass {
  fn execute(&self, comp: &Compiler) -> Result;
  fn name(&self) -> &str;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait VisitPass {
  type Handler;

  fn is_applicable(&self, id: NodeId, node: &dyn Node) -> Option<Self::Handler>;
  fn visit_node(&mut self, handler: Self::Handler, node: ModelNodeRef<dyn Node>) -> Result;

  fn execute_visit(&mut self, model_cell: ModelCell) -> Result {
    let applicable_nodes = {
      let model = model_cell.borrow();
      let iter = model.storage().iter();
      let iter = iter.filter_map(|(id, cell)| {
        self.is_applicable(*id, &*cell.borrow()).map(|h| {
          (*id, cell.clone(), h)
        })
      });

      iter.collect::<Vec<_>>()
    };

    for (id, node_cell, handler) in applicable_nodes {
      let node_id = ModelNodeId::new(model_cell.clone(), id);
      let node_ref = ModelNodeRef::new(node_id, node_cell);
      self.visit_node(handler, node_ref)?;
    }

    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type Result = result::Result<(), ()>;

////////////////////////////////////////////////////////////////////////////////////////////////
