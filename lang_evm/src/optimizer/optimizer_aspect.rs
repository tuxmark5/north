use {
  north_core::{
    compiler::{Aspect, Compiler},
    model::ModelCell,
  },
};


////////////////////////////////////////////////////////////////////////////////////////////////

pub struct OptimizerAspect {
  pub input_model: ModelCell,
}

impl OptimizerAspect {
  pub fn new(input_model: ModelCell) -> Self {
    Self {
      input_model,
    }
  }

  /*pub fn closure(visitor: &VisitorAspect, seed: Vec<NodeId>) -> Vec<NodeId> {
    let mut builder = ClosureBuilder::new(seed);
    builder.build(visitor);
    builder.closure
  }

  pub fn group(visitor: &VisitorAspect, closure: Vec<NodeId>) -> GroupSet {
    let mut builder = GroupBuilder::new();
    builder.build(visitor, closure);
    builder.into_group_set()
  }

  pub fn optimize_rules(&self, comp: &Compiler, rules: Vec<NodeId>) {
    let model = self.input_model.borrow();
    let visitor = comp.aspect_mut::<VisitorAspect>();

    let seed = model.iter(rules)
      .borrow_cast_nodes_to::<mir::ItemRule>()
      .filter_map(|r| r.blocks.first().cloned())
      .map(|b| model.node(b).first_node())
      .collect::<Vec<_>>();

    let closure = Self::closure(&*visitor, seed); 
    let group = Self::group(&*visitor, closure);

  }*/
}

impl Aspect for OptimizerAspect {
  fn create(comp: &Compiler) -> Self where 
    Self: Sized 
  {
    Self::new(comp.model_cell.clone())
  }

  fn name(&self) -> &str {
    "optimizer aspect"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
