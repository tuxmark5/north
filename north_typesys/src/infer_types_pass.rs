use {
  crate::{
    infer_types::{InferTypesCtxCore, InferTypesDyn},
    type_sys_aspect::TypeSysAspect,
  },
  north_core::{
    Node, NodeId,
    compiler::{
      Compiler,
      pass::{self, Pass, VisitPass},
    },
    context::{ModelNodeRef},
  },
  std::{
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct InferTypesPass {
}

impl InferTypesPass {
  pub fn new() -> Self {
    Self { }
  }
}

impl Pass for InferTypesPass {
  fn execute(&self, comp: &Compiler) -> pass::Result {
    let aspect = comp.aspect_mut::<TypeSysAspect>();
    let mut inner = InferTypesPassImpl { aspect: aspect.clone(), compiler: comp };
    inner.execute_visit(comp.model_cell.clone())
  }

  fn name(&self) -> &str {
    "infer types"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct InferTypesPassImpl<'c> {
  aspect: Rc<TypeSysAspect>,
  compiler: &'c Compiler,
}

impl<'c> VisitPass for InferTypesPassImpl<'c> {
  type Handler = Rc<dyn InferTypesDyn>;

  fn is_applicable(&self, _id: NodeId, node: &dyn Node) -> Option<Self::Handler> {
    let node_type = (*node).type_id();
    self.aspect.get_infer_types(node_type)
  }

  fn visit_node(&mut self, rule: Self::Handler, node: ModelNodeRef<dyn Node>) -> pass::Result {
    let core = InferTypesCtxCore {
      aspect: self.aspect.clone(),
      model_cell: node.model_cell,
      node_id: node.node_id,
    };

    let node_cell = node.node_cell;
    let future = rule.infer_types_dyn((core, node_cell));
    self.compiler.spawn_local(future);

    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
