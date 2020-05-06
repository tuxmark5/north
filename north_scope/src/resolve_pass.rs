use {
  crate::{
    resolve::{ResolveCtx, ResolveDyn},
    scope_aspect::ScopeAspect,
  },
  north_core::{
    compiler::{
      pass::{self, Pass, VisitPass},
    },
    context::{ModelNodeRef},
    prelude::*,
  },
  std::{
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ResolvePass {
}

impl ResolvePass {
  pub fn new() -> Self {
    Self { }
  }
}

impl Pass for ResolvePass {
  fn execute(&self, comp: &Compiler) -> pass::Result {
    let aspect = comp.aspect_mut::<ScopeAspect>();
    let model_cell = comp.model_cell.clone();
    let mut inner = ResolvePassImpl { comp, aspect };
    inner.execute_visit(model_cell)
  }

  fn name(&self) -> &str {
    "resolve"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ResolvePassImpl<'c> {
  comp: &'c Compiler,
  aspect: Rc<ScopeAspect>
}

impl<'c> VisitPass for ResolvePassImpl<'c> {
  type Handler = Rc<dyn ResolveDyn>;

  fn is_applicable(&self, _id: NodeId, node: &dyn Node) -> Option<Self::Handler> {
    self.aspect.get_resolve_rule(node)
  }

  fn visit_node(&mut self,
    rule : Self::Handler,
    node : ModelNodeRef<dyn Node>
  ) -> pass::Result {
    let ctx = ResolveCtx::new(self.aspect.clone(), node);
    let future = rule.resolve_dyn(ctx);
    self.comp.spawn_local(future);
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
