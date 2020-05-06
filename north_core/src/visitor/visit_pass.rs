use {
  crate::{
    Node, NodeId,
    compiler::{
      Compiler,
      pass::{self, Pass, VisitPass as VisitPassTrait},
    },
    context::{
      ModelNodeRef,
      NodeCellCtxExt,
      NodeIdCtxExt,
    },
    visitor::{
      Visitor, VisitorAspect,
      visit::{VisitCtxCore, VisitDyn},
    },
  },
  std::{
    marker::PhantomData,
    rc::Rc
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VisitPass<V> {
  visitor_type: PhantomData<V>,
}

impl<V> VisitPass<V> where
  V: Visitor,
{
  pub fn new() -> Self {
    Self { visitor_type: PhantomData }
  }
}

impl<V> Pass for VisitPass<V> where
  V: Visitor<ExplicitArgs=(), ImplicitArgs=()>,
{
  fn execute(&self, comp: &Compiler) -> pass::Result {
    let aspect = comp.aspect_mut::<VisitorAspect>();
    let mut inner = VisitPassImpl {
      aspect: &*aspect,
      visitor_type: PhantomData::<V>,
    };
    inner.execute_visit(comp.model_cell.clone())
  }

  fn name(&self) -> &str {
    "visit pass"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VisitPassImpl<'a, V> {
  aspect: &'a VisitorAspect,
  visitor_type: PhantomData<V>,
}

impl<'a, V> pass::VisitPass for VisitPassImpl<'a, V> where
  V: Visitor<ExplicitArgs=(), ImplicitArgs=()>
{
  type Handler = Rc<dyn VisitDyn<V>>;

  fn is_applicable(&self, _id: NodeId, node: &dyn Node) -> Option<Self::Handler> {
    let node_type = (*node).type_id();
    self.aspect.get_visit::<V>(node_type)
  }

  fn visit_node(&mut self, rule: Self::Handler, node: ModelNodeRef<dyn Node>) -> pass::Result {
    let core = VisitCtxCore {
      aspect: self.aspect,
      node_id: node.node_id(),
      imp_args: &mut (),
    };

    let node = &*node.node();
    rule.visit_dyn((core, node, &mut ()))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
