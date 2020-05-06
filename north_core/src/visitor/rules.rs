use {
  crate::{
    Node,
    iter::ChildrenRaw,
    visitor::{Visit, VisitCtx, Visitor},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VisitAbort;

impl<V, N> Visit<V, N> for VisitAbort where
  V: Visitor, N: Node + ?Sized
{
  fn visit(&self, _: VisitCtx<V, N>) -> Result<(), ()> {
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VisitChildren;

impl<V, N> Visit<V, N> for VisitChildren where
  V: Visitor, N: Node
{
  fn visit(&self, (mut core, node, args): VisitCtx<V, N>) -> Result<(), ()> {
    let ids = ChildrenRaw::new(node).map(|e| e.1);
    core.visit_n(ids, args)
  }
}

impl<V> Visit<V, dyn Node> for VisitChildren where
  V: Visitor
{
  fn visit(&self, (mut core, node, args): VisitCtx<V, dyn Node>) -> Result<(), ()> {
    let ids = ChildrenRaw::new(node).map(|e| e.1);
    core.visit_n(ids, args)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
