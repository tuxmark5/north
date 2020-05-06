use {
  crate::{
    ScopeKind,
    scope::{self, ScopePtrOpt},
    scope_aspect::ScopeAspect,
  },
  north_core::{
    context::{
      ModelCellCtxExt,
      ModelNodeIdCtxExt,
      NodeIdCtxExt,
    },
    model::ModelCell,
    prelude::*,
    util::cast::{dynamic_cast},
  },
  north_derive::context,
  std::{
    marker::PhantomData,
    rc::Rc
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait DeriveScope<N, K> where
  N: ?Sized
{
  // Binding: relative to Ref node and curr node, relative to curr node, global
  const RELATIVE: bool = false;

  fn derive_scope(&self, pair: DeriveScopeCtx<N>) -> ScopePtrOpt;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type DeriveScopeCtx<'n, N> = (DeriveScopeCtxCore, &'n N);

////////////////////////////////////////////////////////////////////////////////////////////////

#[context(ModelCellCtxExt, model_cell)]
#[context(ModelNodeIdCtxExt)]
#[context(NodeIdCtxExt, node_id)]
pub struct DeriveScopeCtxCore {
  pub aspect: Rc<ScopeAspect>,
  pub model_cell: ModelCell,
  pub node_id: NodeId,
  pub ref_node_id: NodeId,
}

impl DeriveScopeCtxCore {
  pub fn parent_scope<S>(&mut self, kind: S) -> ScopePtrOpt where
    S: ScopeKind
  {
    self.parent_id().and_then(|id| self.scope_of(kind, id))
  }

  pub fn prev_sibling_scope<S>(&mut self, kind: S) -> ScopePtrOpt where
    S: ScopeKind
  {
    self.prev_sibling_id().and_then(|id| self.scope_of(kind, id))
  }

  pub fn scope_composite<'i, S, I, N1>(&mut self, kind: S, iter: I) -> ScopePtrOpt where
    S: ScopeKind, I: IntoIterator<Item=&'i NodeId<N1>>, N1: Node + ?Sized
  {
    let scopes = iter.into_iter()
      .filter_map(|id| self.scope_of(kind, id))
      .collect::<Vec<_>>();
    scope::Composite::make(scopes)
  }

  pub fn scope_of<S, I>(&mut self, _kind: S, node_id: I) -> ScopePtrOpt where
    S: ScopeKind, I: ToNodeId
  {
    let aspect = self.aspect.clone();
    let node_id = node_id.to_node_id().cast();
    let ref_node_id = self.node_id;
    aspect.derive_scope::<S>(node_id, ref_node_id)
  }

  pub fn self_scope<S, I>(&mut self, _kind: S, ref_node_id: I) -> ScopePtrOpt where
    S: ScopeKind, I: ToNodeId
  {
    let aspect = self.aspect.clone();
    let ref_node_id = ref_node_id.to_node_id().cast();
    aspect.derive_scope::<S>(self.node_id, ref_node_id)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait DeriveScopeDyn {
  fn derive_scope_dyn(&self, ctx: DeriveScopeCtx<dyn Node>) -> ScopePtrOpt;
  fn is_relative(&self) -> bool;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DeriveScopeDynAdapter<R, N, K> where
  N: ?Sized
{
  pub rule: R,
  pub node_type: PhantomData<N>,
  pub kind_type: PhantomData<K>,
}

impl<A, N, K> DeriveScopeDynAdapter<A, N, K> where
  A: 'static + DeriveScope<N, K>, N: Node + ?Sized, K: 'static
{
  pub fn wrap(rule: A) -> Rc<dyn DeriveScopeDyn> {
    Rc::new(Self {
      rule,
      node_type: PhantomData,
      kind_type: PhantomData
    })
  }
}

impl<A, N, K> DeriveScopeDyn for DeriveScopeDynAdapter<A, N, K> where
  A: DeriveScope<N, K>, N: Node + ?Sized
{
  fn derive_scope_dyn(&self, (ctx, node): DeriveScopeCtx<dyn Node>) -> ScopePtrOpt {
    let node = dynamic_cast(node).ok().unwrap();
    self.rule.derive_scope((ctx, node))
  }

  fn is_relative(&self) -> bool {
    A::RELATIVE
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! derive_scope_r {
  (__add, $aspect:expr, [$kind_ty:ty, _ => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<ScopeAspect>($aspect) {
      aspect.add_derive_default::<$kind_ty, _>($rule);
    }
  };

  (__add, $aspect:expr, [$kind_ty:ty, $node_ty:ty => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<ScopeAspect>($aspect) {
      aspect.add_derive::<$node_ty, $kind_ty, _>($rule);
    }
  };

  (__method, derive_scope,
    ($scope_ty:ty, $node_ty:ty),
    ($self:ident, $ctx:ident, $node:ident),
    (), $body:tt
  ) => {
    #[allow(unused_mut)]
    fn derive_scope(
      &$self, (mut $ctx, $node): DeriveScopeCtx<$node_ty>
    ) -> ScopePtrOpt { $body }
  };

  (__trait, ($($generics:tt)*), ($kind_ty:ty, $node_ty:ty), $target:ty, $body:tt) => {
    impl $($generics)* DeriveScope<$node_ty, $kind_ty> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
