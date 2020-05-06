use {
  crate::{
    type_sys_aspect::TypeSysAspect,
  },
  north_core::{
    Node, NodeId, context_impl,
    futures::future::LocalFutureObj,
    model::{ModelCell, NodeCell},
    node_id::ToNodeId,
    rule_prelude::*,
    util::dynamic_cast,
  },
  north_derive::context,
  std::{
    hash::Hash,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type InferTypesFuture = LocalFutureObj<'static, Result<(), ()>>;
pub type InferTypesRuleKey = RuleKeySingleNode;
pub type InferTypesRuleMap = RuleMap<InferTypesRuleKey, dyn InferTypesDyn>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait InferTypes<N> where
  N: ?Sized
{
  fn infer_types(&self, ctx: InferTypesCtx<N>) -> InferTypesFuture;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type InferTypesCtx<N> = (InferTypesCtxCore, NodeCell<N>);

////////////////////////////////////////////////////////////////////////////////////////////////

#[context(ModelCellCtxExt, model_cell)]
#[context(ModelNodeIdCtxExt)]
#[context(NodeIdCtxExt, node_id)]
pub struct InferTypesCtxCore {
  pub aspect: Rc<TypeSysAspect>,
  pub model_cell: ModelCell,
  pub node_id: NodeId,
}

impl InferTypesCtxCore {
  pub fn make_type<B>(&self, data: B) -> NodeId where
    B: Clone + Eq + Hash + Node + PartialEq
  {
    self.aspect.make_type(data)
  }

  pub fn type_eq(&self, type_a: NodeId, type_b: NodeId) {
    let aspect = self.aspect.clone();
    aspect.unify_types_start(type_a, type_b);
  }

  pub fn type_of<T: ToNodeId>(&self, id: T) -> NodeId {
    self.aspect.type_of(id.to_top())
  }

  pub fn type_of_n<'i, A, B, I>(&self, iter: I) -> Vec<NodeId<B>> where
    A: Node + ?Sized, B: Node + ?Sized, I: 'i + Iterator<Item=&'i NodeId<A>>
  {
    iter.map(|node_id| self.type_of(*node_id).cast())
      .collect::<Vec<_>>()
  }

  pub fn type_of_self(&self) -> NodeId {
    let node_id = self.node_id;
    self.type_of(node_id)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait InferTypesDyn {
  fn infer_types_dyn(&self, ctx: InferTypesCtx<dyn Node>) -> InferTypesFuture;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<N, R> InferTypesDyn for RuleDynAdapter<N, R> where
  N: Node + ?Sized, R: 'static + InferTypes<N>
{
  fn infer_types_dyn(&self, (ctx, node): InferTypesCtx<dyn Node>) -> InferTypesFuture {
    let node = dynamic_cast(node).ok().unwrap();
    self.rule.infer_types((ctx, node))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! async_block_2 {
  ($($body:tt)*) => {
    crate::futures::__rt::gen_future(
      static move || {
        if false { yield crate::futures::__rt::Async::Pending }
        $($body)*
      }
    )
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! infer_types_r {
  (__add, $aspect:expr, [$node_ty:ty => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<TypeSysAspect>($aspect) {
      aspect.infer_types_rules.add::<$node_ty, _>($rule);
    }
  };

  (__method, infer_types,
    ($node_ty:ty),
    ($self:ident, $ctx:ident, $node:ident),
    (), $body:tt
  ) => {
    #[allow(unused_mut)]
    fn infer_types(
      &$self, (mut $ctx, node_cell): InferTypesCtx<$node_ty>
    ) -> InferTypesFuture {
      let body = async move { let $node = node_cell.borrow(); $body; Ok(()) };
      north_core::futures::future::LocalFutureObj::new(box body)
    }
  };

  (__trait, ($($generics:tt)*), ($node_ty:ty), $target:ty, $body:tt) => {
    impl $($generics)* InferTypes<$node_ty> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
