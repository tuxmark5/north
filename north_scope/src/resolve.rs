use {
  crate::{
    scope::{ScopePtrOpt},
    scope_aspect::ScopeAspect,
  },
  north_core::{
    context::ModelNodeRef,
    futures::{future::LocalFutureObj},
    model::{NodeCell, Reference},
    prelude::*,
    rule_prelude::*,
    structure::StorageExt,
    trait_manager::TraitManagerExt,
    util::cast::{dynamic_cast}
  },
  north_derive::context,
  std::{
    cell::{Ref, RefMut},
    rc::Rc,
  },
};


////////////////////////////////////////////////////////////////////////////////////////////////

pub type ResolveFuture = LocalFutureObj<'static, Result<(), ()>>;
pub type ResolveRuleKey = RuleKeySingleNode;
pub type ResolveRuleMap = RuleMap<ResolveRuleKey, dyn ResolveDyn>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Resolve<N: Node + ?Sized>: 'static {
  fn resolve(&self, ctx: ResolveCtx<N>) -> ResolveFuture;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[context(ModelCellCtxExt, model_cell)]
#[context(ModelNodeIdCtxExt)]
#[context(NodeIdCtxExt, node_id)]
pub struct ResolveCtx<N: Node + ?Sized> {
  pub aspect: Rc<ScopeAspect>,
  pub model_cell: ModelCell,
  pub node_id: NodeId,
  pub node_cell: NodeCell<N>,
}

impl<N> ResolveCtx<N> where
  N: Node + ?Sized
{
  pub fn new(aspect: Rc<ScopeAspect>, model_ref: ModelNodeRef<N>) -> Self {
    Self {
      aspect,
      model_cell: model_ref.model_cell,
      node_id: model_ref.node_id,
      node_cell: model_ref.node_cell,
    }
  }

  // NodeCellCtxExt
  pub fn node_mut(&self) -> RefMut<N> {
    self.node_cell.borrow_mut()
  }

  pub fn own_scope<K>(&mut self) -> ScopePtrOpt where
    K: 'static
  {
    let aspect = self.aspect.clone();
    aspect.derive_scope::<K>(self.node_id, self.node_id)
  }

  pub fn resolve_ref<T, O>(&self, ref_: &mut Reference<T, O>, scope: ScopePtrOpt) where
    T: Node + ?Sized, O: AsRef<str>
  {
    let scope = match scope {
      Some(scope) => scope,
      None => return,
    };

    let key = ref_.ref_object.as_ref();
    if let Some(target) = scope.lookup(key) {
      let model = self.model_cell.borrow();
      let target_node = model.data(target);
      if model.cast::<_, Ref<T>>(target_node).is_ok() {
        ref_.target_node = Some(target.cast());
      } else {
        panic!("can't assign ref_key={}", key);
      }
    } else {
      panic!("can't resolve {}", key);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ResolveDyn {
  fn resolve_dyn(&self, ctx: ResolveCtx<dyn Node>) -> ResolveFuture;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<N, R> ResolveDyn for RuleDynAdapter<N, R> where
  N: Node + ?Sized, R: Resolve<N>
{
  fn resolve_dyn(&self, ctx: ResolveCtx<dyn Node>) -> ResolveFuture {
    let new_ctx = ResolveCtx {
      aspect: ctx.aspect,
      model_cell: ctx.model_cell,
      node_id: ctx.node_id,
      node_cell: dynamic_cast(ctx.node_cell).ok().unwrap(),
    };

    self.rule.resolve(new_ctx)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! resolve_r {
  (__add, $aspect:expr, [$node_ty:ty => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<ScopeAspect>($aspect) {
      aspect.resolve_rules.add::<$node_ty, _>($rule);
    }
  };

  (__method, resolve,
    ($node_ty:ty),
    ($self:ident, $ctx:ident),
    (), $body:tt
  ) => {
    #[allow(unused_mut)]
    fn resolve(&$self, mut $ctx: ResolveCtx<$node_ty>) -> ResolveFuture {
      let body = async move { $body; Ok(()) };
      north_core::futures::future::LocalFutureObj::new(box body)
    }
  };

  (__trait, ($($generics:tt)*), ($($params:tt)*), $target:ty, $body:tt) => {
    impl $($generics)* Resolve<$($params)*> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
