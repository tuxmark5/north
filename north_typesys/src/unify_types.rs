use {
  crate::{
    type_sys_aspect::TypeSysAspect
  },
  north_core::{
    Node, NodeId,
    //futures::prelude::*,
    model::ModelCell,
    util::dynamic_cast,
  },
  std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait UnifyTypes<A, B> where
  A: ?Sized, B: ?Sized
{
  fn unify_types(&self, ctx: UnifyTypesCtx, type_a: &A, type_b: &B);
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct UnifyTypesCtx {
  pub aspect: Rc<TypeSysAspect>,
  pub model_cell: ModelCell,
}

impl UnifyTypesCtx {
  pub fn new(aspect: Rc<TypeSysAspect>, model_cell: ModelCell) -> Self {
    Self { aspect, model_cell }
  }

  pub fn unify<N>(&self, type_a: NodeId<N>, type_b: NodeId<N>) where
    N: Node + ?Sized
  {
    let aspect = self.aspect.clone();
    aspect.unify_types_start(type_a.cast(), type_b.cast())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait UnifyTypesDyn {
  fn unify_types_dyn(&self, ctx: UnifyTypesCtx, type_a: &dyn Node, type_b: &dyn Node);
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct UnifyTypesDynAdapter<A, B, R> where
  A: ?Sized, B: ?Sized
{
  type_a: PhantomData<A>,
  type_b: PhantomData<B>,
  rule: R,
}

impl<A, B, R> UnifyTypesDynAdapter<A, B, R> where
  A: 'static + ?Sized, B: 'static + ?Sized, R: 'static + UnifyTypes<A, B>
{
  pub fn wrap(rule: R) -> Rc<dyn UnifyTypesDyn> {
    let adapter = Self { type_a: PhantomData, type_b: PhantomData, rule };
    Rc::new(adapter)
  }
}

impl<A, B, R> UnifyTypesDyn for UnifyTypesDynAdapter<A, B, R> where
  A: 'static + ?Sized, B: 'static + ?Sized, R: UnifyTypes<A, B>
{
  fn unify_types_dyn(&self, ctx: UnifyTypesCtx, type_a: &dyn Node, type_b: &dyn Node) {
    let type_a = dynamic_cast::<_, &A>(type_a).ok().unwrap();
    let type_b = dynamic_cast::<_, &B>(type_b).ok().unwrap();
    self.rule.unify_types(ctx, type_a, type_b);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct UnifyTypesRuleKey {
  pub left_type: TypeId,
  pub right_type: TypeId,
}

impl UnifyTypesRuleKey {
  pub fn new_static<A, B>() -> Self where
    A: Node + ?Sized, B: Node + ?Sized
  {
    Self {
      left_type: TypeId::of::<A>(),
      right_type: TypeId::of::<B>(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type UnifyTypesRuleMap = HashMap<UnifyTypesRuleKey, Rc<dyn UnifyTypesDyn>>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! unify_types_r {
  (__add, $aspect:expr, [$node_ty:ty => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<TypeSysAspect>($aspect) {
      aspect.add_unify_types::<$node_ty, $node_ty, _>($rule);
    }
  };

  (__method, unify_types,
    ($node_ty:ty),
    ($self:ident, $ctx:ident, $a:ident, $b:ident),
    (), $body:tt
  ) => {
    fn unify_types(
      &$self, $ctx: UnifyTypesCtx,
      $a: &$node_ty, $b: &$node_ty,
    ) { $body }
  };

  (__trait, ($($generics:tt)*), ($node_ty:ty), $target:ty, $body:tt) => {
    impl $($generics)* UnifyTypes<$node_ty, $node_ty> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! unify_types_part {
  ($part:ident { $(($ty_a:ty, $ty_b:ty) => $rule:expr),* }) => {
    pub struct $part;

    impl AspectPart for $part {
      type Aspect = TypeSysAspect;

      fn setup(&self, aspect: &mut TypeSysAspect) {
        $( aspect.add_unify_types::<$ty_a, $ty_b, _>($rule); )*
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
