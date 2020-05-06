use {
  crate::{
    Reduction,
    gen_aspect::GenAspect,
  },
  north_core::{
    context::{
      ModelCellCtxExt,
      ModelNodeIdCtxExt,
      NodeIdCtxExt
    },
    model::{ModelCell, Prop, PropMapExt},
    prelude::*,
    util::dynamic_cast
  },
  north_derive::context,
  std::{
    any::{Any, TypeId},
    collections::HashMap,
    marker::PhantomData,
    rc::Rc
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Reduce<R, N>: 'static where
  R: Reduction, N: Node + ?Sized
{
  type Output;// = <R as Reduction>::Output;
  fn reduce(&self, ctx: ReduceCtx<R, N>) -> Self::Output;
  fn reduce_late(&self, _ctx: ReduceCtx<R, N>, _value: R::Output) { }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ReduceCtx<'a, 'ctx, 'arg, 'n, R, N> = (
  &'ctx mut ReduceCtxCore<'a, R>, &'n N, &'arg mut <R as Reduction>::Arg
);

pub type ReduceCtxDyn<'a, 'arg, 'n, R> = (
  ReduceCtxCore<'a, R>, &'n dyn Node, &'arg mut <R as Reduction>::Arg
);

////////////////////////////////////////////////////////////////////////////////////////////////

#[context(ModelNodeIdCtxExt)]
#[context(NodeIdCtxExt, node_id)]
pub struct ReduceCtxCore<'a, R> where
  R: Reduction
{
  pub aspect: &'a GenAspect,
  pub node_id: NodeId,
  pub inner: R::InnerCtx,
}

impl<'a, R> ReduceCtxCore<'a, R> where
  R: Reduction
{
  pub fn fork<Q>(&self, node_id: NodeId) -> ReduceCtxCore<'a, Q> where
    Q: Reduction<InnerCtx = R::InnerCtx>
  {
    ReduceCtxCore {
      aspect: self.aspect,
      node_id,
      inner: self.inner.clone(),
    }
  }

  pub fn gen_prop<A, P>(&self, id: A, _prop: P) -> Option<P::Type> where
    A: ToNodeId, P: Prop, P::Type: Clone
  {
    let prop_map = self.aspect.properties.borrow();
    let node_id = id.to_node_id().cast();
    prop_map.prop::<P>(node_id).cloned()
  }

  pub fn gen_prop_own<P>(&self, prop: P) -> Option<P::Type> where
    P: Prop, P::Type: Clone
  {
    self.gen_prop(self.node_id, prop)
  }

  pub fn gen_prop_own_set<P>(&self, prop: P, value: P::Type) where
    P: Prop
  {
    self.gen_prop_set(self.node_id, prop, value);
  }

  pub fn gen_prop_set<A, P>(&self, id: A, prop: P, value: P::Type) where
    A: ToNodeId, P: Prop
  {
    let mut prop_map = self.aspect.properties.borrow_mut();
    let node_id = id.to_node_id().cast();
    prop_map.prop_set::<P>(node_id, prop, value);
  }

  pub fn reduce_cld<A>(&self, id: A, arg: &mut R::Arg) -> R::Output where
    A: ToNodeId
  {
    self.reduce_cld_with::<A, R>(R::default(), id, arg)
  }

  pub fn reduce_cld_into<A, B>(&self, id: A, arg: &mut R::Arg) -> B where
    A: ToNodeId, B: From<R::Output>
  {
    let output = self.reduce_cld_with::<A, R>(R::default(), id, arg);
    B::from(output)
  }

  pub fn reduce_cld_n<A, B>(&self, iter: A, arg: &mut R::Arg) -> Vec<B> where
    A: IntoIterator, A::Item: ToNodeId, B: From<R::Output>
  {
    iter.into_iter()
      .map(|id| self.reduce_cld_into(id, arg))
      .collect()
  }

  pub fn reduce_cld_with<A, Q>(&self, _red: Q, id: A, arg: &mut Q::Arg) -> Q::Output where
    A: ToNodeId, Q: Reduction<InnerCtx = R::InnerCtx>
  {
    let node_id = id.to_node_id().cast();
    let core = self.fork(node_id);
    self.aspect.reduce_cld::<Q>(core, arg)
  }

  pub fn reduce_link<A>(&self, id: A, arg: &mut R::Arg) -> R::Output where
    A: ToNodeId
  {
    self.reduce_link_with::<A, R>(R::default(), id, arg)
  }

  pub fn reduce_link_into<A, B>(&self, id: A, arg: &mut R::Arg) -> B where
    A: ToNodeId, B: From<R::Output>
  {
    let output = self.reduce_link_with::<A, R>(R::default(), id, arg);
    B::from(output)
  }

  pub fn reduce_link_n<A, B>(&self, iter: A, arg: &mut R::Arg) -> Vec<B> where
    A: IntoIterator, A::Item: ToNodeId, B: From<R::Output>
  {
    iter.into_iter()
      .map(|id| self.reduce_link_into(id, arg))
      .collect()
  }

  pub fn reduce_link_with<A, Q>(&self, _red: Q, id: A, arg: &mut Q::Arg) -> Q::Output where
    A: ToNodeId, Q: Reduction<InnerCtx = R::InnerCtx>
  {
    let node_id = id.to_node_id().cast();
    let core = self.fork(node_id);
    self.aspect.reduce_link::<Q>(core, arg)
  }

  pub fn reduce_ref<A>(&self, id: A, arg: &mut R::Arg) -> R::Output where
    A: ToNodeId
  {
    self.reduce_ref_with::<A, R>(R::default(), id, arg)
  }

  pub fn reduce_ref_with<A, Q>(&self, _red: Q, id: A, arg: &mut Q::Arg) -> Q::Output where
    A: ToNodeId, Q: Reduction<InnerCtx = R::InnerCtx>
  {
    let node_id = id.to_node_id().cast();
    let core = self.fork(node_id);
    self.aspect.reduce_ref::<Q>(core, arg)
  }
}

impl<'a, R> ModelCellCtxExt for ReduceCtxCore<'a, R> where
  R: Reduction
{
  fn model_cell(&self) -> &ModelCell {
    &self.aspect.input_model
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ReduceDyn<R> where
  R: Reduction
{
  fn reduce_dyn(&self, ctx: ReduceCtxDyn<R>, mode: (bool, bool));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ReduceDynAdapter<R, N: ?Sized, A> {
  reduction_type: PhantomData<R>,
  node_type: PhantomData<N>,
  rule: A
}

impl<R, N, A> ReduceDyn<R> for ReduceDynAdapter<R, N, A> where
  R: 'static + Reduction,
  N: Node + ?Sized,
  A: Reduce<R, N>,
  A::Output: Into<R::Output>
{
  fn reduce_dyn(&self, ctx: ReduceCtxDyn<R>, mode: (bool, bool)) {
    let (mut ctx, node, arg) = ctx;
    let node = ctx.model_cell().cast(node).ok().unwrap();

    if mode.0 {
      let result = self.rule.reduce((&mut ctx, node, arg)).into();
      ctx.aspect.add_reduction::<R>(ctx.node_id, result);
    }

    if mode.1 {
      let value = ctx.aspect.reduction::<R>(ctx.node_id).unwrap();
      self.rule.reduce_late((&mut ctx, node, arg), value);
      ctx.aspect.complete_reduction::<R>(ctx.node_id);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct ReduceRuleKey {
  reduction_type: TypeId,
  node_type: TypeId,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct ReduceRuleMap {
  rules: HashMap<ReduceRuleKey, Box<dyn Any>>
}

impl ReduceRuleMap {
  pub fn new() -> Self {
    Self { rules: HashMap::new() }
  }

  pub fn add<R, N, A>(&mut self, rule: A) where
    R: Reduction,
    N: Node + ?Sized,
    A: Reduce<R, N>,
    A::Output: Into<R::Output>
  {
    let key = ReduceRuleKey {
      reduction_type: TypeId::of::<R>(),
      node_type: TypeId::of::<N>(),
    };

    let adapter = ReduceDynAdapter {
      reduction_type: PhantomData,
      node_type: PhantomData,
      rule
    };

    let rule_dyn: Rc<dyn ReduceDyn<R>> = Rc::new(adapter);
    self.rules.insert(key, box rule_dyn);
  }

  pub fn get<R>(&self, node: &dyn Node) -> Rc<dyn ReduceDyn<R>> where
    R: Reduction
  {
    let node_type = node.type_id();
    match self.get_opt::<R>(node_type) {
      Some(rule) => rule,
      None => panic!("no reduce rule for {:?}", node.instance_concept().name())
    }
  }

  pub fn get_opt<R>(&self, node_type: TypeId) -> Option<Rc<dyn ReduceDyn<R>>> where
    R: Reduction
  {
    let key = ReduceRuleKey {
      reduction_type: TypeId::of::<R>(),
      node_type
    };

    self.rules.get(&key)
      .and_then(|x| dynamic_cast::<&dyn Any, &Rc<dyn ReduceDyn<R>>>(&**x).ok())
      .cloned()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! reduce_r {
  (__add, $aspect:expr, [$red_ty:ty, $node_ty:ty => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<GenAspect>($aspect) {
      aspect.reduce_rules.add::<$red_ty, $node_ty, _>($rule);
    }
  };

  (__method, reduce,
    ($red_ty:ty, $node_ty:ty),
    ($self:ident, $ctx:ident, $node:ident, $arg:ident),
    ($ret_ty:ty), $body:tt
  ) => {
    type Output = $ret_ty;
    fn reduce(
      &$self, ($ctx, $node, $arg): ReduceCtx<$red_ty, $node_ty>
    ) -> Self::Output { $body }
  };

  (__method, reduce_late,
    ($red_ty:ty, $node_ty:ty),
    ($self:ident, $ctx:ident, $node:ident, $arg:ident, $value:ident),
    (), $body:tt
  ) => {
    fn reduce_late(
      &$self, ($ctx, $node, $arg): ReduceCtx<$red_ty, $node_ty>,
      $value: <$red_ty as Reduction>::Output
    ) { $body }
  };

  (__trait, ($($generics:tt)*), ($($params:tt)*), $target:ty, $body:tt) => {
    impl $($generics)* Reduce<$($params)*> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
