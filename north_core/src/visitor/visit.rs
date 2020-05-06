use {
  crate::{
    Node, NodeId,
    context::{
      ModelCellCtxExt,
      ModelNodeIdCtxExt,
      NodeIdCtxExt
    },
    iter::ChildrenRaw,
    model::ModelCell,
    node_id::ToNodeId,
    util::dynamic_cast,
    visitor::{Visitor, VisitorAspect},
  },
  north_derive::context,
  std::{
    any::{Any, TypeId},
    collections::HashMap,
    marker::PhantomData,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Visit<V, N>: 'static where
  V: Visitor, N: Node + ?Sized
{
  fn visit(&self, ctx: VisitCtx<V, N>) -> Result<(), ()>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type VisitCtx<'a, 'ia, 'ea, 'n, V, N> = (
  VisitCtxCore<'a, 'ia, V>, &'n N, &'ea mut <V as Visitor>::ExplicitArgs
);

////////////////////////////////////////////////////////////////////////////////////////////////

//#[context(ModelCellCtxExt, &self.aspect.input_model)]
#[context(ModelNodeIdCtxExt)]
#[context(NodeIdCtxExt, node_id)]
pub struct VisitCtxCore<'a, 'ia, V: Visitor> {
  pub aspect: &'a VisitorAspect,
  pub node_id: NodeId,
  pub imp_args: &'ia mut V::ImplicitArgs,
}

impl<'a, 'ia, V> VisitCtxCore<'a, 'ia, V> where
  V: Visitor
{
  pub fn fork(&mut self, node_id: NodeId) -> VisitCtxCore<V> {
    VisitCtxCore {
      aspect: self.aspect,
      node_id,
      imp_args: self.imp_args,
    }
  }

  pub fn visit<I>(
    &mut self, id: I, args: &mut V::ExplicitArgs
  ) -> Result<(), ()> where
    I: ToNodeId
  {
    let aspect = self.aspect;
    let new_ctx = self.fork(id.to_top());
    aspect.visit(new_ctx, args)
  }

  pub fn visit_children(
    &mut self, node: &dyn Node, args: &mut V::ExplicitArgs
  ) -> Result<(), ()>   {
    let children = ChildrenRaw::new(node).map(|e| e.1);
    self.visit_n(children, args)
  }

  pub fn visit_n<I>(
    &mut self, iter: I, args: &mut V::ExplicitArgs
  ) -> Result<(), ()> where
    I: IntoIterator, I::Item: ToNodeId,
  {
    for id in iter { self.visit(id, args)?; }
    Ok(())
  }
}

impl<'a, 'ia, V> ModelCellCtxExt for VisitCtxCore<'a, 'ia, V> where
  V: Visitor
{
  fn model_cell(&self) -> &ModelCell {
    &self.aspect.input_model
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait VisitDyn<V: Visitor> {
  fn visit_dyn(&self, ctx: VisitCtx<V, dyn Node>) -> Result<(), ()>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VisitDynAdapter<R, V, N> where
  R: Visit<V, N>, V: Visitor, N: Node + ?Sized, 
{
  rule: R,
  visitor_type: PhantomData<V>,
  node_type: PhantomData<N>,
}

impl<R, V, N> VisitDyn<V> for VisitDynAdapter<R, V, N> where
  R: Visit<V, N>, V: Visitor, N: Node + ?Sized, 
{
  fn visit_dyn(&self, (ctx, node, args): VisitCtx<V, dyn Node>) -> Result<(), ()> {
    let node = dynamic_cast::<_, &N>(node).ok().unwrap();
    self.rule.visit((ctx, node, args))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct VisitRuleKey {
  pub visitor_type: TypeId,
  pub node_type: TypeId,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct VisitRuleMap {
  default_rules: HashMap<TypeId, Box<dyn Any>>,
  rules: HashMap<VisitRuleKey, Box<dyn Any>>,
}

impl VisitRuleMap {
  pub fn new() -> Self {
    Self { 
      default_rules: HashMap::new(),
      rules: HashMap::new() 
    }
  }

  pub fn add<R, V, N>(&mut self, rule: R) where
    R: Visit<V, N>, V: Visitor, N: Node + ?Sized, 
  {
    let key = VisitRuleKey {
      visitor_type: TypeId::of::<V>(),
      node_type: TypeId::of::<N>(),
    };

    let adapter = VisitDynAdapter {
      rule,
      visitor_type: PhantomData,
      node_type: PhantomData,
    };

    let rule_dyn: Rc<dyn VisitDyn<V>> = Rc::new(adapter);
    self.rules.insert(key, box rule_dyn);
  }

  pub fn add_default<R, V>(&mut self, rule: R) where
    R: Visit<V, dyn Node>, V: Visitor
  {
    let key = TypeId::of::<V>();
    let adapter = VisitDynAdapter {
      rule,
      visitor_type: PhantomData,
      node_type: PhantomData,
    };

    let rule_dyn: Rc<dyn VisitDyn<V>> = Rc::new(adapter);
    self.default_rules.insert(key, box rule_dyn);
  }

  pub fn get<V>(&self, node: &dyn Node) -> Rc<dyn VisitDyn<V>> where
    V: Visitor
  {
    let node_type = node.type_id();
    match self.get_opt::<V>(node_type) {
      Some(rule) => rule,
      None => self.get_default::<V>(node),
    }
  }

  pub fn get_default<V>(&self, node: &dyn Node) -> Rc<dyn VisitDyn<V>> where
    V: Visitor
  {
    let key = TypeId::of::<V>();
    self.default_rules.get(&key)
      .and_then(|x| dynamic_cast::<&dyn Any, &Rc<dyn VisitDyn<V>>>(&**x).ok())
      .cloned()
      .unwrap_or_else(|| panic!("no visit rule for {:?}", node.instance_concept().name()))
  }

  pub fn get_opt<V>(&self, node_type: TypeId) -> Option<Rc<dyn VisitDyn<V>>> where
    V: Visitor
  {
    let key = VisitRuleKey {
      visitor_type: TypeId::of::<V>(),
      node_type
    };

    self.rules.get(&key)
      .and_then(|x| dynamic_cast::<&dyn Any, &Rc<dyn VisitDyn<V>>>(&**x).ok())
      .cloned()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! visit_r {
  (__add, $aspect:expr, [$visitor:ty, _ => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<VisitorAspect>($aspect) {
      aspect.visit_rules.add_default::<_, $visitor>($rule);
    }
  };

  (__add, $aspect:expr, [$visitor:ty, $node_ty:ty => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<VisitorAspect>($aspect) {
      aspect.visit_rules.add::<_, $visitor, $node_ty>($rule);
    }
  };

  (__method, visit,
    ($visitor:ty, $node_ty:ty),
    ($self:ident, $ctx:ident, $node:ident $(, $args:ident)*),
    (), $body:tt
  ) => {
    #[allow(unused_mut)]
    fn visit(
      &$self, (mut $ctx, $node, ($($args),*)): VisitCtx<$visitor, $node_ty>
    ) -> Result<(), ()> { 
      $body
    }
  };

  (__method, visit_ok,
    ($visitor:ty, $node_ty:ty),
    ($self:ident, $ctx:ident, $node:ident $(, $args:ident)*),
    (), $body:tt
  ) => {
    #[allow(unused_mut)]
    fn visit(
      &$self, (mut $ctx, $node, ($($args),*)): VisitCtx<$visitor, $node_ty>
    ) -> Result<(), ()> { 
      $body; Ok(())
    }
  };

  (__trait, ($($generics:tt)*), ($visitor:ty, $node_ty:ty), $target:ty, $body:tt) => {
    impl $($generics)* Visit<$visitor, $node_ty> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
