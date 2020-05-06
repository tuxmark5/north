use {
  crate::{
    mir,
    optimizer::RuleBuilder,
  },
  lang_mir::Cursor,
  north_core::{
    Node,
    util::dyn_traits::*,
  },
  std::{
    any::{Any, TypeId},
    hash::{Hash, Hasher},
    marker::PhantomData,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait BuildMIR {
  fn build_mir(&self, rb: &mut RuleBuilder, cur: &mut Cursor);
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Group: 'static + AsRef<dyn Any> + Eq + Hash + PartialEq {
  const ASYNC: bool = false;
  const PRIORITY: i32 = 0;

  type Value: Default;

  fn build_mir(
    &self,
    value: &Self::Value,
    rb: &mut RuleBuilder,
    cur: &mut Cursor
  );

  fn is_accepted(&self, _value: &Self::Value, _rb: &mut RuleBuilder) -> bool {
    true
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait GroupDyn: AsRef<dyn Any> + EqDyn + HashDyn + PartialEqDyn { 
  fn build_mir_dyn(
    &self,
    value: &dyn Any, 
    rb: &mut RuleBuilder, 
    cur: &mut Cursor
  );

  fn is_accepted(&self, value: &dyn Any, rb: &mut RuleBuilder) -> bool;

  fn is_async(&self) -> bool;

  fn priority(&self) -> i32;
}

impl<G: Group> GroupDyn for G { 
  fn build_mir_dyn(
    &self,
    value: &dyn Any,
    rb: &mut RuleBuilder, 
    cur: &mut Cursor
  ) {
    let value = value.downcast_ref::<G::Value>().unwrap();
    self.build_mir(value, rb, cur);
  }

  fn is_accepted(&self, value: &dyn Any, rb: &mut RuleBuilder) -> bool {
    let value = value.downcast_ref::<G::Value>().unwrap();
    self.is_accepted(value, rb)
  }

  fn is_async(&self) -> bool {
    G::ASYNC
  }

  fn priority(&self) -> i32 {
    G::PRIORITY
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LabelGroup<V> {
  value_type: PhantomData<V>
}

impl<V> LabelGroup<V> {
  pub fn new() -> Self {
    Self { value_type: PhantomData }
  }
}

impl<V: 'static> AsRef<dyn Any> for LabelGroup<V> {
  fn as_ref(&self) -> &dyn Any { self }
}

impl<V> Eq for LabelGroup<V> { }

impl<V: 'static> Hash for LabelGroup<V> { 
  fn hash<H: Hasher>(&self, state: &mut H) {
    TypeId::of::<V>().hash(state)
  }
}

impl<V> Group for LabelGroup<V> where 
  V: 'static + BuildMIR + Default 
{
  type Value = V;

  fn build_mir(
    &self,
    value: &Self::Value, 
    rb: &mut RuleBuilder, 
    cur: &mut Cursor
  ) {
    value.build_mir(rb, cur);
  }
}

impl<V> PartialEq for LabelGroup<V> { 
  fn eq(&self, _other: &Self) -> bool { true }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct UniqueGroup<N> {
  node: N
}

impl<N> UniqueGroup<N> {
  pub fn new(node: N) -> Self {
    Self { node }
  }
}

impl<N> AsRef<dyn Any> for UniqueGroup<N> where
  N: Node
{
  fn as_ref(&self) -> &dyn Any { self }
}

impl<N> Group for UniqueGroup<N> where
  N: Clone + Eq + Hash + Node + PartialEq + mir::Stmt
{
  type Value = ();

  fn build_mir(
    &self,
    _value: &Self::Value, 
    _rb: &mut RuleBuilder, 
    cur: &mut Cursor
  ) {
    let node = self.node.clone();
    cur.build_stmt(node);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
