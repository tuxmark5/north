use {
  crate::NodeId,
  std::{
    any::{Any, TypeId},
    collections::HashMap,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Prop: 'static + Sized {
  const NAME: &'static str;
  type Type;
}

#[macro_export]
macro_rules! impl_prop {
  ($name:ident, $prop_ty:ty) => {
    #[allow(non_camel_case_types)]
    pub struct $name;

    impl $crate::model::Prop for $name {
      const NAME: &'static str = stringify!($name);
      type Type = $prop_ty;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Hash, Eq, PartialEq)]
pub struct PropKey {
  node_id: NodeId,
  prop_id: TypeId,
}

impl PropKey {
  pub fn new<P: Prop>(node_id: NodeId) -> Self {
    Self {
      node_id,
      prop_id: TypeId::of::<P>(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct PropMap {
  props: HashMap<PropKey, Box<dyn Any>>
}

impl PropMap {
  pub fn new() -> Self {
    Self::default()
  }
}

impl PropMapExt for PropMap {
  fn prop_map(&self) -> &PropMap { self }
  fn prop_map_mut(&mut self) -> &mut PropMap { self }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait PropMapExt {
  fn prop_map(&self) -> &PropMap;
  fn prop_map_mut(&mut self) -> &mut PropMap;

  fn prop<P: Prop>(&self, id: NodeId) -> Option<&P::Type> {
    let prop_key = PropKey::new::<P>(id);
    self.prop_map().props.get(&prop_key)
      .map(|p| (&**p).downcast_ref::<P::Type>().unwrap())
  }

  fn prop_mut<P: Prop>(&mut self, id: NodeId) -> Option<&mut P::Type> {
    let prop_key = PropKey::new::<P>(id);
    self.prop_map_mut().props.get_mut(&prop_key)
      .map(|p| (&mut **p).downcast_mut::<P::Type>().unwrap())
  }

  fn prop_set<P: Prop>(&mut self, id: NodeId, _prop: P, value: P::Type) {
    let prop_key = PropKey::new::<P>(id);
    let prop_value = box value;
    self.prop_map_mut().props.insert(prop_key, prop_value);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
