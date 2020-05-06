use {
  crate::{
    Reduction,
    reduce::{
      ReduceCtxCore,
      ReduceRuleMap
    },
  },
  north_core::{
    compiler::{Aspect, Compiler},
    model::{ModelCell, PropMap},
    prelude::*,
    util::dynamic_cast,
  },
  std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct ReduceKey {
  reduction_id: TypeId,
  node_id: NodeId,
}

impl ReduceKey {
  fn new<R: Reduction>(node_id: NodeId) -> Self {
    Self {
      reduction_id: TypeId::of::<R>(),
      node_id
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ReduceValue {
  value: Box<dyn Any>,
  completed: bool,
}

impl ReduceValue {
  fn new<V: 'static>(value: V, completed: bool) -> Self {
    Self {
      value: box value,
      completed,
    }
  }

  fn get<V: 'static + Clone>(&self) -> V {
    dynamic_cast::<_, &V>(&*self.value).unwrap().clone()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct GenAspect {
  pub input_model: ModelCell,
  pub properties: RefCell<PropMap>,
  pub reduce_rules: ReduceRuleMap,
  pub reductions: RefCell<HashMap<ReduceKey, ReduceValue>>,
}

impl GenAspect {
  pub fn new(input_model: ModelCell) -> Self {
    Self {
      input_model,
      properties: RefCell::new(PropMap::new()),
      reduce_rules: ReduceRuleMap::new(),
      reductions: RefCell::new(HashMap::new()),
    }
  }

  pub fn add_reduction<R>(&self, node_id: NodeId, value: R::Output) where
    R: Reduction
  {
    let mut reductions = self.reductions.borrow_mut();
    let key = ReduceKey::new::<R>(node_id);
    let value = ReduceValue::new(value, false);
    reductions.insert(key, value);
  }

  pub fn call_reduce<'a, R>(
    &self,
    core : ReduceCtxCore<'a, R>,
    arg  : &mut R::Arg,
    mode : (bool, bool),
  ) -> R::Output where R: Reduction
  {
    let node_id = core.node_id;
    let node_cell = self.input_model.node_cell_dyn(node_id);
    let node_ref = node_cell.borrow();
    let reduce_rule = self.reduce_rules.get::<R>(&*node_ref);

    let node = &*node_ref;
    reduce_rule.reduce_dyn((core, node, arg), mode);

    self.reduction::<R>(node_id).unwrap()
  }

  pub fn complete_reduction<R>(&self, node_id: NodeId) where
    R: Reduction
  {
    let mut reductions = self.reductions.borrow_mut();
    let key = ReduceKey::new::<R>(node_id);
    let value = reductions.get_mut(&key).unwrap();
    value.completed = true;
  }

  pub fn core_ctx<'a, R>(
    &'a self,
    node_id   : NodeId,
    inner     : R::InnerCtx
  ) -> ReduceCtxCore<'a, R> where
    R: Reduction
  {
    ReduceCtxCore {
      aspect: self,
      node_id,
      inner
    }
  }

  pub fn reduce_cld<'a, R>(
    &self,
    core : ReduceCtxCore<'a, R>,
    arg  : &mut R::Arg,
  ) -> R::Output where R: Reduction
  {
    let mode = match R::CACHE_RESULT {
      true => self.reduction_mode::<R>(core.node_id),
      false => (true, true),
    };
    self.call_reduce(core, arg, mode)
  }

  pub fn reduce_link<'a, R>(
    &self,
    core : ReduceCtxCore<'a, R>,
    arg  : &mut R::Arg,
  ) -> R::Output where R: Reduction
  {
    self.reduce_ref::<R>(core, arg)
  }

  pub fn reduce_ref<'a, R>(
    &self,
    core : ReduceCtxCore<'a, R>,
    arg  : &mut R::Arg,
  ) -> R::Output where R: Reduction
  {
    let value = self.reduction::<R>(core.node_id);
    if let Some(value) = value { return value; }
    self.call_reduce(core, arg, (true, false))
  }

  pub fn reduction<R>(&self, node_id: NodeId) -> Option<R::Output> where
    R: Reduction
  {
    let reductions = self.reductions.borrow();
    let key = ReduceKey::new::<R>(node_id);
    let value = reductions.get(&key)?.get::<R::Output>();
    Some(value)
  }

  pub fn reduction_mode<R>(&self, node_id: NodeId) -> (bool, bool) where
    R: Reduction
  {
    let reductions = self.reductions.borrow();
    let key = ReduceKey::new::<R>(node_id);
    match reductions.get(&key) {
      Some(value) => (false, !value.completed),
      None => (true, true),
    }
  }
}

impl Aspect for GenAspect {
  fn create(comp: &Compiler) -> Self where Self: Sized {
    Self::new(comp.model_cell.clone())
  }

  fn name(&self) -> &str { "gen" }
}

////////////////////////////////////////////////////////////////////////////////////////////////
