use {
  crate::{
    Scope,
    derive_scope::{
      DeriveScope,
      DeriveScopeCtxCore,
      DeriveScopeDyn,
      DeriveScopeDynAdapter,
    },
    resolve::{Resolve, ResolveDyn, ResolveRuleMap},
    scope::ScopePtrOpt,
  },
  north_core::{
    compiler::{
      aspect::Aspect,
    },
    model::ModelCell,
    prelude::*,
  },
  std::{
    any::{TypeId},
    cell::RefCell,
    collections::HashMap,
    rc::Rc
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
struct ScopeCacheKey {
  target_node: NodeId,
  ref_node: Option<NodeId>,
  scope_type: TypeId,
}

////////////////////////////////////////////////////////////////////////////////////////////////

type ScopeCacheMap = HashMap<ScopeCacheKey, Option<Rc<dyn Scope>>>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
struct DeriveRuleKey {
  node_type: TypeId,
  scope_type: TypeId
}

impl DeriveRuleKey {
  fn new<N, K>() -> Self where
    N: 'static, K: 'static
  {
    Self {
      node_type: TypeId::of::<N>(),
      scope_type: TypeId::of::<K>(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ScopeAspect {
  derive_rules: HashMap<DeriveRuleKey, Rc<dyn DeriveScopeDyn>>,
  derive_rules_default: HashMap<TypeId, Rc<dyn DeriveScopeDyn>>,
  model_cell: ModelCell,
  pub resolve_rules: ResolveRuleMap,
  scope_cache: RefCell<ScopeCacheMap>,
}

impl ScopeAspect {
  pub fn new(model_cell: ModelCell) -> Self {
    Self {
      derive_rules: HashMap::new(),
      derive_rules_default: HashMap::new(),
      model_cell,
      resolve_rules: ResolveRuleMap::new(),
      scope_cache: RefCell::new(ScopeCacheMap::new()),
    }
  }

  pub fn add_derive<N, K, R>(&mut self, rule: R) where
    N: Node, K: 'static, R: 'static + DeriveScope<N, K>
  {
    let rule_key = DeriveRuleKey::new::<N, K>();
    let rule = DeriveScopeDynAdapter::wrap(rule);
    self.derive_rules.insert(rule_key, rule);
  }

  pub fn add_derive_default<K, R>(&mut self, rule: R) where
    K: 'static, R: 'static + DeriveScope<dyn Node, K>
  {
    let rule_key = TypeId::of::<K>();
    let rule = DeriveScopeDynAdapter::wrap(rule);
    self.derive_rules_default.insert(rule_key, rule);
  }

  pub fn add_resolve<N, R>(&mut self, rule: R) where
    N: Node + ?Sized, R: Resolve<N>
  {
    self.resolve_rules.add::<N, R>(rule);
  }

  pub fn derive_scope<S>(
    self        : Rc<Self>,
    node_id     : NodeId,
    ref_node_id : NodeId,
  ) -> ScopePtrOpt where
    S: 'static
  {
    let node_cell = self.model_cell.node_cell_dyn(node_id);
    let node_ref = node_cell.borrow();
    let node_type = node_ref.type_id();
    let rule = self.get_derive_rule::<S>(node_type)?;

    let cache_key = ScopeCacheKey {
      target_node: node_id,
      ref_node: if rule.is_relative() { Some(ref_node_id) } else { None },
      scope_type: TypeId::of::<S>(),
    };

    {
      let scope_cache = self.scope_cache.borrow();
      if let Some(scope) = scope_cache.get(&cache_key) {
        return scope.clone();
      } else {
        // TODO: insert marker to prevent recursive scopes
      }
    }


    let scope = {
      let core = DeriveScopeCtxCore {
        aspect: self.clone(),
        model_cell: self.model_cell.clone(),
        node_id,
        ref_node_id,
      };

      let node = &*node_ref;
      rule.derive_scope_dyn((core, &*node))
    };

    {
      let mut scope_cache = self.scope_cache.borrow_mut();
      scope_cache.insert(cache_key, scope.clone());
    }

    scope
  }

  pub fn get_derive_rule<K>(&self, node_type: TypeId)
    -> Option<Rc<dyn DeriveScopeDyn>> where
    K: 'static
  {
    let scope_type = TypeId::of::<K>();
    let derive_rule_key = DeriveRuleKey { node_type, scope_type };

    let rule = self.derive_rules.get(&derive_rule_key);
    if rule.is_some() { return rule.cloned(); }

    let rule = self.derive_rules_default.get(&scope_type);
    if rule.is_some() { return rule.cloned(); }

    panic!("no derive");
  }

  pub fn get_resolve_rule(&self, node: &dyn Node) -> Option<Rc<dyn ResolveDyn>> {
    self.resolve_rules.get_with_node_opt(node)
  }
}

impl Aspect for ScopeAspect {
  fn create(comp: &Compiler) -> Self where Self: Sized {
    Self::new(comp.model_cell.clone())
  }

  fn name(&self) -> &str {
    "scope"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
