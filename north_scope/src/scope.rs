use {
  north_core::NodeId,
  std::rc::Rc
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Scope {
  fn collect(&self, target: &mut Vec<(String, NodeId)>);
  fn lookup(&self, name: &str) -> Option<NodeId>;

  fn wrap(self) -> ScopePtr where Self: 'static + Sized {
    Rc::new(self)
  }

  fn wrap_opt(self) -> ScopePtrOpt where Self: 'static + Sized {
    Some(Rc::new(self))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ScopePtr = Rc<dyn Scope>;
pub type ScopePtrOpt = Option<ScopePtr>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Empty;

impl Empty {
  pub fn new() -> Rc<Self> {
    Rc::new(Empty)
  }
}

impl Scope for Empty {
  fn collect(&self, _target: &mut Vec<(String, NodeId)>) { }
  fn lookup(&self, _name: &str) -> Option<NodeId> { None }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Composite {
  inner: Vec<Rc<dyn Scope>>
}

impl Composite {
  pub fn new(inner: Vec<ScopePtr>) -> Self {
    Self { inner }
  }

  pub fn make(mut inner: Vec<ScopePtr>) -> ScopePtrOpt {
    match inner.len() {
      0 => None,
      1 => inner.pop(),
      2 => Pair::new(inner.pop().unwrap(), inner.pop().unwrap()).wrap_opt(),
      _ => Self::new(inner).wrap_opt(),
    }
  }
}

impl Scope for Composite {
  fn collect(&self, target: &mut Vec<(String, NodeId)>) {
    for scope in self.inner.iter() {
      scope.collect(target);
    }
  }

  fn lookup(&self, name: &str) -> Option<NodeId> {
    self.inner.iter().find_map(|scope| scope.lookup(name))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Pair {
  scope_a: Rc<dyn Scope>,
  scope_b: Rc<dyn Scope>
}

impl Pair {
  pub fn new(scope_a: Rc<dyn Scope>, scope_b: Rc<dyn Scope>) -> Self {
    Self { scope_a, scope_b }
  }

  pub fn make(scope_a: ScopePtrOpt, scope_b: ScopePtrOpt) -> ScopePtrOpt {
    match (scope_a, scope_b) {
      (Some(a), Some(b)) => Some(Pair::new(a, b).wrap()),
      (scope_a, scope_b) => scope_a.or(scope_b),
    }
  }
}

impl Scope for Pair {
  fn collect(&self, target: &mut Vec<(String, NodeId)>) {
    self.scope_a.collect(target);
    self.scope_b.collect(target);
  }

  fn lookup(&self, name: &str) -> Option<NodeId> {
    self.scope_b.lookup(name)
      .or_else(|| self.scope_a.lookup(name))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Single {
  name: String, // switch to Atom
  target: NodeId,
}

impl Single {
  pub fn new(name: String, target: NodeId) -> Self {
    Single { name, target }
  }

  pub fn make<N: ToString>(name: N, target: NodeId) -> ScopePtrOpt {
    Self::new(name.to_string(), target).wrap_opt()
  }
}

impl Scope for Single {
  fn collect(&self, target: &mut Vec<(String, NodeId)>) {
    let entry = (self.name.clone(), self.target);
    target.push(entry);
  }

  fn lookup(&self, name: &str) -> Option<NodeId> {
    if name == self.name {
      Some(self.target)
    } else {
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
