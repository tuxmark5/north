use {
  crate::Node,
  std::{
    any::TypeId,
    collections::HashMap,
    hash::Hash,
    marker::{PhantomData, Unsize},
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

/*pub trait FromRule<R> {
  fn from_rule(rule: R) -> Self;
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct RuleDynAdapter<N, R> where
  N: Node + ?Sized, R: 'static
{
  pub node_type: PhantomData<N>,
  pub rule: R,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct RuleKeySingleNode {
  pub node_type: TypeId
}

impl RuleKeySingleNode {
  pub fn new_static<N: 'static + ?Sized>() -> Self {
    Self { node_type: TypeId::of::<N>() }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct RuleMap<K, V> where
  K: Eq + Hash, V: ?Sized
{
  rules: HashMap<K, Rc<V>>
}

impl<K, V> RuleMap<K, V> where
  K: Eq + Hash, V: ?Sized
{
  pub fn new() -> Self {
    Self {
      rules: HashMap::new(),
    }
  }

  pub fn get(&self, key: K) -> Option<Rc<V>> {
    self.rules.get(&key).cloned()
  }
}

impl<V> RuleMap<RuleKeySingleNode, V> where
  V: ?Sized
{
  pub fn add<N, R>(&mut self, rule: R) where
    N: Node + ?Sized, R: 'static, RuleDynAdapter<N, R>: Unsize<V>
  {
    let key = RuleKeySingleNode::new_static::<N>();
    let rule = RuleDynAdapter { node_type: PhantomData, rule };
    let rule = Rc::new(rule);
    self.rules.insert(key, rule);
  }

  pub fn get_with_node(&self, node: &dyn Node) -> Rc<V> {
    match self.get_with_node_opt(node) {
      Some(rule) => rule,
      None => panic!("no print rule for node: {}", node.instance_concept().name())
    }
  }

  pub fn get_with_node_opt(&self, node: &dyn Node) -> Option<Rc<V>> {
    let node_type = node.type_id();
    let rule_key = RuleKeySingleNode { node_type };
    self.get(rule_key)
  }
}

impl<K, V> Default for RuleMap<K, V> where
  K: Eq + Hash, V: ?Sized
{
  fn default() -> Self {
    Self::new()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! rule_part {
  (
    $aspect_ty:ty, $rule_map:ident,
    $part:ident { $($node_ty:ty => $rule:expr,)* }
  ) => {
    pub struct $part;

    impl AspectPart for $part {
      type Aspect = $aspect_ty;

      fn setup(&self, aspect: &mut $aspect_ty) {
        $( aspect.$rule_map.add::<$node_ty, _>($rule); )*
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
