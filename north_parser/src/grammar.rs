use {
  crate::{
    parse::{Parse, ParseDyn, ParseDynAdapter},
  },
  north_core::Node,
  std::{
    any::TypeId,
    collections::HashMap,
    marker::{PhantomData, Unsize},
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct Grammar {
  rule_traits: HashMap<TypeId, RuleTrait>
}

impl Grammar {
  pub fn new() -> Self {
    Self {
      rule_traits: HashMap::new()
    }
  }

  pub fn add_rule<T>(&mut self) -> RuleBuilder<T> where
    T: 'static + ?Sized
  {
    let trait_type = TypeId::of::<T>();
    let entry = self.rule_traits.entry(trait_type);

    RuleBuilder::<T> {
      rule_type: PhantomData,
      rule_trait: entry.or_default(),
    }
  }

  pub fn add_rule_ex<A, B, R>(&mut self, precedence: i32, rule: R) where
    A: 'static + ?Sized, B: Node, R: 'static + Parse<B>
  {
    let trait_type = TypeId::of::<A>();
    let entry = self.rule_traits.entry(trait_type);
    let rule_trait = entry.or_default();
    let rule_dyn = ParseDynAdapter::wrap(rule);
    rule_trait.add_rule(precedence, rule_dyn);
  }

  pub fn add_rule_simple<N, R>(&mut self, rule: R) where
    N: Node + ?Sized, R: 'static + Parse<N>
  {
    let node_type = TypeId::of::<N>();
    let rule_dyn = ParseDynAdapter::wrap(rule);
    let prec_level = PrecedenceLevel { precedence: 100, parse_rules: vec![rule_dyn] };
    let rule_trait = RuleTrait { levels: vec![prec_level] };
    self.rule_traits.insert(node_type, rule_trait);
  }

  pub fn get_rule<T>(&self) -> Option<&RuleTrait> where
    T: 'static + ?Sized
  {
    let trait_type = TypeId::of::<T>();
    self.rule_traits.get(&trait_type)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct PrecedenceLevel {
  pub precedence: i32,
  pub parse_rules: Vec<Rc<dyn ParseDyn>>,
}

impl PrecedenceLevel {
  pub fn new(precedence: i32) -> Self {
    Self {
      precedence,
      parse_rules: Vec::new(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct RuleBuilder<'a, T: ?Sized> {
  rule_type: PhantomData<T>,
  rule_trait: &'a mut RuleTrait,
}

impl<'a, T: ?Sized> RuleBuilder<'a, T> {
  pub fn add<N, R>(self, precedence: i32, rule: R) -> Self where
    N: Node + Unsize<T>, R: 'static + Parse<N>
  {
    let rule = ParseDynAdapter::wrap(rule);
    self.rule_trait.add_rule(precedence, rule);
    self
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct RuleTrait {
  pub levels: Vec<PrecedenceLevel>
}

impl RuleTrait {
  pub fn add_rule(&mut self, precedence: i32, rule: Rc<dyn ParseDyn>) {
    let pos = self.levels.binary_search_by(|e| {
      e.precedence.cmp(&precedence).reverse()
    });

    let level = match pos {
      Ok(pos) => {
        &mut self.levels[pos]
      },
      Err(pos) => {
        self.levels.insert(pos, PrecedenceLevel::new(precedence));
        &mut self.levels[pos]
      },
    };

    level.parse_rules.push(rule);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
