use {
  crate::{
    ParseCtx,
    grammar::{Grammar, RuleTrait},
  },
  north_core::{
    Model, NodeId, TokenOpaque,
  },
  std::{
    any::{TypeId},
    collections::HashMap,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type TokenIterator = Iterator<Item=TokenOpaque>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Parser<'g, 'm> {
  pub chart: HashMap<(usize, TypeId), ParseEntry>,
  pub grammar: &'g Grammar,
  pub model: &'m mut Model,
  pub source: Box<TokenIterator>,
  pub tokens: Vec<TokenOpaque>,
}

impl<'g, 'm> Parser<'g, 'm> {
  pub fn new(
    grammar: &'g Grammar,
    model: &'m mut Model,
    source: Box<TokenIterator>
  ) -> Self
  {
    Self {
      chart: HashMap::new(),
      grammar,
      model,
      source,
      tokens: Vec::new(),
    }
  }

  pub fn context<'p>(&'p mut self) -> ParseCtx<'g, 'm, 'p> {
    ParseCtx {
      parser: self,
      min_prec: 0,
      offset: 0,
    }
  }

  pub fn fill(&mut self, offset: usize) {
    while offset >= self.tokens.len() {
      match self.source.next() {
        Some(token) => self.tokens.push(token),
        None => break,
      }
    }
  }

  pub fn get_rule<T>(&self) -> Option<&'g RuleTrait> where
    T: 'static + ?Sized
  {
    self.grammar.get_rule::<T>()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ParseEntry {
  pub end_offset: usize,
  pub precedence: i32,
  pub result: Option<NodeId>
}

////////////////////////////////////////////////////////////////////////////////////////////////
