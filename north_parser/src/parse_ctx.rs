use {
  crate::{
    Result,
    parser::{Parser, ParseEntry},
  },
  north_core::{
    Token, TokenOpaque,
    model::Model,
    prelude::*,
  },
  std::{
    any::TypeId,
    intrinsics::type_name
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ParseCtx<'g, 'm, 'p> where 'g: 'p, 'm: 'p {
  pub parser: &'p mut Parser<'g, 'm>, // Rc<RefCell<>>???
  pub min_prec: i32,
  pub offset: usize,
}

impl<'g, 'm, 'p> ParseCtx<'g, 'm, 'p> where 'g: 'p, 'm: 'p {
  pub fn accept<T>(&mut self) -> Option<Token<T>> where
    T: 'static + Clone
  {
    self.expect::<T>().ok()
  }

  pub fn build<N>(&mut self, node: N) -> Result<NodeId<N>> where
    N: Node
  {
    let node_id = self.parser.model.build_node(node);
    Ok(node_id)
  }

  pub fn current(&mut self) -> Option<&TokenOpaque> {
    self.fill();
    self.parser.tokens.get(self.offset)
  }

  pub fn error(&mut self, token: &TokenOpaque) {
    println!("unexpected token {:?}", token);
  }

  pub fn expect<T>(&mut self) -> Result<Token<T>> where
    T: 'static + Clone
  {
    let result = self.current()
      .and_then(|x| x.downcast_ref::<T>())
      .cloned();

//    if result.is_some() {
//      println!("EXPECT[{}] {:?}", offset, self.current());
//    }

    match result {
      Some(token) => { self.offset += 1; Ok(token) },
      None => Err(())
    }
  }

  fn fill(&mut self) {
    let offset = self.offset;
    self.parser.fill(offset);
  }

  pub fn fork<'c>(&'c mut self, min_prec: i32) -> ParseCtx<'g, 'm, 'c> {
    ParseCtx {
      parser: self.parser,
      min_prec,
      offset: self.offset,
    }
  }

  pub fn model_mut(&mut self) -> &mut Model {
    self.parser.model
  }

  pub fn new_entry(&self, precedence: i32, result: Option<NodeId>) -> ParseEntry {
    ParseEntry {
      end_offset: self.offset,
      precedence,
      result,
    }
  }

  // split to parse for Sized, and parse_dyn for unsized
  pub fn parse<'r, N>(&mut self) -> Result<NodeId<N>> where
    N: Node + ?Sized
  {
    self.parse_dyn(0)
  }

  pub fn parse_dyn<N>(&mut self, min_prec: i32) -> Result<NodeId<N>> where
    N: Node + ?Sized,
  {
    let rule_trait = self.parser.get_rule::<N>()
      .unwrap_or_else(|| panic!("rule trait has no implementations {:?}", 
        type_name::<N>() 
      ));

    let chart_key = (self.offset, TypeId::of::<N>());
    if let Some(result) = self.test_chart::<N>(min_prec) {
      return result;
    }

    let mut offset = self.offset;
    let mut result = Err(());

    // Prevent infinite left recursion
    let parse_entry = self.new_entry(0, None);
    self.parser.chart.insert(chart_key, parse_entry);

    for level in rule_trait.levels.iter() {
      if min_prec > level.precedence {
        continue;
      }

      loop {
        let mut changed = false;

        for parse_rule in level.parse_rules.iter() {
          // skip hints here

          let mut new_ctx = self.fork(level.precedence);
          if let Ok(node_id) = parse_rule.parse_dyn(&mut new_ctx) {
            let parse_entry = new_ctx.new_entry(level.precedence, Some(node_id.cast()));
            new_ctx.parser.chart.insert(chart_key, parse_entry);
            offset = new_ctx.offset;
            result = Ok(node_id);
            changed |= parse_rule.is_left_recursive();
          }
        }

        if !changed {
          break;
        }
      }
    }

    if result.is_ok() {
      self.offset = offset;
    }

    result.map(|i| i.cast())
  }

  pub fn parse_list<'r, N, E>(&mut self) -> Result<Vec<NodeId<N>>> where
    N: Node + ?Sized, E: 'static
  {
    let mut elems = Vec::new();

    loop {
      if self.peek::<E>() { break; }
      elems.push(self.parse::<N>()?);
    }

    Ok(elems)
  }

  pub fn parse_list_sep<'r, N, S>(&mut self) -> Result<Vec<NodeId<N>>> where
    N: Node + ?Sized, S: 'static + Clone
  {
    let mut elems = Vec::new();

    match self.parse::<N>() {
      Ok(elem) => { elems.push(elem); }
      Err(_) => { return Ok(elems); }
    }

    while self.accept::<S>().is_some() {
      elems.push(self.parse::<N>()?);
    }

    Ok(elems)
  }

  pub fn parse_rec<'r, N>(&mut self, delta_prec: i32) -> Result<NodeId<N>> where
    N: Node + ?Sized
  {
    let new_min_prec = self.min_prec + delta_prec;
    self.parse_dyn(new_min_prec)
  }

  pub fn peek<T>(&mut self) -> bool where
    T: 'static
  {
    match self.current() {
      Some(token) => (token.token_type() == TypeId::of::<T>()),
      None => false,
    }
  }

  pub fn test_chart<T>(&mut self, min_prec: i32) -> Option<Result<NodeId<T>>> where
    T: Node + ?Sized
  {
    let chart_key = (self.offset, TypeId::of::<T>());
    let offset = &mut self.offset;
    self.parser.chart.get(&chart_key).map(|entry| {
      if let Some(result) = entry.result {
        if entry.precedence >= min_prec {
          *offset = entry.end_offset;
          return Ok(result.cast());
        }
      }
      Err(())
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
