use {
  crate::{
    mir,
    collections::{
      BitMaskBuilder, IndexMap, Table,
      bit_mask, table,
    },
    runtime::{
      CallElem, MatchElem, MatchMap, ParserAlloc, ParserCtx, TaskCtorFn,
      prelude::*,
    },
  },
  north_core::{
    model::ModelCell,
    node_id::{NodeId, ToNodeId},
  },
  std::{
    iter,
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type BitMask = bit_mask::BitMask<ParserAlloc>;
pub type RuleMap = Table<MatchId, Vec<RuleEntry>>;
pub type RuleSet = Vec<NodeId>;
pub type Transitions = IndexMap<ReduceId, Vec<StateId>>;
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CtorDescr {
  crate ctor: TaskCtorFn,
  crate mask: BitMask,
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct MatchSpecData {
  crate mask: BitMask,
  crate transitions: Transitions,
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Grammar {
  crate context: Rc<ParserCtx>,
  crate call_cache: Table<CallSpecId, CtorDescr>,
  crate match_specs: Table<MatchSpecId, MatchSpecData>,
  crate match_map: MatchMap,
  crate rules: Table<MatchId, Vec<RuleEntry>>,
}

impl Grammar {
  pub fn new(context: Rc<ParserCtx>) -> Self {
    Self { 
      context,
      call_cache: <_>::default(),
      match_map: MatchMap::new(),
      match_specs: <_>::default(),
      rules: <_>::default(),
    }
  }

  pub fn add(&mut self, model: ModelCell, grammar_id: NodeId<mir::ItemGrammar>) {
    let model = model.borrow();
    let grammar = model.node(grammar_id);

    for i in &grammar.impls {
      let entry = RuleEntry {
        reduce_id: i.reduce_id,
        prec: i.prec,
        rule_id: Some(i.rule.to_top()),
        ctor_fn: None,
      };

      self.match_map.insert(i.match_id, i.reduce_id, i.prec);
      self.rules.entry(i.match_id)
        .or_insert_with(<_>::default)
        .push(entry);
    }
  }

  pub fn add_ctor(&mut self, cs_id: CallSpecId, ctor: TaskCtorFn) -> CtorDescr {
    let spec = self.context.get_call_spec(cs_id);
    let mask = self.build_call_spec_mask(&*spec);
    let descr = CtorDescr { ctor, mask };
    self.call_cache.insert(cs_id, descr.clone());
    descr
  }

  pub fn build_call_spec_mask(&self, call_spec: &[CallElem]) -> BitMask {
    let mut builder = BitMaskBuilder::new();

    for call_e in call_spec {
      if let Some(entries) = self.rules.get(call_e.match_id) {
        for rule_e in entries {
          if call_e.min_prec > rule_e.prec { continue }
          let reduce_id = rule_e.reduce_id.into();
          builder.insert(reduce_id);
        }
      }
    }

    builder.into_mask()
  }

  pub fn build_match_spec_mask(rules: &RuleMap, match_spec: &[MatchElem]) -> BitMask {
    let mut builder = BitMaskBuilder::new();

    for match_e in match_spec {
      if let Some(entries) = rules.get(match_e.match_id) {
        for rule_e in entries {
          if !match_e.prec_range.contains(&rule_e.prec) { continue }
          let reduce_id = rule_e.reduce_id.into();
          builder.insert(reduce_id);
        }
      }
    }

    builder.into_mask()
  }

  /*pub fn build_transitions(match_map: &MatchMap, spec: &[MatchElem], reduce_id: ReduceId) -> Vec<StateId> {
    let mut positive_states = Vec::new();

    for e in spec {
      if match_map.matches(e.match_id, reduce_id, e.prec_range.clone()) {
        positive_states.push(e.state_id);
      }
    }

    positive_states
  }*/

  pub fn build_transitions(
    rules: &Table<MatchId, Vec<RuleEntry>>,
    match_spec: &[MatchElem],
  ) -> Transitions {
    let mut transitions = Transitions::default();

    for match_elem in match_spec {
      let rule_entries = match rules.get(match_elem.match_id) {
        Some(entries) => entries,
        None => continue,
      };

      for rule in rule_entries {
        if match_elem.prec_range.contains(&rule.prec) {
          let entry = transitions.entry(rule.reduce_id);
          let states = entry.or_insert_with(<_>::default);
          if !states.contains(&match_elem.state_id) {
            states.push(match_elem.state_id);
          }
        }
      }
    }

    transitions
  }

  pub fn clear(&mut self) {
    self.call_cache.clear();
    self.match_map.clear();
    self.match_specs.clear();
    self.rules.clear();
  }

  pub fn dump_info(&self) {
    println!("GRAMMAR:");
    println!("  call_cache size: {}", self.call_cache.len());
    println!("  match_map size: {}", self.match_map.len());
    println!("  match_specs size: {}", self.match_specs.len());
    println!("  rules size: {}", self.rules.len());
  }

  pub fn lookup_ctor(&self, call_spec_id: CallSpecId) -> Option<CtorDescr> {
    self.call_cache.get(call_spec_id).cloned()
  }

  pub fn match_spec_data(&mut self, ms_id: MatchSpecId) -> &mut MatchSpecData {
    match self.match_specs.entry(ms_id) {
      table::Entry::Occupied(occupied) => occupied.get_mut(),
      table::Entry::Vacant(vacant) => {
        let spec = self.context.get_match_spec(ms_id);
        let mask = Self::build_match_spec_mask(&self.rules, &*spec);
        let transitions = Self::build_transitions(&self.rules, &*spec);
        let data = MatchSpecData { mask, transitions };
        vacant.insert(data)
      }
    }
  }

  pub fn match_spec_mask(&mut self, ms_id: MatchSpecId) -> &BitMask {
    &self.match_spec_data(ms_id).mask
  }

  pub fn matching_iter<'a>(&'a self, match_id: MatchId, min_prec: u16) 
    -> Box<dyn 'a + Iterator<Item=NodeId>>
  {
    if let Some(entries) = self.rules.get(match_id) {
      box entries.iter()
        .filter(move |e| min_prec <= e.prec)
        .map(|e| e.rule_id.unwrap())
    } else {
      box iter::empty()
    }
  }

  pub fn matching_rules(&self, call_spec: &[CallElem]) -> (RuleSet, Vec<ReduceId>) {
    let mut rule_set = RuleSet::new();
    let mut reduce_ids = Vec::new();

    for call_e in call_spec {
      if let Some(entries) = self.rules.get(call_e.match_id) {
        for rule_e in entries {
          if call_e.min_prec > rule_e.prec { continue }

          let rule_id = rule_e.rule_id.unwrap();
          if let Err(idx) = rule_set.binary_search(&rule_id) {
            rule_set.insert(idx, rule_id);
          }

          let reduce_id = rule_e.reduce_id;
          if !reduce_ids.contains(&reduce_id) {
            reduce_ids.push(reduce_id);
          }
        }
      }
    }

    (rule_set, reduce_ids)
  }

  pub fn positive_matches(&mut self, spec_id: MatchSpecId, reduce_id: ReduceId) -> &[StateId] {
    let _ = self.match_spec_data(spec_id);
    let data = self.match_specs.get_mut(spec_id).unwrap();
    match data.transitions.get(&reduce_id) {
      Some(states) => states,
      None => &[],
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct GrammarVec {
  context: Rc<ParserCtx>,
  grammars: Vec<Rc<Grammar>>
}

impl GrammarVec {
  pub fn new(context: Rc<ParserCtx>) -> Self {
    Self { 
      context,
      grammars: Vec::new() 
    }
  }

  pub fn clear(&mut self) {
    self.grammars.clear();
  }

  pub fn dump_info(&self) {
    for g in &self.grammars {
      g.dump_info();
    }
  }

  pub fn get(&self, idx: GrammarId) -> &Grammar {
    let idx: usize = idx.into();
    &self.grammars[idx]
  }

  pub fn get_mut(&mut self, idx: GrammarId) -> &mut Grammar {
    let idx: usize = idx.into();
    Rc::get_mut(&mut self.grammars[idx]).unwrap()
  }

  pub fn get_rc(&self, idx: GrammarId) -> Rc<Grammar> {
    let idx: usize = idx.into();
    self.grammars[idx].clone()
  }

  pub fn match_map(&self, idx: GrammarId) -> &MatchMap {
    let idx: usize = idx.into();
    &self.grammars[idx].match_map
  }

  pub fn new_grammar(&mut self) -> &mut Grammar {
    let context = self.context.clone();
    let grammar = Rc::new(Grammar::new(context));
    self.grammars.push(grammar);

    let index = self.grammars.len() - 1;
    self.get_mut(index.into())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct RuleEntry {
  pub reduce_id: ReduceId,
  pub prec: u16,
  pub rule_id: Option<NodeId>,
  pub ctor_fn: Option<TaskCtorFn>,
}

////////////////////////////////////////////////////////////////////////////////////////////////
