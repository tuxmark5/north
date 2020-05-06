use {
  crate::{
    collections::{
      IndexMap, IntervalMap,
      index_map::Entry::*,
    },
    optimizer::{
      BuildDFA,
      closure::{Closure, ClosureId, ClosureSeed},
      closure_builder::{ClosureBuilder},
      closure_interner::{ClosureInterner},
      dfa::{self, DFA, Transitions},
    },
  },
  north_core::{
    node_id::{NodeId},
    visitor::{VisitorAspect},
  },
  std::{
    ops::{RangeInclusive},
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub enum Action {
  Complete(ClosureId),
  Fail(ClosureId),
  Shift(ClosureId),
  Unresolved(ClosureId),
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DFABuilder {
  crate closure_builder: Rc<ClosureBuilder>,
  crate closure_interner: Rc<ClosureInterner>,
  crate visitor_aspect: Rc<VisitorAspect>,

  crate instr_cache: IndexMap<ClosureId, Rc<Instr>>,

  crate completions: IndexMap<ClosureId, ()>,
  crate instrs: IndexMap<ClosureId, Rc<Instr>>,
}

impl DFABuilder {
  pub fn new(
    closure_builder: Rc<ClosureBuilder>,
    closure_interner: Rc<ClosureInterner>,
    visitor_aspect: Rc<VisitorAspect>,
  ) -> Self {
    Self {
      closure_builder,
      closure_interner,
      visitor_aspect,

      instr_cache: <_>::default(),

      completions: <_>::default(),
      instrs: <_>::default(),
    }
  }

  crate fn add_instr(&mut self, key: ClosureId, instr: Rc<Instr>) -> u16 {
    match self.instrs.entry(key) {
      Occupied(o) => { o.index() as u16 },//panic!("duplicate dfa instruction"),
      Vacant(v) => {
        let result = v.index() as u16;
        v.insert(instr);
        result
      }
    }
  }

  crate fn add_instr_new<I: Into<Instr>>(&mut self, key: ClosureId, instr: I) -> u16 {
    let instr = Rc::new(instr.into());

    if let Some(_) = self.instr_cache.insert(key, instr.clone()) {
      //panic!("duplicate cached dfa instruction");
    }

    self.add_instr(key, instr)
  }

  pub fn build_dfa(&mut self, closure_id: ClosureId) -> (Rc<DFA>, Vec<Closure>) {
    let _id = self.resolve_instr_id(closure_id);

    let dfa = self.compile_dfa();
    let completions = self.compile_completions();
    self.reset();

    (dfa, completions)
  }

  crate fn closure(&mut self, seed: ClosureSeed) -> ClosureId {
    let closure = self.closure_builder.build(seed);
    self.closure_interner.intern(closure)
  }

  crate fn compile_completions(&mut self) -> Vec<Closure> {
    let interner = &self.closure_interner;
    self.completions.keys()
      .map(|id| interner.get(*id).clone())
      .collect()
  }

  crate fn compile_dfa(&mut self) -> Rc<DFA> {
    let mut index = 0;
    let mut states = Vec::new();

    while index < self.instrs.len() {
      let entry = self.instrs.get_index(index);
      let instr = (*entry.unwrap().1).clone();
      let state = instr.compile(self);
      states.push(state);
      index += 1;
    }

    Rc::new(DFA { 
      states: states.into_boxed_slice()
    })
  }

  crate fn load_cached_instr(&mut self, closure_id: ClosureId) -> Option<u16> {
    if let Some((id, _, _)) = self.instrs.get_full(&closure_id) {
      return Some(id as u16);
    }

    let instr = self.instr_cache.get(&closure_id).cloned();
    instr.map(|instr| self.add_instr(closure_id, instr))
  }

  pub fn merge_closure_nodes(&mut self, closure_id: ClosureId) -> Option<DFABuilderState> {
    let closure = self.closure_interner.get(closure_id);
    let mut builder_state = DFABuilderState::default();

    for node_id in closure.iter() {
      let _ = self.visitor_aspect.visit_full::<BuildDFA, _>(
        node_id, &mut (), &mut builder_state
      );
    }

    if builder_state.has_rest() && !builder_state.prefer_shift {
      return None;
    }

    Some(builder_state)
  }

  pub fn reset(&mut self) {
    self.completions.clear();
    self.instrs.clear();
  }

  crate fn resolve_complete_id(&mut self, closure_id: ClosureId) -> u16 {
    let entry = self.completions.entry(closure_id);
    let index = entry.index();
    entry.or_insert(());
    index as u16
  }

  crate fn resolve_instr_id(&mut self, closure_id: ClosureId) -> u16 {
    if let Some(id) = self.load_cached_instr(closure_id) {
      return id as u16;
    }

    if closure_id.is_empty() {
      let instr = InstrFail { fail_closure: closure_id };
      return self.add_instr_new(closure_id, instr);
    }

    let builder_state = self.merge_closure_nodes(closure_id);
    let builder_state = match builder_state {
      Some(state) => state,
      None => return self.add_instr_new(closure_id, InstrComplete {
        result_closure: closure_id
      })
    };

    let complete_closure = self.closure(builder_state.rest);
    let eof_closure = self.closure(builder_state.fail);
    let mut state_builder = StateBuilder::new();

    for (range, seed) in &builder_state.next {
      let closure_id = self.closure(seed.clone());
      state_builder.add_range(range, closure_id);
    }

    let (transition_ids, transitions) = state_builder.compile();

    let shift_closure = self.closure(builder_state.ok);
    let shift_id = self.add_instr_new(shift_closure, InstrShift {
      eof_closure,
      targets: transitions,
      transitions: transition_ids,
    });

    if complete_closure.is_empty() {
      return shift_id;
    }

    self.add_instr_new(closure_id, InstrLookahead {
      try_closure: shift_closure,
      else_closure: complete_closure,
    })
  }

  crate fn resolve_instr_ids(&mut self, closure_ids: &[ClosureId]) -> Vec<u16> {
    closure_ids.iter()
      .map(|id| self.resolve_instr_id(*id))
      .collect::<Vec<_>>()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct DFABuilderState {
  crate fail: ClosureSeed,
  crate ok: ClosureSeed,
  crate rest: ClosureSeed,
  crate next: IntervalMap<u8, ClosureSeed>,
  crate prefer_shift: bool,
}

impl DFABuilderState {
  pub fn add_fail(&mut self, fail: ClosureSeed) {
    self.fail.merge(&fail);
  }

  pub fn add_map(&mut self, id: NodeId, map: IntervalMap<u8, ClosureSeed>) {
    self.ok.insert(id);
    self.next.merge(map.into_entries(), |a, b| a.merge(b));
  }

  pub fn add_prefer_shift(&mut self, prefer_shift: bool) {
    self.prefer_shift |= prefer_shift;
  }

  pub fn add_rest(&mut self, id: NodeId) {
    self.rest.insert(id);
  }

  pub fn has_rest(&self) -> bool {
    !self.rest.is_empty()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Instr {
  Complete(InstrComplete),
  Fail(InstrFail),
  Lookahead(InstrLookahead),
  Shift(InstrShift),
}

impl Instr {
  crate fn compile(&self, db: &mut DFABuilder) -> dfa::State {
    match self {
      Instr::Complete(i) => i.compile(db),
      Instr::Fail(i) => i.compile(db),
      Instr::Lookahead(i) => i.compile(db),
      Instr::Shift(i) => i.compile(db),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct InstrComplete {
  crate result_closure: ClosureId,
}

impl InstrComplete {
  crate fn compile(&self, db: &mut DFABuilder) -> dfa::State {
    dfa::State::Complete { 
      result_id: db.resolve_complete_id(self.result_closure) 
    }
  }
}

impl From<InstrComplete> for Instr {
  fn from(i: InstrComplete) -> Self { 
    Instr::Complete(i)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct InstrFail {
  crate fail_closure: ClosureId,
}

impl InstrFail {
  crate fn compile(&self, db: &mut DFABuilder) -> dfa::State {
    dfa::State::Fail { 
      result_id: db.resolve_complete_id(self.fail_closure) 
    }
  }
}

impl From<InstrFail> for Instr {
  fn from(i: InstrFail) -> Self { 
    Instr::Fail(i)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct InstrLookahead {
  crate try_closure: ClosureId,
  crate else_closure: ClosureId,
}

impl InstrLookahead {
  crate fn compile(&self, db: &mut DFABuilder) -> dfa::State {
    dfa::State::Lookahead { 
      try_state: db.resolve_instr_id(self.try_closure),
      complete_id: db.resolve_complete_id(self.else_closure),
    }
  }
}

impl From<InstrLookahead> for Instr {
  fn from(i: InstrLookahead) -> Self { 
    Instr::Lookahead(i)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct InstrShift {
  crate eof_closure: ClosureId,
  crate targets: Vec<ClosureId>,
  crate transitions: Transitions,
}

impl InstrShift {
  crate fn compile(&self, db: &mut DFABuilder) -> dfa::State {
    dfa::State::Shift { 
      eof_state: db.resolve_instr_id(self.eof_closure),
      state_ids: db.resolve_instr_ids(self.targets.as_ref()),
      transitions: self.transitions.clone(),
    }
  }
}

impl From<InstrShift> for Instr {
  fn from(i: InstrShift) -> Self { 
    Instr::Shift(i)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct StateBuilder {
  crate transitions: IndexMap<ClosureId, ()>,
  crate transition_ids: [u8; 256],
}

impl StateBuilder {
  pub fn new() -> Self {
    Self {
      transitions: <_>::default(),
      transition_ids: [0xFF; 256],
    }
  }

  pub fn add_range(&mut self, range: RangeInclusive<u8>, closure_id: ClosureId) {
    let transition_id = self.resolve_transition_id(closure_id);
    for i in range {
      let slot = &mut self.transition_ids[i as usize];
      if *slot == 0xFF {
        *slot = transition_id;
      } else {
        panic!("overlapping ranges")
      }
    }
  }

  pub fn compile(mut self) -> (Transitions, Vec<ClosureId>) {
    if self.transitions.len() < 256 {
      let fail_closure = ClosureId::EMPTY;
      let fail_transition_id = self.resolve_transition_id(fail_closure);
      for val in &mut self.transition_ids as &mut [u8] {
        if *val == 0xFFu8 { *val = fail_transition_id; }
      }
    }

    let transition_ids = Rc::new(self.transition_ids);
    let transitions = self.transitions.keys().cloned().collect();
    (transition_ids, transitions)
  }

  pub fn resolve_transition_id(&mut self, closure_id: ClosureId) -> u8 {
    let entry = self.transitions.entry(closure_id);
    let transition_id = entry.index() as u8;
    let _ = entry.or_insert(());
    transition_id
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
