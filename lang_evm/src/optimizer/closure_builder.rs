use {
  crate::{
    optimizer::{BuildClosure, Closure, ClosureSeed},
    runtime::Grammar,
    util::bit_flags,
  },
  north_core::{
    node_id::{NodeId, ToNodeId},
    visitor::{VisitorAspect},
  },
  std::{
    cell::RefCell,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

type CF = ClosureFlags;

bit_flags! {
  pub enum ClosureFlags, ClosureFlagsTag: u32 {
    pub ASYNC_SUCC  = 1 << 0,
    pub BLOCK       = 1 << 1,
    pub BLOCK_FIRST = 1 << 2,
    pub CALL        = 1 << 3,
    pub CALL_TARGET = 1 << 4,
    pub REDUCE      = 1 << 5,
    pub RELEVANT    = 1 << 6,
    pub SYNC_SUCC   = 1 << 7,
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ClosureBuilder {
  pub grammar: RefCell<Option<Rc<Grammar>>>,
  pub visitor_aspect: Rc<VisitorAspect>,
}

impl ClosureBuilder {
  pub fn new(visitor_aspect: Rc<VisitorAspect>) -> Self {
    Self {
      grammar: <_>::default(),
      visitor_aspect,
    }
  }

  pub fn build(&self, seed: ClosureSeed) -> Closure {
    if seed.is_empty() { return Closure::default(); }
    let grammar = self.grammar.borrow().clone();
    let profile = ClosureProfile::standard(seed.follow_calls);
    let mut state = ClosureBuilderState::new(grammar, profile, seed);
    state.build(&*self.visitor_aspect);
    state.complete()
  }

  pub fn set_grammar(&self, new_grammar: Option<Rc<Grammar>>) {
    let mut grammar = self.grammar.borrow_mut();
    *grammar = new_grammar;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ClosureBuilderState {
  pub queue: Vec<NodeId>,
  pub insert_index: usize,

  pub grammar: Option<Rc<Grammar>>,
  pub profile: ClosureProfile,
  pub visit_calls: bool,

  pub closure: Vec<NodeId>, // index_map
}

impl ClosureBuilderState {
  pub fn new(
    grammar: Option<Rc<Grammar>>, 
    profile: ClosureProfile,
    seed: ClosureSeed,
  ) -> Self {
    Self { 
      queue: seed.entries,
      insert_index: 0,

      grammar,
      profile,
      visit_calls: seed.follow_calls,

      closure: Vec::new(),
    }
  }

  pub fn build(&mut self, aspect: &VisitorAspect) {
    let mut index = 0;
    while index < self.queue.len() {
      let node_id = self.queue[index];
      self.insert_index = index + 1;
      let _ = aspect.visit_full::<BuildClosure, _>(node_id, &mut (), self);
      index += 1;
    }
  }

  pub fn complete(self) -> Closure {
    Closure::new(self.closure)
  }

  pub fn grammar(&self) -> &Grammar {
    match &self.grammar {
      Some(grammar) => &**grammar,
      None => panic!("no grammar"),
    }
  }

  pub fn grammar_cell(&self) -> Rc<Grammar> {
    match &self.grammar {
      Some(grammar) => grammar.clone(),
      None => panic!("no grammar"),
    }
  }

  pub fn visit<I: ToNodeId>(&mut self, flags: ClosureFlags, id: I) {
    let id = id.to_top();

    if (self.profile.add_mask & flags).is_non_zero() {
      if !self.closure.contains(&id) {
        self.closure.push(id);
      }
    }

    if (self.profile.queue_mask & flags).is_non_zero() {
      let pos = self.queue.iter().position(|e| e.eq(&id));

      match pos {
        Some(pos) if self.insert_index < pos => {
          self.queue.remove(pos);
          self.queue.insert(self.insert_index, id);
          self.insert_index += 1;
        }

        Some(_) => {}

        None => {
          self.queue.insert(self.insert_index, id);
          self.insert_index += 1;
        }
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub struct ClosureProfile {
  pub add_mask: ClosureFlags,
  pub queue_mask: ClosureFlags,
}

impl ClosureProfile {
  pub fn reduce(follow_calls: bool) -> Self {
    let mut queue_mask = CF::ASYNC_SUCC | CF::BLOCK | CF::BLOCK_FIRST;

    if follow_calls {
      queue_mask |= CF::CALL_TARGET;
    }

    Self { 
      add_mask: CF::REDUCE, 
      queue_mask 
    }
  }

  pub fn standard(follow_calls: bool) -> Self {
    let mut add_mask = CF::RELEVANT;
    let mut queue_mask = CF::ASYNC_SUCC | CF::BLOCK | CF::BLOCK_FIRST;

    if follow_calls {
      queue_mask |= CF::CALL_TARGET;
    } else {
      add_mask |= CF::CALL;
    }

    Self { add_mask, queue_mask }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
