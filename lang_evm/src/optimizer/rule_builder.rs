use {
  crate::{
    mir::{self, BlockRef, LValLink},
    mir_ext::mark_persistent_recursive,
    optimizer::{
      BuildGroups,
      ClosureBuilder,
      GroupBuilder, GroupDyn, GroupSet, MatchMap,
      OptimizerAspect, SuccBuilder,
      closure::{Closure, ClosureId, ClosureSeed},
      closure_interner::{ClosureInterner},
      dfa_builder::{DFABuilder},
    },
    runtime::{
      Grammar, ReduceId,
      options::Options,
    },
  },
  lang_mir::Cursor,
  north_core::{
    compiler::Compiler,
    iter::ModelIterator,
    model::{Child, Link, ModelCell},
    node_id::{NodeId, ToNodeId},
    structure::ForestExt,
    visitor::{VisitorAspect, VisitCtxCore},
  },
  std::{
    any::Any,
    cell::RefCell,
    collections::{HashMap, VecDeque, hash_map::Entry::*},
    mem::replace,
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////
crate type GroupPair = (Box<dyn GroupDyn>, Box<dyn Any>);
////////////////////////////////////////////////////////////////////////////////////////////////

crate enum BuildJob {
  BuildOne {
    cursor: Cursor,
    closure: Closure,
    pair: GroupPair,
  },

  BuildSet {
    cursor: Cursor,
    closure: Closure,
  },
}

////////////////////////////////////////////////////////////////////////////////////////////////
// ClosureInterner
// Closure -> TransitionTable

pub struct RuleBuilder {
  crate _optimizer: Rc<OptimizerAspect>,
  crate visitor: Rc<VisitorAspect>,
  crate output_model: ModelCell,
  crate options: Options,

  crate closure_builder: Rc<ClosureBuilder>,
  crate closure_interner: Rc<ClosureInterner>,
  crate dfa_builder: DFABuilder,

  crate grammar: Option<Rc<Grammar>>,
  crate reduce_ids: Vec<ReduceId>,

  crate closure_map: HashMap<Closure, Option<BlockRef>>, // IndexMap 
  crate value_map: HashMap<NodeId, NodeId>,
  crate build_queue: VecDeque<BuildJob>,
  crate fail_block: Option<BlockRef>,

  crate num_built: usize,
  crate origin_matches: MatchMap,
  crate origin_stmts: Rc<RefCell<Vec<GroupPair>>>,
  
  crate rule_ty: Option<Link<mir::TypeFn>>,
  crate iter_locals: Vec<LValLink>,
  crate locals: Vec<Child<mir::RuleLocal>>,
  crate blocks: Vec<Child<mir::Block>>,
}

impl RuleBuilder {
  pub fn new(comp: &Compiler, options: Options) -> Self {
    let visitor = comp.aspect_mut::<VisitorAspect>();

    let closure_builder = Rc::new(ClosureBuilder::new(visitor.clone()));
    let closure_interner = ClosureInterner::new();
    let dfa_builder = DFABuilder::new(
      closure_builder.clone(), 
      closure_interner.clone(), 
      visitor.clone(),
    );

    Self {
      _optimizer: comp.aspect_mut::<OptimizerAspect>(),
      visitor: visitor.clone(),
      output_model: comp.model_cell.clone(),
      options,

      closure_builder,
      closure_interner,
      dfa_builder,

      grammar: None,
      reduce_ids: Vec::new(),

      closure_map: HashMap::new(),
      value_map: HashMap::new(),
      build_queue: VecDeque::new(),
      fail_block: None,

      num_built: 0,
      origin_matches: MatchMap::default(),
      origin_stmts: <_>::default(),

      rule_ty: None,
      iter_locals: Vec::new(),
      locals: Vec::new(),
      blocks: Vec::new(),
    }
  }

  pub fn add_rules(&mut self, rules: &Vec<NodeId>) {
    let seed = {
      let model = self.visitor.input_model.borrow();

      let first_rule = model.get::<_, mir::ItemRule>(rules[0]).unwrap();
      self.rule_ty = Some(first_rule.rule_ty.clone());

      model.iter(rules)
        .borrow_cast_nodes_to::<mir::ItemRule>()
        .filter_map(|r| r.blocks.first().cloned())
        .map(|b| model.node(b).first_node())
        .collect::<Vec<_>>()
    };

    let mut seed = ClosureSeed::new(seed);
    seed.follow_calls = true;
    let _main_block = self.resolve_closure_seed(seed);

    let fail_seed = ClosureSeed::default();
    let fail_block = self.resolve_closure_seed(fail_seed);
    self.fail_block = Some(fail_block);
  }

  pub fn add_value_map<A: ToNodeId, B: ToNodeId>(&mut self, from: A, to: B) {
    self.value_map.insert(from.to_top(), to.to_top());
  }

  pub fn at_origin(&self) -> bool {
    self.num_built <= 2
  }

  pub fn build(
    &mut self, grammar: Rc<Grammar>, rules: &Vec<NodeId>, reduce_ids: Vec<ReduceId>,
  ) -> (String, NodeId<mir::ItemRule>) 
  {
    assert!(rules.len() >= 1);

    self.closure_builder.set_grammar(Some(grammar.clone()));
    self.grammar = Some(grammar);
    self.reduce_ids = reduce_ids;
    self.add_rules(rules);

    while let Some(job) = self.build_queue.pop_front() {
      match job {
        BuildJob::BuildOne { cursor, closure, pair } => {
          // println!("BUILD_ONE {:?} => {:?}", closure, cursor.orig_block_id());
          self.build_one(cursor, closure, pair);
        },
        BuildJob::BuildSet { cursor, closure } => {
          // println!("BUILD_CLOSURE {:?} => {:?}", closure, cursor.orig_block_id());
          self.build_set(cursor, closure);
          self.num_built += 1;
        },
      }
    }

    // self.dump_info();
    self.closure_builder.set_grammar(None);
    self.closure_map.clear();
    self.grammar = None;
    self.iter_locals.clear();
    self.num_built = 0;
    self.origin_matches.clear();
    self.origin_stmts.borrow_mut().clear();
    self.value_map.clear();

    let mut model = self.output_model.borrow_mut();

    let node_id = model.new_node();
    let name = format!("opt_{}", node_id.idx());

    //println!("BUILD LOCALS {:?}", self.locals.len());

    model.complete_node(node_id, mir::ItemRule {
      name: name.clone(),
      rule_ty: self.rule_ty.take().unwrap(),
      params: Vec::new(),
      locals: replace(&mut self.locals, Vec::new()),
      blocks: replace(&mut self.blocks, Vec::new()),
    });

    drop(model);

    // Mark persistent blocks
    mark_persistent_recursive(&*self.visitor, node_id);

    (name, node_id)
  }

  pub fn build_one(&mut self, mut cursor: Cursor, closure: Closure, pair: GroupPair) {
    let (group_k, group_v) = pair;
    let succ_seed = self.successors(closure);
    let succ_closure = self.closure(succ_seed);

    group_k.build_mir_dyn(&*group_v, self, &mut cursor);

    match (cursor.is_completed(), succ_closure.is_empty()) {
      (true, true) => { },
      (true, false) => { panic!("completed group with successors"); },
      (false, true) => { self.fuse_block(cursor); },
      (false, false) => {
        let job = BuildJob::BuildSet { cursor, closure: succ_closure };
        self.build_queue.push_back(job);
      },
    }
  }

  crate fn build_origin_stmts(&mut self, cursor: &mut Cursor) {
    let stmts = self.origin_stmts.clone();
    let stmts = stmts.borrow();

    for (k, v) in stmts.iter() {
      k.build_mir_dyn(&**v, self, cursor);
    }
  }

  pub fn build_set(&mut self, mut cursor: Cursor, closure: Closure) {
    if closure.is_empty() {
      cursor.build_ctl(mir::CtlFail { });
      return;
    }

    let group_set = self.group(&closure);
    group_set.build_mir(self, cursor);
  }

  pub fn closure(&self, seed: ClosureSeed) -> Closure {
    self.closure_builder.build(seed)
  }

  pub fn dump_info(&self) {
    let mut map = HashMap::new();

    for (closure, _) in &self.closure_map {
      let mut closure_ids = closure.entries.clone();
      closure_ids.sort();
      map.insert(closure_ids, ());
    }

    println!("CLOSURES {:?} vs {:?}", map.len(), self.closure_map.len());
  }

  pub fn fail_block(&self) -> BlockRef {
    self.fail_block.clone().unwrap()
  }

  pub fn fuse_block(&self, mut cur: Cursor) {
    if !cur.is_completed() {
      let fail_block = self.fail_block();
      cur.build_ctl(mir::CtlBr { block: fail_block });
    }
  }

  pub fn group(&mut self, closure: &Closure) -> GroupSet {
    let mut group_buider = GroupBuilder::new();
    let visitor = self.visitor.clone();

    for entry in &closure.entries {
      let node_id = *entry;
      let core = VisitCtxCore::<BuildGroups> { 
        aspect: &*visitor, node_id, imp_args: self
      };
      let _ = visitor.visit(core, &mut group_buider);
    }
    
    group_buider.complete()
  }

  pub fn intern_closure(&self, closure: Closure) -> ClosureId {
    self.closure_interner.intern(closure)
  }

  pub fn is_origin_pure(&self) -> bool {
    let incorporate_reductions = self.options.incorporate_reductions;
    let is_pure = self.origin_stmts.borrow().is_empty();
    incorporate_reductions && is_pure
  }

  pub fn is_reduce_external(&self, reduce_id: ReduceId) -> bool {
    self.reduce_ids.contains(&reduce_id)
  }

  pub fn map_local(&mut self, src: NodeId<mir::RuleLocal>) -> NodeId<mir::RuleLocal> {
    let ty = {
      let model = self.visitor.input_model.borrow();
      let node = model.node(src);
      node.ty.clone() // XXX: BAD: type needs to be deep copied/transfered
    };

    let mut model_out = self.output_model.borrow_mut();
    let local_out = model_out.build_node(mir::RuleLocal {
      ty, index: self.locals.len()
    });

    self.locals.push(local_out.into());
    local_out
  }

  pub fn map_local_iter(&mut self, src: &LValLink, depth: usize) -> LValLink {
    if let Some(local) = self.iter_locals.get(depth).cloned() {
      self.add_value_map(src, &local);
      return local;
    } 
    
    if depth != self.iter_locals.len() {
      panic!("invalid depth");
    }

    let local = self.map_lval(src);
    self.iter_locals.push(local.clone());

    local
  }

  pub fn map_lval(&mut self, src: &LValLink) -> LValLink {
    let node_id = src.to_top();
    if let Some(result) = self.value_map.get(&node_id) {
      return result.cast().into();
    }

    let new_local = self.map_local(node_id.cast());
    self.value_map.insert(node_id, new_local.to_top());
    new_local.cast().into()
  }

  pub fn map_lval_lookup(&mut self, src: &LValLink) -> LValLink {
    let node_id = src.to_top();
    match self.value_map.get(&node_id) {
      Some(result) => result.cast().into(),
      None => panic!("unknown value: {:?}", node_id),
    }
  }

  crate fn origin_matches(&self, reduce_id: ReduceId) -> ClosureSeed {
    let mut result = ClosureSeed::default();

    if let Some(grammar) = &self.grammar {
      let match_map = &grammar.match_map;
      for (match_id, prec_map) in &self.origin_matches {
        let prec = match match_map.get(*match_id, reduce_id) {
          Some(prec) => prec,
          None => continue,
        };

        if let Some(closure) = prec_map.get(&prec) {
          result.merge(closure);
        }
      }
    }

    result
  }

  crate fn queue_build_one(&mut self, cursor: Cursor, closure: Closure, pair: GroupPair) {
    let job = BuildJob::BuildOne { cursor, closure, pair };
    self.build_queue.push_back(job);
  }

  crate fn resolve<F>(&mut self, closure: Closure, job_ctor: F) -> BlockRef where
    F: FnOnce(Cursor, Closure) -> BuildJob
  {
    match self.closure_map.entry(closure) {
      Occupied(occupied) => {
        occupied.get().clone().unwrap()
      }
      Vacant(vacant) => {
        let mut model = self.output_model.borrow_mut();
        let block_id = model.new_node::<mir::Block>();
        self.blocks.push(block_id.into());

        let cursor = Cursor::new(self.output_model.clone(), block_id);
        let job = job_ctor(cursor, vacant.key().clone());
        self.build_queue.push_back(job);

        vacant.insert(Some(block_id.into()));
        block_id.into()
      }
    }
  }

  crate fn resolve_closure(&mut self, closure: Closure) -> BlockRef {
    self.resolve(closure, |cursor, closure| {
      BuildJob::BuildSet { cursor, closure }
    })
  }

  crate fn resolve_closure_seed(&mut self, seed: ClosureSeed) -> BlockRef {
    let closure = self.closure(seed);
    self.resolve_closure(closure)
  }

  crate fn resolve_one(&mut self, seed: ClosureSeed, pair: GroupPair) -> BlockRef {
    let closure = seed.into_closure();
    self.resolve(closure, |cursor, closure| {
      BuildJob::BuildOne { cursor, closure, pair }
    })
  }

  crate fn successors(&self, seed: Closure) -> ClosureSeed {
    let mut builder = SuccBuilder::new(seed.entries);
    builder.build(&*self.visitor);
    builder.complete()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
