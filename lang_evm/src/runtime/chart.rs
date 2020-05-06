use {
  crate::{
    collections::{
      Histogram, IndexMap, PointerTrie, Slot,
      block_vec, linked_list::RotateResult::*,
    },
    mir::ReduceKind,
    runtime::{
      Grammar, GrammarVec, 
      LinkedList, MatchElem, MatchMap, 
      ParserCtx, SuspendedTask, 
      Task, TaskBox, TaskVec,
      grammar::CtorDescr,
      prelude::*,
      options::{Options},
      task::{TaskData},
      trace::{TraceDecoder, TraceStore},
    },
  },
  std::{
    cell::RefCell,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Eq, PartialEq)]
crate enum MatchType {
  Exact(TraceId),
  Shadow,
}

////////////////////////////////////////////////////////////////////////////////////////////////

thread_local! {
  static NUM_SUSP: RefCell<usize> = RefCell::new(0);
}

////////////////////////////////////////////////////////////////////////////////////////////////
pub type ChartEntries = PointerTrie<PosId, ChartEntry>;
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Chart {
  crate entries: ChartMap,
  crate complete_queue: TaskVec,
  crate exec_stack: Vec<ChartId>,
  crate generation: u32,
  crate num_calls_dup: usize,
  crate num_calls_total: usize,
  crate num_incomplete: usize,
  crate num_incomplete_reds: usize,
  crate num_reduce: usize,
  crate num_reduce_ok: usize,
  crate resume_queue: Vec<TaskBox>,
  crate options: Options,
  crate tracer: TraceStore,
  // crate reduce_hist: Histogram<ReduceId>,
}

impl Chart {
  crate fn new(options: Options) -> Self {
    Self { 
      entries: <_>::default(),
      complete_queue: Vec::new(),
      exec_stack: Vec::new(),
      generation: 1,
      num_calls_dup: 0,
      num_calls_total: 0,
      num_incomplete: 0,
      num_incomplete_reds: 0,
      num_reduce: 0,
      num_reduce_ok: 0,
      options,
      resume_queue: Vec::new(),
      tracer: <_>::default(),
      // reduce_hist: <_>::default(),
    }
  }

  crate fn add_reduction(
    &mut self, grammars: &mut GrammarVec, parent: &mut dyn Task, reduce_spec: ReduceSpec, 
  ) -> bool {
    self.num_reduce += 1;

    let entry = self.entries.origin_entry_mut(parent);
    let data = parent.data_mut();

    let reduction = Self::begin_reduce(
      &mut self.tracer, data, entry, &reduce_spec
    );

    let match_type = entry.contains_reduction(&reduction);
    match match_type {
      Some(MatchType::Exact(old_trace)) => {
        self.tracer.merge_reduce(old_trace, reduction.trace_id);
        data.trace = old_trace;
      }

      Some(MatchType::Shadow) => { }

      None => {
        self.num_reduce_ok += 1;

        for susp_task in &mut entry.suspended_tasks {
          ChartEntry::resume_task(
            grammars, 
            &mut self.tracer,
            &reduction, 
            susp_task, 
            &mut self.resume_queue
          );
        }

        entry.push_reduction(reduction);
        self.resume_tasks();
      }
    }

    let reduce_continue = match_type != Some(MatchType::Shadow);
    reduce_continue
  }

  crate fn add_reduction_short(
    &mut self, parent: &mut dyn Task, reduce_spec: ReduceSpec
  ) -> bool {
    self.num_reduce += 1;

    let entry = self.entries.origin_entry_mut(parent);
    let data = parent.data_mut();

    let reduction = Self::begin_reduce(
      &mut self.tracer, data, entry, &reduce_spec
    );

    let match_type = entry.contains_reduction(&reduction);
    match match_type {
      Some(MatchType::Exact(old_trace)) => {
        self.tracer.merge_reduce(old_trace, reduction.trace_id);
        data.trace = old_trace;
        true
      }

      Some(MatchType::Shadow) => { false }

      None => { 
        entry.push_reduction(reduction);
        true
      }
    }
  }

  crate fn add_suspended(
    &mut self, 
    grammars: &mut GrammarVec, 
    mut susp_task: SuspendedTask,
  ) {
    let entry = self.entries.suspend_entry_mut(&*susp_task.task);

    for reduction in &entry.reductions {
      ChartEntry::resume_task(
        grammars, 
        &mut self.tracer,
        reduction, 
        &mut susp_task, 
        &mut self.resume_queue
      );
    }

    entry.push_susp_task(susp_task);

    self.resume_tasks();
  }

  crate fn add_task(&mut self, task: TaskBox) {
    let origin = task.data().origin;
    let entry = self.entries.entry_by_id_mut(origin);

    entry.add_task(task);

    if !entry.queued {
      entry.queued = true;
      self.exec_stack.push(origin);
    }

    if entry.completed_gen != 0 { 
      entry.completed_gen = 0;
      entry.num_wake += 1;
      self.num_incomplete += 1;
    }
  }

  crate fn add_task_new(
    &mut self, 
    grammar: &mut Grammar,
    ctor_descr: CtorDescr,
    position: PosId,
    args: *const (),
  ) {
    let (index, entry) = self.entries.entry_by_pos_new(position);

    let mut mask = ctor_descr.mask;
    for susp_task in &entry.suspended_tasks {
      let ms_id = susp_task.pos_spec_id;
      let curr_mask = grammar.match_spec_mask(ms_id);
      mask.subtract(curr_mask);
    }

    self.num_calls_total += 1;
    if mask.is_zero() {
      self.num_calls_dup += 1;
      return;
    }

    let mut task = (ctor_descr.ctor)(position, args);
    let data = task.data_mut();
    if data.origin.idx() == std::u32::MAX {
      data.origin = index;
    } else {
      panic!("fresh task contains invalid origin={:?}", data.origin);
    }

    entry.add_task(task);

    if !entry.queued {
      entry.queued = true;
      self.exec_stack.push(index);
    }

    if entry.completed_gen != 0 { 
      // println!("W {:?}", entry.position.idx());
      entry.completed_gen = 0;
      entry.num_wake += 1;
      self.num_incomplete += 1;
    }
  }

  #[inline(always)]
  crate fn begin_reduce(
    tracer      : &mut TraceStore,
    data        : &mut TaskData, 
    entry       : &mut ChartEntry,
    reduce_spec : &ReduceSpec,
  ) -> Reduction {
    let trace_id = tracer.add_reduce(
      &mut data.trace, 
      reduce_spec.reduce_id, 
      (entry.position, data.position),
    );

    Reduction { 
      kind: reduce_spec.reduce_kind,
      reduce_id: reduce_spec.reduce_id, 
      size: PosId::size(entry.position, data.position), 
      trace_id
    }
  }

  pub fn clear(&mut self) {
    self.entries.clear();
    self.complete_queue.clear();
    self.exec_stack.clear();
    self.resume_queue.clear();
  }

  crate fn dump_info(&self) {
    let mut max_reductions = 0;
    let mut num_reductions = 0;
    let mut num_reductions_4plus = 0;
    let mut num_suspended = 0;
    let mut num_suspended_max = 0;
    let mut num_suspended_4plus = 0;

    let mut reduce_hist = Histogram::new();
    let mut resume_hist = Histogram::new();
    let mut susp_hist = Histogram::new();
    let mut wake_hist = Histogram::new();

    let entry_size = std::mem::size_of::<ChartEntry>();

    for entry in self.entries.iter() {
      let pos = entry.position;

      wake_hist.insert(entry.num_wake as usize);

      let curr_num_reds = entry.reductions.len();
      num_reductions += curr_num_reds;
      max_reductions = std::cmp::max(max_reductions, curr_num_reds);
      reduce_hist.insert(curr_num_reds);

      if curr_num_reds >= 4 {
        num_reductions_4plus += 1;
      }

      for susp in &entry.suspended_tasks {
        resume_hist.insert(susp.resumes.len());
      }

      let curr_num_suspended = entry.suspended_tasks.len();
      num_suspended += curr_num_suspended;
      num_suspended_max = std::cmp::max(num_suspended_max, curr_num_suspended);
      susp_hist.insert(curr_num_suspended);

      if curr_num_suspended >= 4 {
        num_suspended_4plus += 1;
      }

      if curr_num_suspended == 14 {
        println!("SUSP_HIGH num={} offset={:?}:", 
          curr_num_suspended, pos,
        );

        for (i, susp_task) in entry.suspended_tasks.iter().enumerate() {
          let parser_ctx = ParserCtx::get();
          let spec_id = susp_task.pos_spec_id;
          let pos_spec = parser_ctx.get_match_spec(spec_id);
          let match_ids = pos_spec.iter().map(|e| e.match_id).collect::<Vec<_>>();
          println!("  {:2}: {:5} {:?}", i, spec_id.idx(), match_ids);
        }
      }
    }

    self.entries.dump_info();
    self.tracer.dump_info();

    println!("CHART:");
    println!("  total entries: {} * {} = {}", 
      self.entries.len(), entry_size, self.entries.len() * entry_size);
    println!("  num_calls: {} (dup={})", self.num_calls_total, self.num_calls_dup);
    //println!("  max_suspended: {}", MAX_SUSPENDED.with(|cell| cell.get()));
    println!("  num_reductions: calls={}, ok={} (max={}, 4plus={})", 
      self.num_reduce, 
      self.num_reduce_ok,
      max_reductions, 
      num_reductions_4plus
    );
    println!("  num_incomplete: {}", self.num_incomplete);
    println!("  num_incomplete_reds: {}", self.num_incomplete_reds);
    println!("  num_suspended: {} (max={}, 4plus={})", 
      num_suspended, num_suspended_max, num_suspended_4plus);

    println!("  idx_map {:?}", std::mem::size_of::<IndexMap<u64, u64>>());

    println!("REDUCE_HIST:");
    println!("{:?}", reduce_hist);

    println!("RESUME_HIST:");
    println!("{:?}", resume_hist);

    println!("SUSP_HIST:");
    println!("{:?}", susp_hist);

    // println!("REDUCE_TYPE_HIST:");
    // println!("{:?}", self.reduce_hist);
  }

  crate fn dump_parse_tree(&self) {
    match self.main_trace() {
      Some(main_trace) => {
        let mut decoder = TraceDecoder::new(&self.tracer);
        decoder.dump_reduce(0, main_trace);
      }
      None => {
        println!("parse error");
      }
    }
  }

  crate fn lookup_origin_position<'c>(&self, task: &dyn Task) -> PosId {
    self.entries.origin_entry(task).position
  }

  crate fn main_trace(&self) -> Option<TraceId> {
    let entry_0 = self.entries.first().unwrap();
    let main_red = entry_0.reductions.iter().max_by_key(|r| r.size);
    main_red.map(|r| r.trace_id)
  }

  crate fn pop_task(&mut self, grammars: &GrammarVec) -> Option<TaskBox> {
    while let Some(index) = self.exec_stack.last() {
      // Try to pop a running task
      let index = *index;
      let entry = self.entries.entry_by_id_mut(index);
      let task = entry.pop_task();
      if task.is_some() { return task; }

      // Attempt to fail a task within the entry
      let task = entry.fail_task(grammars);
      if task.is_some() { return task; }

      // Finally, fail the whole entry
      let _ = self.exec_stack.pop();
      entry.completed_gen = self.generation;
      entry.queued = false;

      self.generation += 1;
      if self.options.enable_gc && ((self.generation & 0xFF) == 0) {
        self.run_gc();
      }
    }

    None
  }

  crate fn resume_tasks(&mut self) {
    for new_task in self.resume_queue.drain(..).rev() {
      // println!("RESUME {:?}", new_task.data().state);
      let entry = self.entries.origin_entry_mut(&*new_task);
      entry.add_task(new_task);
    }
  }

  #[inline(never)]
  crate fn run_gc(&mut self) {
    let last_entry = match self.exec_stack.last() {
      Some(last_index) => self.entries.entry_by_id(*last_index),
      None => return,
    };

    let last_pos = last_entry.position;
    self.entries.release_entries(|entry| {
      entry.completed_gen != 0 && entry.position.idx() < last_pos.idx()
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct ChartEntry {
  crate position: PosId,
  crate reductions: LinkedList<Reduction>,
  crate running_tasks: LinkedList<TaskBox>,
  crate suspended_tasks: LinkedList<SuspendedTask>,
  crate completed_gen: u32,
  crate num_wake: u32,
  crate queued: bool,
}

impl ChartEntry {
  crate fn new(position: PosId) -> Self {
    Self {
      position,
      reductions: <_>::default(),
      running_tasks: <_>::default(),
      suspended_tasks: <_>::default(),
      completed_gen: 0,
      num_wake: 0,
      queued: false,
    }
  }

  crate fn add_task(&mut self, task: TaskBox) {
    self.running_tasks.push_front(task);
  }

  crate fn clear(&mut self) {
    self.reductions.clear();
    self.running_tasks.clear();
    self.suspended_tasks.clear();
  }

  crate fn contains_reduction(&self, reduction: &Reduction) -> Option<MatchType> {
    //self.reductions.iter().any(|r| r.matches(reduction))
    self.reductions.iter().find_map(|r| r.matches_2(self.position, reduction))
  }

  crate fn fail_task(
    &mut self, 
    grammars: &GrammarVec, 
  ) -> Option<TaskBox> {
    let reductions = &self.reductions;
    self.suspended_tasks.rotate(move |task| {
      if !task.has_negative_matches() {
        return (Keep, None);
      }

      let grammar_id = task.grammar_id();
      let match_map = grammars.match_map(grammar_id);

      while let Some(e) = task.pop_neg_spec() {
        if Self::test_match(reductions, match_map, e) { continue }
        //println!("FAIL_SPEC {:?} => {:?}", ReduceId(e.reduce_id), e);
        let new_task = task.task.clone(e.state_id);
        return (Keep, Some(new_task));
      }

      if task.has_positive_matches() {
        (Rotate, None)
      } else {
        (Consume, None)
      }
    })
  }

  crate fn pop_task(&mut self) -> Option<TaskBox> {
    self.running_tasks.pop_front()
  }

  crate fn push_reduction(&mut self, reduction: Reduction) {
    self.reductions.push_front(reduction);
  }

  crate fn push_susp_task(&mut self, susp_task: SuspendedTask) {
    // this may be wrong: suspended tasks may require strict ordering
    // to resume them correctly
    if susp_task.has_negative_matches() {
      self.suspended_tasks.push_front(susp_task);
    } else {
      self.suspended_tasks.push_back(susp_task);  
    }
  }

  crate fn reset(&mut self, position: PosId) {
    self.completed_gen = 0;
    self.position = position;
  }

  crate fn resume_task(
    grammars: &mut GrammarVec, 
    tracer: &mut TraceStore,
    reduction: &Reduction,
    susp_task: &mut SuspendedTask, 
    resume_queue: &mut Vec<TaskBox>
  ) {
    if reduction.kind == ReduceKind::Reject { 
      return
    }

    let grammar_id = susp_task.grammar_id();
    let grammar = grammars.get_mut(grammar_id);

    for state_id in grammar.positive_matches(susp_task.pos_spec_id, reduction.reduce_id) {
      if let Some(new_task) = susp_task.resume(tracer, reduction, *state_id) {
        resume_queue.push(new_task);
      }
    }
  }

  crate fn test_match<'a, I: IntoIterator<Item=&'a Reduction>>(
    reductions: I, 
    match_map: &MatchMap, 
    match_elem: &MatchElem
  ) -> bool {
    let match_id = match_elem.match_id;
    let range = &match_elem.prec_range;
    reductions.into_iter()
      .filter(|r| match_map.matches(match_id, r.reduce_id, range.clone()))
      .next().is_some()
  }

  // crate fn wake(&mut self, curr_gen: u32) -> bool {
  //   if self.complete_gen != 0 {
  //     self.complete_gen = 0;
  //   }
  // }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Default)]
pub struct ChartMap {
  crate entries: ChartEntries,
  crate free_slots: Vec<ChartId>,
}

impl ChartMap {
  pub fn dump_info(&self) {
    self.entries.dump_info();
  }

  pub fn clear(&mut self) {
    self.entries.clear();
    self.free_slots.clear();
  }

  pub fn entry_by_id(&self, id: ChartId) -> &ChartEntry {
    self.entries.get_index(id.into()).unwrap()
  }

  pub fn entry_by_id_mut(&mut self, id: ChartId) -> &mut ChartEntry {
    self.entries.get_index_mut(id.into()).unwrap()
  }

  pub fn entry_by_pos(&mut self, pos: PosId) -> &mut ChartEntry {
    self.entries.get_mut(pos).unwrap()
  }

  pub fn entry_by_pos_new(&mut self, pos: PosId) -> (ChartId, &mut ChartEntry) {
    if let Some(index) = self.entries.index(pos) {
      let chart_id = ChartId::from(index);
      return (chart_id, self.entries.get_index_mut(index).unwrap());
    }

    if let Some(chart_id) = self.free_slots.pop() {
      let index = chart_id.idx() as usize;
      self.entries.bind_key(pos, index);

      let entry = self.entries.get_index_mut(index).unwrap();
      entry.reset(pos);

      return (chart_id, entry);
    }

    let (index, slot) = self.entries.slot(pos);
    match slot {
      Slot::Occupied(_) => unreachable!(),
      Slot::Vacant(vacant) => {
        let entry = ChartEntry::new(pos);
        let chart_id = ChartId::from(index);
        (chart_id, vacant.insert(entry))
      }
    }
  }

  pub fn first(&self) -> Option<&ChartEntry> {
    self.entries.get_index(0)
  }

  pub fn iter(&self) -> block_vec::Iter<ChartEntry> {
    (&self.entries).into_iter()
  }

  pub fn len(&self) -> usize {
    self.entries.len()
  }

  pub fn origin_entry(&self, task: &dyn Task) -> &ChartEntry {
    let index = task.data().origin;
    self.entry_by_id(index)
  }

  pub fn origin_entry_mut(&mut self, task: &dyn Task) -> &mut ChartEntry {
    let index = task.data().origin;
    self.entry_by_id_mut(index)
  }

  pub fn release_entries<F>(&mut self, pred: F) where
    F: Fn(&mut ChartEntry) -> bool
  {
    for index in 1..self.entries.len() {
      let entry = self.entries.get_index_mut(index).unwrap();
      if !entry.queued && pred(entry) {
        entry.clear();
        entry.completed_gen = 0;

        let chart_id = ChartId::from(index);
        let position = entry.position;
        self.entries.unbind_key(position);
        self.free_slots.push(chart_id);
      }
    }
  }

  pub fn release_index(&mut self, id: ChartId) {
    self.free_slots.push(id);
    self.entry_by_id_mut(id).clear();
  }

  pub fn suspend_entry_mut(&mut self, task: &dyn Task) -> &mut ChartEntry {
    let data = task.data();
    let self_ = self as *mut ChartMap;
    let origin_entry = self.entry_by_id_mut(data.origin);

    if origin_entry.position == data.position {
      origin_entry
    } else {
      let self_ = unsafe { &mut *self_ };
      self_.entry_by_pos_new(data.position).1
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Eq, PartialEq)]
pub struct Reduction {
  crate kind: ReduceKind,
  crate reduce_id: ReduceId,
  crate size: u32,
  crate trace_id: TraceId,
}

impl Reduction {
  crate fn matches(&self, new_red: &Reduction) -> bool {
    if self.reduce_id != new_red.reduce_id { return false }
    if self.kind < new_red.kind { return true }
    if self.kind > new_red.kind { println!("WARN: reduction slip"); }
    self.size == new_red.size
  }

  crate fn matches_2(&self, pos: PosId, new_red: &Reduction) -> Option<MatchType> {
    if self.reduce_id != new_red.reduce_id { return None }
    if self.kind < new_red.kind { return Some(MatchType::Shadow) }
    if self.kind > new_red.kind { println!("WARN: reduction slip at {:?}", pos); }
    if self.size == new_red.size { 
      Some(MatchType::Exact(self.trace_id)) 
    } else { 
      None 
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
