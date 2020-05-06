use {
  crate::{
    alloc::{ABox, Fridge, ScopedAlloc},
    collections::linked_list,
    mir::ReduceKind,
    optimizer::DFA,
    runtime::{
      grammar::CtorDescr,
    }
  },
  north_core::{
    flame,
    log::{trace},
    quote::{
      QuoteType, RustQuoter,
      rust_type::{Type},
    },
  },
  north_derive::reflect_fn,
  std::{
    alloc::{AllocInit, AllocRef, Global, Layout},
    cell::{RefCell},
    cmp::max,
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod chart;
pub mod cursor;
pub mod grammar;
pub mod jit_resolver;
pub mod match_map;
pub mod options;
pub mod parser_ctx;
pub mod spec;
pub mod task;
pub mod trace;
pub mod trace_entry;

pub use self::{
  chart::{Chart},
  cursor::Cursor,
  grammar::{Grammar, GrammarVec},
  jit_resolver::{JITResolver},
  match_map::{MatchMap},
  options::{Options},
  parser_ctx::{ParserCtx},
  spec::*,
  task::{SuspendedTask, Task, TaskData},
  trace::{TraceId},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type LinkedList<T> = linked_list::LinkedList<T, ParserAlloc>;
pub type ParserAlloc = ScopedAlloc<Fridge<Global>>;
pub type ResumeEntry = (u32, *const u8);
pub type TaskBox = ABox<dyn Task, ParserAlloc>;
pub type TaskCtorFn = extern fn (PosId, *const ()) -> TaskBox;
pub type TaskVec = Vec<TaskBox>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use super::{
    spec::*,
    trace::{TraceId},
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TaskCtx {
  pub alloc: Rc<RefCell<Fridge<Global>>>,
  pub chart: Chart,
  pub context: Rc<ParserCtx>,
  pub input: Cursor,
  pub grammar_vec: GrammarVec,
  pub options: Options,
  pub resolver: JITResolver,
  pub max_pos: PosId,
}

impl TaskCtx {
  pub fn new(context: Rc<ParserCtx>, resolver: JITResolver, options: Options) -> Self {
    let grammar_vec = GrammarVec::new(context.clone());

    Self {
      alloc: <_>::default(),
      chart: Chart::new(options.clone()),
      context,
      input: Cursor::empty(),
      grammar_vec,
      options,
      resolver,
      max_pos: PosId::from_id(0u32),
    }
  }

  #[reflect_fn]
  pub extern fn add_reduce(
    &mut self, 
    parent      : &'static mut dyn Task, 
    reduce_kind : ReduceKind, 
    reduce_id   : ReduceId,
  ) -> bool {
    let data = parent.data();
    self.max_pos = max(self.max_pos, data.position);

    if self.options.show_reductions {
      let cursor = self.cursor_for(parent);
      println!("REDUCE org={:?}/{:?} {:?} {:?}: {:?}", 
        data.origin, 
        data.position,
        reduce_kind,
        reduce_id, 
        cursor.to_slice()
      );
    }

    let reduce_spec = ReduceSpec { reduce_kind, reduce_id };
    self.chart.add_reduction(
      &mut self.grammar_vec, parent, reduce_spec
    )
  }

  #[reflect_fn]
  pub extern fn add_reduce_short(
    &mut self, 
    parent      : &'static mut dyn Task, 
    reduce_kind : ReduceKind, 
    reduce_id   : ReduceId,
  ) -> bool {
    let data = parent.data();
    self.max_pos = max(self.max_pos, data.position);

    if self.options.show_reductions {
      let cursor = self.cursor_for(parent);
      println!("REDUCE_S org={:?}/{:?} {:?} {:?}: {:?}", 
        data.origin, 
        data.position,
        reduce_kind,
        reduce_id, 
        cursor.to_slice()
      );
    }

    let reduce_spec = ReduceSpec { reduce_kind, reduce_id };
    self.chart.add_reduction_short(
      parent, reduce_spec
    )
  }

  #[reflect_fn]
  pub extern fn add_suspend(
    &mut self,
    mut task: TaskBox, 
    pos_spec_id: MatchSpecId,
    neg_spec: MatchSpec,
  ) {
    let susp_task = SuspendedTask { 
      task,
      resumes: <_>::default(),
      pos_spec_id, 
      neg_spec 
    };

    // let key = ChartKey { pos: susp_task.position() };
    // let entry = self.chart.entries.entry(key);
    // let index = entry.index();
    // let org = susp_task.task.data().origin_idx;

    // println!("SUSPEND {:?}:{:?}", pos_spec_id, neg_spec);

    self.chart.add_suspended(
      &mut self.grammar_vec,
      susp_task,
    );
    
    // let susp_spec = pos_spec.iter().map(|e| ReduceId(e.reduce_id)).collect::<Vec<_>>();
    
  }

  #[reflect_fn]
  pub extern fn add_task(&mut self, task: TaskBox) {
    self.chart.add_task(task);
  }

  #[reflect_fn]
  pub extern fn add_task_new(&mut self, _task: TaskBox) {
    unimplemented!()
  }

  #[reflect_fn]
  pub extern fn alloc(size: usize, align: usize) -> *mut u8 {
    let mut alloc = ParserAlloc::default();
    unsafe {
      let layout = Layout::from_size_align_unchecked(size, align);
      let padding = layout.padding_needed_for(8);
      let layout = Layout::from_size_align_unchecked(size + padding, 8);
      let init = AllocInit::Uninitialized;
      let block = alloc.alloc(layout, init).unwrap();
      block.ptr.as_ptr()
    }
  }

  #[reflect_fn]
  pub extern fn call_rule_dyn(
    &mut self, 
    callee: &TaskData, 
    call_spec_id: CallSpecId, 
    args: *const ()
  ) {
    // let call_spec_2 = call_spec.iter().map(|(r, p)| (ReduceId(*r), p)).collect::<Vec<_>>();
    // println!("CALL DYN {:?}", call_spec_2);

    let position = callee.position;
    let grammar_id = callee.grammar_id;
    let descr = self.ctor_descr(grammar_id, call_spec_id);
    let grammar = self.grammar_vec.get_mut(grammar_id);
    
    self.chart.add_task_new(
      grammar, descr, position, args
    );
  }

  #[reflect_fn]
  pub extern fn complete_task(&mut self, _task: TaskBox) {
    //println!("COMPLETE TASK YAYS state={:?}", task.data().state);
  }

  pub fn ctor_descr(&mut self, grammar_id: GrammarId, call_spec_id: CallSpecId) -> CtorDescr {
    let grammar = self.grammar_vec.get(grammar_id);
    match grammar.lookup_ctor(call_spec_id) {
      Some(descr) => { descr },
      None => {
        let grammar = self.grammar_vec.get_rc(grammar_id);
        let ctor_fn = self.resolver.resolve(grammar, call_spec_id);

        let grammar = self.grammar_vec.get_mut(grammar_id);
        grammar.add_ctor(call_spec_id, ctor_fn)
      }
    }
  }

  pub fn cursor_for(&self, task: &dyn Task) -> Cursor {
    let data = task.data();
    let start_pos = self.chart.lookup_origin_position(task);
    let end_pos = data.position;

    Cursor {
      start: self.input.start.wrapping_add(start_pos.into()),
      end: self.input.start.wrapping_add(end_pos.into()),
    }
  }

  pub fn dump_info(&self) {
    self.alloc.borrow().dump_info();
    self.chart.dump_info();
    self.context.dump_info();
    self.grammar_vec.dump_info();
    
    println!("MAX_POS: {:?}", self.max_pos);
  }

  pub fn dump_parse_tree(&self) {
    self.chart.dump_parse_tree();
  }

  pub fn exec(&mut self) {
    let _guard = flame::start_guard("exec");

    ScopedAlloc::with_cell(self.alloc.clone(), || {
      while let Some(task) = self.chart.pop_task(&self.grammar_vec) {
        let data = task.data();
        // println!("E {:?}", data.state);
        let task = unsafe { ABox::into_box(task) };
        let _result = task.resume(self);
      }
    });
  }

  #[reflect_fn]
  pub extern fn exec_dfa(&mut self, task: &'static mut dyn Task, dfa: &'static DFA) -> u32 {
    let data = task.data_mut();
    let result = dfa.exec(self.input, &mut data.position);
    result
  }

  #[reflect_fn]
  pub extern fn fork_task(&mut self, task: &'static dyn Task, state: StateId) {
    let new_task = task.clone_direct(state);
    self.chart.add_task(new_task);
  }

  pub fn start_rule(&mut self, grammar_id: GrammarId, match_id: MatchId, cursor: Cursor) {
    trace!("starting rule {:?}:{:?}", grammar_id, match_id);

    let call_spec = vec![CallElem { match_id, min_prec: 0 }];

    self.input = cursor;

    ScopedAlloc::with_cell(self.alloc.clone(), || {
      let call_spec_id = self.context.call_spec_interner.intern(call_spec);
      let descr = self.ctor_descr(grammar_id, call_spec_id);
      let grammar = self.grammar_vec.get_mut(grammar_id);
      let pos_id = PosId::from_id(0u32);
      self.chart.add_task_new(grammar, descr, pos_id, &());
    });
  }
}

impl Drop for TaskCtx {
  fn drop(&mut self) {
    self.chart.clear();
    self.grammar_vec.clear();
  }
}

impl QuoteType for TaskCtx {
  fn quote(_q: &mut RustQuoter) -> Type {
    Type::Unit
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
