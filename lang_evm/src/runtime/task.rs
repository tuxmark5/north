use {
  crate::{
    alloc::{ABox, ReverseAlloc},
    collections::{Slot, SmallMap},
    runtime::{
      MatchElem, MatchSpec, MatchSpecId, ParserAlloc,
      StateId, TaskBox, TaskCtx,
      chart::Reduction, 
      prelude::*,
      trace::TraceStore,
    },
  },
  north_core::{
    quote::{
      QuoteType, RustQuoter,
      rust_type::{Type},
    },
  },
  std::{
    fmt::{self, Debug},
    mem::transmute_copy,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, PartialEq)]
pub struct Resume {
  crate state_id: StateId,
  crate size: u32,
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SuspendedTask {
  crate task: TaskBox,
  // crate resumes: LinkedList<Resume>, // SmallMap
  // crate resumes: LinkedMap<Resume, (), ParserAlloc>,
  crate resumes: SmallMap<Resume, TraceId, ParserAlloc>,
  crate pos_spec_id: MatchSpecId,
  crate neg_spec: MatchSpec,
}

impl SuspendedTask {
  pub fn has_negative_matches(&self) -> bool {
    !self.neg_spec.is_empty()
  }

  pub fn has_positive_matches(&self) -> bool {
    //!self.pos_spec.is_empty()
    true
  }

  pub fn grammar_id(&self) -> GrammarId {
    self.task.data().grammar_id
  }

  pub fn pop_neg_spec(&mut self) -> Option<&'static MatchElem> {
    let spec_len = self.neg_spec.len();
    if spec_len >= 1 {
      let tail = &self.neg_spec[spec_len - 1];
      self.neg_spec = &self.neg_spec[..spec_len - 1];
      Some(tail)
    } else {
      None
    }
  }

  pub fn position(&self) -> PosId {
    self.task.data().position
  }

  pub fn resume(
    &mut self, 
    tracer: &mut TraceStore, 
    reduction: &Reduction, 
    state_id: StateId
  ) -> Option<TaskBox> {
    let size = reduction.size;
    let resume = Resume { state_id, size };

    match self.resumes.slot(resume) {
      Slot::Occupied(occupied) => {
        let trace_id = occupied.into_value();
        tracer.merge_shift(*trace_id, reduction.trace_id);
        None
      }

      Slot::Vacant(vacant) => {
        let mut new_task = self.task.clone_direct(state_id);
        let data = new_task.data_mut();
        let trace_id = tracer.add_shift(data.trace, reduction.trace_id);
        data.position = data.position.advance(size);
        data.trace = trace_id;
        vacant.insert(trace_id);
        Some(new_task)
      }
    }
  }

  // pub fn resume(
  //   &mut self, 
  //   tracer: &mut TraceStore, 
  //   reduction: &Reduction, 
  //   state_id: StateId
  // ) -> Option<TaskBox> {
  //   let size = reduction.size;
  //   let resume = Resume { state_id, size };
  //   let trace_id = reduction.trace_id;
  //   let result = self.resumes.insert_fold(
  //     resume, trace_id, |a, b| tracer.merge_shift(a, b)
  //   );
  //   if result.is_some() { return None; }

  //   let mut new_task = self.task.clone_direct(state_id);
  //   let data = new_task.data_mut();
  //   data.position = data.position.advance(size);
  //   tracer.add_shift(&mut data.trace, trace_id);
  //   Some(new_task)
  // }
}

impl Debug for SuspendedTask {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("SuspendedTask")
      .field("pos_spec_id", &self.pos_spec_id)
      .field("neg_spec", &self.neg_spec)
      .finish()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Task {
  fn clone(&self, new_state: StateId) -> TaskBox;
  fn eq(&self, other: &dyn Task) -> bool;
  fn hash(&self, state: *mut u64);
  fn resume(self: Box<Self>, ctx: &mut TaskCtx) -> i32;
}

impl<'a> dyn 'a + Task {
  pub fn clone_direct(&self, state_id: StateId) -> TaskBox {
    let self_ = &self;
    let box_ptr = self_ as *const _ as *const TaskBox;
    let mut new_task = unsafe { (*box_ptr).clone_bits() };
    new_task.data_mut().state = state_id;
    new_task
  }

  pub fn data(&self) -> &TaskData {
    let self_ = self;
    unsafe { transmute_copy::<&dyn Task, &TaskData>(&self_) }
  }

  pub fn data_mut(&mut self) -> &mut TaskData {
    let mut self_ = self;
    unsafe { transmute_copy::<&mut dyn Task, &mut TaskData>(&mut self_) }
  }
}

impl Debug for dyn Task {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.data().fmt(f)
  }
}

impl QuoteType for &'static dyn Task {
  fn quote(q: &mut RustQuoter) -> Type {
    Type::make_struct(vec![
      q.quote::<*const u8>(),
      q.quote::<*const u8>(),
    ])
  }
}

impl QuoteType for &'static mut dyn Task {
  fn quote(q: &mut RustQuoter) -> Type {
    Type::make_struct(vec![
      q.quote::<*const u8>(),
      q.quote::<*const u8>(),
    ])
  }
}

impl<A: 'static + ReverseAlloc> QuoteType for ABox<dyn Task, A> {
  fn quote(q: &mut RustQuoter) -> Type {
    Type::make_struct(vec![
      q.quote::<*const u8>(),
      q.quote::<*const u8>(),
    ])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
#[repr(C)]
pub struct TaskData {
  pub state: StateId,
  pub origin: ChartId,
  pub position: PosId,
  pub trace: TraceId,
  pub grammar_id: GrammarId,
}

impl QuoteType for TaskData {
  fn quote(q: &mut RustQuoter) -> Type {
    Type::make_struct(vec![
      q.quote::<StateId>(),
      q.quote::<PosId>(),
      q.quote::<PosId>(),
      q.quote::<TraceId>(),
      q.quote::<GrammarId>(),
    ])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
