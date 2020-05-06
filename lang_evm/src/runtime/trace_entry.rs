use {
  crate::{
    runtime::{PosId, ReduceId, TraceId},
  },
  std::{
    mem::size_of,
    slice::{from_raw_parts},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub const TRACE_EMPTY: u16 = 0;
pub const TRACE_ALT: u16 = 2;
pub const TRACE_REDUCE: u16 = 3;
pub const TRACE_REDUCE_EX: u16 = 4;
pub const TRACE_SHIFT: u16 = 5;
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub union TraceEntry {
  pub alt: TraceAlt,
  pub empty: TraceEmpty,
  pub reduce: TraceReduce,
  pub reduce_ex: TraceReduceEx,
  pub shift: TraceShift,
  pub type_: u16,
}

impl TraceEntry {
  pub fn as_shift_mut(&mut self) -> &mut TraceShift {
    unsafe {
      assert_eq!(self.type_, TRACE_SHIFT);
      &mut self.shift
    }
  }

  pub fn as_slice(&self) -> &[u32] {
    let data = self as *const _ as *const u32;
    let size = self.slot_size();
    unsafe { from_raw_parts(data, size) }
  }

  pub fn conv_into_alt(&mut self, alt_a: TraceId, alt_b: TraceId) {
    unsafe {
      self.alt.header = TRACE_ALT;
      self.alt.extra = 1;
      self.alt.alt_a = alt_a;
      self.alt.alt_b = alt_b;
    }
  }

  pub fn decode(&self) -> TraceEntryRef {
    unsafe {
      match self.type_ {
        TRACE_EMPTY     => TraceEntryRef::Empty(&self.empty),
        TRACE_ALT       => TraceEntryRef::Alt(&self.alt),
        TRACE_REDUCE    => TraceEntryRef::Reduce(&self.reduce),
        TRACE_REDUCE_EX => TraceEntryRef::ReduceEx(&self.reduce_ex),
        TRACE_SHIFT     => TraceEntryRef::Shift(&self.shift),
        _ => panic!("invalid trace entry type: {:?}", self.type_),
      }
    }
  }

  pub fn slot_size(&self) -> usize {
    unsafe {
      match self.type_ {
        TRACE_EMPTY     => slot_size_of::<TraceEmpty>(),
        TRACE_ALT       => slot_size_of::<TraceAlt>(),
        TRACE_REDUCE    => slot_size_of::<TraceReduce>(),
        TRACE_REDUCE_EX => slot_size_of::<TraceReduceEx>(),
        TRACE_SHIFT     => slot_size_of::<TraceShift>(),
        _ => panic!("invalid trace entry type: {:?}", self.type_),
      }
    }
  }
}

impl PartialEq for TraceEntry {
  fn eq(&self, other: &Self) -> bool {
    let slice_l = self.as_slice();
    let slice_r = other.as_slice();
    slice_l.eq(slice_r)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum TraceEntryMut<'a> {
  Alt(&'a mut TraceAlt),
  Empty(&'a mut TraceEmpty),
  Reduce(&'a mut TraceReduce),
  ReduceEx(&'a mut TraceReduceEx),
  Shift(&'a mut TraceShift),
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum TraceEntryRef<'a> {
  Alt(&'a TraceAlt),
  Empty(&'a TraceEmpty),
  Reduce(&'a TraceReduce),
  ReduceEx(&'a TraceReduceEx),
  Shift(&'a TraceShift),
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
#[repr(C)]
pub struct TraceAlt {
  pub header: u16,
  pub extra: u16,
  pub alt_a: TraceId,
  pub alt_b: TraceId,
}

impl TraceAlt {
  pub fn new(alt_a: TraceId, alt_b: TraceId) -> Self {
    Self { header: TRACE_ALT, extra: 0, alt_a, alt_b }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
#[repr(C)]
pub struct TraceEmpty {
  pub header: u16,
  pub extra: u16,
}

impl TraceEmpty {
  pub fn new() -> Self {
    Self { header: TRACE_EMPTY, extra: 0 }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
#[repr(C)]
pub struct TraceReduce {
  pub header: u16,
  pub reduce_id: ReduceId,
  pub rest: TraceId,
}

impl TraceReduce {
  pub fn new(reduce_id: ReduceId, rest: TraceId) -> Self {
    Self { header: TRACE_REDUCE, reduce_id, rest }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
#[repr(C)]
pub struct TraceReduceEx {
  pub header: u16,
  pub reduce_id: ReduceId,
  pub rest: TraceId,
  pub pos0: PosId,
  pub pos1: PosId,
}

impl TraceReduceEx {
  pub fn new(reduce_id: ReduceId, rest: TraceId, pos0: PosId, pos1: PosId) -> Self {
    Self { header: TRACE_REDUCE_EX, reduce_id, rest, pos0, pos1 }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
#[repr(C)]
pub struct TraceShift {
  pub header: u16,
  pub extra: u16,
  pub rest: TraceId,
  pub elem: TraceId,
}

impl TraceShift {
  pub fn new(rest: TraceId, elem: TraceId) -> Self {
    Self { header: TRACE_SHIFT, extra: 0, rest, elem }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn slot_size_of<T>() -> usize {
  size_of::<T>() / 4
}

////////////////////////////////////////////////////////////////////////////////////////////////
