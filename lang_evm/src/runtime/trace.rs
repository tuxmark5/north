use {
  crate::{
    collections::IndexMap,
    runtime::{PosId, ReduceId, trace_entry::*},
    util::Index,
  },
  std::{
    fmt::{self, Debug},
    mem::{size_of},
    ptr::{copy_nonoverlapping},
  },
  std_alloc::{
    raw_vec::RawVec,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type TraceId = Index<u32, TraceIdTag>;
pub enum TraceIdTag {}
pub type TraceSlot = [u32; 4];
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TraceDecoder<'a> {
  cache: IndexMap<TraceId, ()>,
  store: &'a TraceStore,
}

impl<'a> TraceDecoder<'a> {
  pub fn new(store: &'a TraceStore) -> Self {
    Self {
      cache: <_>::default(),
      store,
    }
  }

  pub fn dump(&mut self, depth: usize, mut index: TraceId) {
    let mut children = Vec::new();

    loop {
      match self.store.decode(index) {
        TraceEntryRef::Alt(_) |
        TraceEntryRef::Reduce(_) |
        TraceEntryRef::ReduceEx(_) => {
          children.push(index);
          break;
        }
        TraceEntryRef::Empty(_) => {
          break;
        }
        TraceEntryRef::Shift(e) => {
          children.push(e.elem);
          index = e.rest;
        }
      }
    }

    for trace_id in children.iter().rev() {
      self.dump_reduce(depth + 1, *trace_id);
    }
  }

  pub fn dump_reduce(&mut self, mut depth: usize, mut index: TraceId) {
    let mut extra_depth = false;

    // if let Some(_) = self.cache.insert(index, ()) {
    //   self.print_str(depth, index, "<CYCLE>");
    //   return;
    // }

    loop {
      match self.store.decode(index) {
        TraceEntryRef::Alt(e) => {
          if !extra_depth { 
            let text = format!("<ALT-{}>", e.extra);
            self.print_str(depth, index, text.as_ref());
            extra_depth = true; depth += 1; 
          }
          self.dump_reduce(depth, e.alt_b);
          index = e.alt_a;
        }
        TraceEntryRef::Reduce(e) => {
          self.print_reduce(depth, index, e.reduce_id);
          self.dump(depth, e.rest);
          break;
        }
        TraceEntryRef::ReduceEx(e) => {
          self.print_reduce_ex(depth, index, e.reduce_id, e.pos0, e.pos1);
          self.dump(depth, e.rest);
          break;
        }
        _ => {
          panic!("invalid reduce trace");
        }
      }
    }
  }

  pub fn print_reduce(&self, depth: usize, index: TraceId, reduce_id: ReduceId) {
    print!("{:8}|", index.idx());
    for _ in 0..depth { print!("  "); }
    println!("{:?}", reduce_id);
  }

  pub fn print_reduce_ex(
    &self, depth: usize, index: TraceId, 
    reduce_id: ReduceId, pos0: PosId, pos1: PosId
  ) {
    print!("{:8}|", index.idx());
    for _ in 0..depth { print!("  "); }
    println!("{:?}: {:?}-{:?}", reduce_id, pos0, pos1);
  }

  pub fn print_str(&self, depth: usize, index: TraceId, text: &str) {
    print!("{:8}|", index.idx());
    for _ in 0..depth { print!("  "); }
    println!("{}", text);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TraceStore {
  entries: RawVec<u32>,
  head: usize,
}

impl TraceStore {
  pub fn new() -> Self {
    let entries = RawVec::with_capacity(128);
    let mut result = Self { entries, head: 0 };
    result.add_trace(TraceEmpty::new());
    result
  }

  pub fn add_trace<E: Copy>(&mut self, entry: E) -> TraceId {
    let size = size_of::<E>() / 4;
    let slot = self.new_slot() as *mut E;
    let index = TraceId::from(self.head);
    unsafe { copy_nonoverlapping(&entry, slot, 1); }
    self.head += size;
    index
  }

  pub fn add_trace_dyn(&mut self, entry: &TraceEntry) -> TraceId {
    let size = entry.slot_size();
    let index = TraceId::from(self.head);
    let src_slot = entry as *const _ as *const u32;
    let dst_slot = self.new_slot() as *mut u32;
    unsafe { copy_nonoverlapping(src_slot, dst_slot, size); }
    self.head += size;
    index
  }

  pub fn add_trace_to<E: Copy>(&mut self, dst: &mut TraceId, entry: E) -> TraceId {
    let result = self.add_trace(entry);
    *dst = result;
    result
  }

  pub fn add_reduce(
    &mut self, dst: &mut TraceId, reduce_id: ReduceId, range: (PosId, PosId),
  ) -> TraceId {
    let entry = TraceReduceEx::new(reduce_id, *dst, range.0, range.1);
    // let entry = TraceReduce::new(reduce_id, *dst);
    self.add_trace_to(dst, entry)
  }
  
  pub fn add_shift(&mut self, dst: TraceId, elem: TraceId) -> TraceId {
    let entry = TraceShift::new(dst, elem);
    self.add_trace(entry)
  }

  pub fn compare_reduce(&self, a: TraceId, b: TraceId) -> bool {
    if a == b { return true }

    let ref_a = self.slot(a).decode();
    let ref_b = self.slot(b).decode();

    use TraceEntryRef::*;
    match (ref_a, ref_b) {
      (ReduceEx(ra), ReduceEx(rb)) => {
        self.compare_shift(ra.rest, rb.rest)
      },
      _ => {
        false
      }
    }
  }

  pub fn compare_shift(&self, mut a: TraceId, mut b: TraceId) -> bool {
    loop {
      if a == b { break true }

      let ref_a = self.slot(a).decode();
      let ref_b = self.slot(b).decode();

      use TraceEntryRef::*;
      if let (Shift(sa), Shift(sb)) = (ref_a, ref_b) {
        if sa.elem != sa.elem { break false }
        a = sa.rest; b = sb.rest;
      } else {
        break false;
      }
    }
  }

  pub fn decode(&self, index: TraceId) -> TraceEntryRef {
    self.slot(index).decode()
  }

  pub fn dump_info(&self) {
    println!("TRACE_STORE:");
    println!("  len: {}", self.head);
  }

  pub fn merge_reduce(&mut self, dst: TraceId, src: TraceId) {
    if self.compare_reduce(dst, src) { return }
    let dst_entry = self.slot(dst).clone();
    let dst_copy = self.add_trace_dyn(&dst_entry);
    let dst_slot = self.slot_mut(dst);
    dst_slot.conv_into_alt(dst_copy, src);
  }

  pub fn merge_shift(&mut self, dst: TraceId, src: TraceId) {
    let elem = self.slot_mut(dst).as_shift_mut().elem;
    let entry = TraceAlt::new(elem, src);
    let alt_id = self.add_trace(entry);
    let slot = self.slot_mut(dst).as_shift_mut();
    slot.elem = alt_id;
  }

  pub fn new_slot(&mut self) -> *mut TraceEntry {
    self.entries.reserve(self.head, 4);
    let start = self.entries.ptr();
    let pos = unsafe { start.offset(self.head as isize) };
    pos as *mut TraceEntry
  }

  pub fn slot(&self, index: TraceId) -> &TraceEntry {
    let start = self.entries.ptr();
    let offset = index.idx() as isize;
    let pos = unsafe { start.offset(offset) };
    unsafe { &*(pos as *mut TraceEntry) }
  }

  pub fn slot_mut(&mut self, index: TraceId) -> &mut TraceEntry {
    let start = self.entries.ptr();
    let offset = index.idx() as isize;
    let pos = unsafe { start.offset(offset) };
    unsafe { &mut *(pos as *mut TraceEntry) }
  }
}

impl Debug for TraceStore {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "TraceStore")
  }
}

impl Default for TraceStore {
  fn default() -> Self {
    Self::new()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
