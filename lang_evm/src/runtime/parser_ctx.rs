use {
  crate::{
    runtime::{
      CallElem, CallSpecId, CallSpecInterner,
      MatchElem, MatchSpecId, MatchSpecInterner,
      MatchIdInterner, ReduceIdInterner,
    },
  },
  north_core::{
    cell::prelude::*,
  },
  north_derive::{
    cell
  },
  std::{
    cell::Ref,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
#[cell(default, scope(compiler))]
static PARSER_CTX: Rc<ParserCtx> = ();
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct ParserCtx {
  crate call_spec_interner: CallSpecInterner,
  crate match_id_interner: MatchIdInterner,
  crate match_spec_interner: MatchSpecInterner,
  crate reduce_id_interner: ReduceIdInterner,
}

impl ParserCtx {
  pub fn new() -> Self {
    Self {
      call_spec_interner: <_>::default(),
      match_id_interner: <_>::default(),
      match_spec_interner: <_>::default(),
      reduce_id_interner: <_>::default(),
    }
  }

  pub fn call_spec_interner(&self) -> &CallSpecInterner {
    &self.call_spec_interner
  }

  pub fn dump_info(&self) {
    println!("PARSER_CTX:");
    println!("  call spec interner len: {:?}", self.call_spec_interner.len());
    println!("  match id interner len: {:?}", self.match_id_interner.len());
    println!("  match spec interner len: {:?}", self.match_spec_interner.len());
    println!("  reduce id interner len: {:?}", self.reduce_id_interner.len());

    // for (k, _) in self.match_spec_interner.entries.borrow().iter() {
    //   println!("  MSPEC {:?}", k.len());
    // }
  }

  pub fn get() -> Rc<ParserCtx> {
    cell_get!(PARSER_CTX)
  }

  pub fn get_call_spec(&self, id: CallSpecId) -> Ref<[CallElem]> {
    self.call_spec_interner.get(id)
  }

  pub fn get_match_spec(&self, id: MatchSpecId) -> Ref<[MatchElem]> {
    self.match_spec_interner.get(id)
  }

  pub fn match_spec_interner(&self) -> &MatchSpecInterner {
    &self.match_spec_interner
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
