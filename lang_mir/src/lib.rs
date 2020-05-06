#![feature(associated_type_defaults)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(drain_filter)]
#![feature(nll)]
#![feature(specialization)]
#![feature(trivial_bounds)]
#![feature(unsize)]
#![feature(untagged_unions)]

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use]
extern crate north_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod builder;
pub mod codegen_ctx;
pub mod cursor;
pub mod layout;
pub mod mir;
pub mod print;
mod quote_type;
mod quote_value;
pub mod reductions;
pub mod to_value;

pub use self::{
  codegen_ctx::{CodegenCtx, CodegenCtxExt},
  cursor::{Cursor, IntoBlockId},
  layout::Layout,
  quote_type::{quote_type, type_quoter},
  quote_value::quote_value,
  reductions::{ModRed, TypeRed, ValueRed},
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  north_core::{
    model::Language,
    prelude::*,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LangMIR;

impl Language for LangMIR {
  fn name(&self) -> &str { "mir" }
}

language_parts! {
  LangMIR {
    mir::ConceptMapPart,
    mir::ty::ConceptMapTypePart,
    print::PrintPart,
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
