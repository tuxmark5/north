#![feature(decl_macro)]
#![feature(specialization)]

////////////////////////////////////////////////////////////////////////////////////////////////

extern crate north_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod dsl;
pub mod print;
pub mod print_aspect;
pub mod print_output;

pub use self::{
  dsl::Printable,
  print::{Print, PrintCtx, PrintCtxCore},
  print_aspect::PrintAspect,
  print_output::PrintOutput,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      print_r, print_rule, print_rule_value,
      dsl::Printable,
      print::{Print, PrintCtx, PrintCtxCore},
      print_aspect::PrintAspect,
    },
    north_core::{
      compiler::{AspectPart, aspect::aspect_cast},
      prelude::*,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  north_core::{
    compiler::COMPILER,
    node_id::ToNodeId,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn print_nodes<I>(idx: I) where
  I: ToNodeId
{
  COMPILER.with(|comp| {
    let aspect = comp.aspect_mut::<PrintAspect>();
    let node_id = idx.to_node_id().cast();
    let mut core = PrintCtxCore::new(comp, &*aspect, node_id);
    let mut output = PrintOutput::new();
    aspect.print(&mut core, &mut output);
    println!("{}", output.buffer);
  })
}

////////////////////////////////////////////////////////////////////////////////////////////////
