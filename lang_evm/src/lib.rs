#![feature(alloc_layout_extra)]
#![feature(allocator_api)]
#![feature(arbitrary_self_types)]
#![feature(box_syntax)]
#![feature(const_fn)]
#![feature(core_intrinsics)]
#![feature(crate_visibility_modifier)]
#![feature(decl_macro)]
#![feature(drain_filter)]
#![feature(fundamental)]
#![feature(generators)]
#![feature(nll)]
#![feature(ptr_offset_from)]
#![feature(ptr_wrapping_offset_from)]
#![feature(proc_macro_hygiene)]
#![feature(raw_vec_internals)]
#![feature(specialization)]
#![feature(stdsimd)]
#![feature(test)]
#![feature(thread_local)]
#![feature(type_ascription)]
#![feature(untagged_unions)]

#![feature(raw)]

////////////////////////////////////////////////////////////////////////////////////////////////

extern crate alloc as std_alloc;
#[macro_use] extern crate north_derive;
#[macro_use] extern crate scoped_tls;
#[macro_use] extern crate serde_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use] pub mod optimizer;

pub mod alloc;
pub mod base_ext;
pub mod collections;
pub mod mir_ext;
pub mod runtime;
pub mod util;

pub use crate::{
  base_ext::ast,
  mir_ext::mir,
  runtime::{JITResolver, ParserCtx, TaskCtx},
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  north_core::{
    lang_prelude::*,
    prelude::*,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn reduction_name(index: u32) -> String {
  COMPILER.with(|comp| {
    let model = comp.model_cell.clone();
    let model = model.borrow();
    let node_id: NodeId = NodeId::from_id(index);

    let result = if let Some(node) = model.get::<_, ast::ItemRule>(node_id) {
      node.name.data.to_string()
    } else if let Some(node) = model.get::<_, ast::ItemRuleDyn>(node_id) {
      node.name.data.to_string()
    } else {
      "aaa".to_owned()
    };

    result
  })
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LangEVM;

impl Language for LangEVM {
  fn name(&self) -> &str {
    "evm"
  }
}

language_parts! {
  LangEVM {
    base_ext::ast::ConceptMapPart,
    base_ext::gen_mir::reduce_call::ReduceCallPart,
    base_ext::gen_mir::reduce_expr::ReduceExprPart,
    base_ext::gen_mir::reduce_gexpr::ReduceGExprPart,
    base_ext::gen_mir::reduce_item::ReduceItemPart,
    base_ext::gen_mir::reduce_type::ReduceTypePart,
    base_ext::parse::ParsePart,
    base_ext::sema::collect_impls::CollectImplsPart,
    base_ext::sema::derive_scope::DeriveScopePart,
    base_ext::sema::infer_types::InferTypesPart,
    base_ext::sema::resolve::ResolvePart,
    base_ext::sema::unify_types::UnifyTypesPart,
    mir_ext::gen_llvm::ReduceCtlPart,
    mir_ext::gen_llvm::reduce_rule::ReduceRulePart,
    mir_ext::optimizer::build_closure::BuildClosurePart,
    mir_ext::optimizer::build_dfa::BuildDFAPart,
    mir_ext::optimizer::build_groups::BuildGroupsPart,
    mir_ext::optimizer::build_succs::BuildSuccsPart,
    mir_ext::mark_persistent::MarkPersistentPart,
    mir_ext::mir::ConceptMapPart,
    mir_ext::print::PrintPart,
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
