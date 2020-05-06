#![feature(arbitrary_self_types)]
#![feature(box_syntax)]
#![feature(generators)]
#![feature(nll)]
#![feature(specialization)]
#![feature(trace_macros)]
#![feature(type_ascription)]

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use] 
extern crate north_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod builtin;
pub mod gen_mir;
pub mod sema;
pub mod structure;
pub mod lexer;
pub mod parse;

pub use {
  crate::structure::{ast, token},
  north_core::futures,
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  crate::{
    lexer::StandardLexer,
    sema::resolve_constr::ResolveConstrs,
  },
  north_core::{
    compiler::{Pass},
    lang_prelude::*,
    visitor::VisitPass,
  },
  north_lexer::{LexerPass},
  north_loader::{LoadPass, SourceFileNode},
  north_parser::ParsePass,
  north_scope::ResolvePass,
  north_typesys::{DerefTypesPass, InferTypesPass},
  std::{
    path::PathBuf,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LangBase { }

impl LangBase {
  pub fn new() -> Self {
    Self { }
  }

  /*pub fn new_codegen_pass() -> Rc<dyn Pass> {
    Rc::new(InferTypesPass::new())
  }*/

  pub fn new_deref_types_pass() -> Rc<dyn Pass> {
    Rc::new(DerefTypesPass::new())
  }

  pub fn new_infer_types_pass() -> Rc<dyn Pass> {
    Rc::new(InferTypesPass::new())
  }

  pub fn new_lex_pass() -> Rc<dyn Pass> {
    Rc::new(LexerPass::<StandardLexer>::new())
  }

  pub fn new_load_pass() -> Rc<dyn Pass> {
    Rc::new(LoadPass::new())
  }

  pub fn new_parse_pass() -> Rc<dyn Pass> {
    Rc::new(ParsePass::<ast::Items>::new())
  }

  pub fn new_resolve_constrs_pass() -> Rc<dyn Pass> {
    Rc::new(VisitPass::<ResolveConstrs>::new())
  }

  pub fn new_resolve_pass() -> Rc<dyn Pass> {
    Rc::new(ResolvePass::new())
  }

  pub fn new_root<A: Into<PathBuf>>(root_filename: A) -> SourceFileNode {
    SourceFileNode::new(root_filename)
  }
}

impl Language for LangBase {
  fn name(&self) -> &str {
    "base"
  }
}

language_parts! {
  LangBase {
    gen_mir::reduce_lval::ReduceLValPart,
    gen_mir::reduce_mod::ReduceModPart,
    gen_mir::reduce_type::ReduceTypePart,
    gen_mir::reduce_value::ReduceValuePart,
    parse::ParsePart,
    sema::derive_scope::DeriveScopePart,
    sema::infer_types::InferTypesPart,
    sema::resolve::ResolvePart,
    sema::resolve_constr::ResolveConstrsPart,
    sema::unify_types::UnifyTypesPart,
    structure::ast::ConceptMapPart,
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
