#![feature(const_raw_ptr_to_usize_cast)]
#![feature(specialization)]
#![feature(untagged_unions)]

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use]
extern crate north_core;
#[macro_use]
extern crate north_derive;

//extern crate rustc_errors;
//extern crate syntax_pos;

////////////////////////////////////////////////////////////////////////////////////////////////

//pub mod code_map;
pub mod diag_handler;
pub mod load_pass;
pub mod loader_aspect;

pub use {
  diag_handler::{DiagHandler},
  load_pass::LoadPass,
  loader_aspect::LoaderAspect,
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  north_core::pos::BytePos,
  //syntax_pos::{FileMap},
  std::{
    path::PathBuf,
    rc::Rc
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct FileMap { 
  src: String
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct SourceCodeNode {
  pub file_map: Rc<FileMap>,
}

impl SourceCodeNode {
  pub fn base_pos(&self) -> BytePos {
    //BytePos(self.file_map.start_pos.0)
    BytePos(0)
  }

  pub fn src(&self) -> &String {
    //self.file_map.src.as_ref().unwrap()
    &self.file_map.src
  }
}

/*impl SourceCodeNode {
  pub fn new(source_file: PathBuf) -> Self {
    Self { source_file }
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct SourceFileNode {
  pub source_file: PathBuf,
  //pub file_map: Option<Rc<FileMap>>,
  //pub root_node: Option<NodeId>,
}

impl SourceFileNode {
  pub fn new<P: Into<PathBuf>>(path: P) -> Self {
    Self { source_file: path.into() }
  }
}
// TranslationUnit
////////////////////////////////////////////////////////////////////////////////////////////////
