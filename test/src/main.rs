#![feature(core_intrinsics)]
#![feature(nll)]

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod command_line;
pub mod parser;
pub mod profiler;

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  crate::{
    command_line::GlobalSettings,
    parser::Parser,
  },
  std::{
    alloc::System,
  },
};

/* 
  TODO: BOUNDARY: WS boundary, operator boundary
    > longest match literals? "." vs "..."
    > aka greedy DFA; recursion may be used
    * problem: DFAs from several langs may be merged
    * problem: rules have to merged for this to work
    * USE BOUNDARIES FIRST to see how large of an issue is this
  TOOD: investigate SmallMap:
    reductions, resumes may benefit from it
    > LESS OVERALL ALLOCATIONS, more inlining
    > 50%+ should fit into smaller versions
    * 2-4 reductions
    * 1-2 resumes??? << check this 
    * 2-4 suspended tasks?
  TODO: chart entries are sorted. this may be used to optimize the map
  TODO: alloc less in general; cache?

  TODO: split MATCH_SPECs:
    eagerly generate Masks for MatchSpecs if the amount is low enough

  TODO: GREEDY list ops
  TODO: critical sections for greedy/negative ops
  TODO: assembly optimizations?

  // RootNode:

  // PHASE I:
  // 1. LexPass:   LexFileNode -> TokenStreamNode
  // 2. ParsePass: TokenStreamNode -> Root
  // 3. ResolvePass: fill refs
  // 4. ExpandMacroPass

  // Phase II:
  // 1. TypeCheckPass
  // 2. ResolveDepNamesPass
  // 3. BreakContinuePass? / StructCheckPass
  // 4. CheckExprStagePass / early vs late

  // Phase III:
  // 1. Lower AST to HIR
  // 2. Transform HIR to MIR

  // Phase IV:
  // 1. MIR to LLVM
  // 2. JIT
  
*/

////////////////////////////////////////////////////////////////////////////////////////////////

#[global_allocator]
static A: System = System;

////////////////////////////////////////////////////////////////////////////////////////////////

fn main() {
  env_logger::init();

  match GlobalSettings::parse_cmd_line() {
    Some(settings) => {
      let mut parser = Parser::new(settings);
      parser.exec();
    }

    None => {
      eprintln!("error: missing some command line args");
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
