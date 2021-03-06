#![feature(box_syntax)]
#![feature(str_internals)]

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use]
extern crate north_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod decode_utf8;
pub mod keyword;
pub mod lexer_pass;
pub mod operator;
pub mod trie;

pub use self::{
  keyword::{Keyword, KeywordDyn},
  lexer_pass::LexerPass,
  operator::{Operator, OperatorDyn},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      define_keyword, define_keywords, 
      define_operator, define_operators,
    }
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  north_core::{
    compiler::Diag,
    pos::BytePos,
    prelude::*,
    util::token::TokenOpaque,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Lexer: Sized {
  fn lex<'a>(base_pos: BytePos, input: &'a str) -> (Vec<TokenOpaque>, Vec<Box<dyn Diag>>);
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TokensNode {
  pub tokens: Vec<TokenOpaque>,
}

impl TokensNode {
  pub fn new(tokens: Vec<TokenOpaque>) -> Self {
    Self { tokens }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
