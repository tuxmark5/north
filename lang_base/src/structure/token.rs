use {
  north_lexer::prelude::*,
  std::fmt::{self, Debug}
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct EOF;

#[derive(Clone)]
pub struct Ident {
  pub name: String
}

impl AsRef<str> for Ident {
  fn as_ref(&self) -> &str {
    self.name.as_ref()
  }
}

impl Debug for Ident {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, ":{}", self.name)
  }
}

impl ToString for Ident {
  fn to_string(&self) -> String {
    self.name.clone()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct LitCharClass {
  pub invert: bool,
  pub groups: Vec<LitCharClassGroup>,
}

#[derive(Clone, Debug)]
pub enum LitCharClassGroup {
  Any,
  Range(char, char),
  Single(char),
}

#[derive(Clone, Debug)]
pub struct LitInt {
  pub value: String
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct LitStr {
  pub value: String
}

impl AsRef<str> for LitStr {
  fn as_ref(&self) -> &str {
    self.value.as_ref()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_keywords! {
  KEYWORDS {
    KwBreak     => "break",
    KwBuiltins  => "builtins",
    KwBuiltinsG => "builtins_grammar",
    KwContinue  => "continue",
    KwDomG      => "dom_g",
    KwElse      => "else",
    KwFn        => "fn",
    KwGrammar   => "grammar",
    KwGroup     => "group",
    KwIf        => "if",
    KwLet       => "let",
    KwLoop      => "loop",
    KwParse     => "parse",
    KwParseG    => "parse_g",
    KwParseL    => "parse_l",
    KwReject    => "reject",
    KwReturn    => "return",
    KwRule      => "rule",
    KwRuleDyn   => "rule_dyn",
    KwShiftP    => "shift_p",
    KwWhile     => "while",
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_operators! {
  OPERATORS {
    OpAnd          => "&"  ,
    OpAndAnd       => "&&" ,
    OpBlockEnd     => "}"  ,
    OpBlockStart   => "{"  ,
    OpBracketL     => "["  ,
    OpBracketR     => "]"  ,
    OpColon        => ":"  ,
    OpComma        => ","  ,
    OpDiv          => "/"  ,
    OpDivDiv       => "//" ,
    OpDivEq        => "/=" ,
    OpEq           => "="  ,
    OpEqEq         => "==" ,
    OpEx           => "!"  ,
    OpExEq         => "!=" ,
    OpGt           => ">"  ,
    OpGtEq         => ">=" ,
    OpGtGt         => ">>" ,
    OpHash         => "#"  ,
    OpLt           => "<"  ,
    OpLtEq         => "<=" ,
    OpLtLt         => "<<" ,
    OpLtLtEq       => "<<=",
    OpMinus        => "-"  ,
    OpMinusEq      => "-=" ,
    OpMinusGt      => "->" ,
    OpMod          => "%"  ,
    OpModEq        => "%=" ,
    OpModQm        => "%?" ,
    OpMult         => "*"  ,
    OpMultEq       => "*=" ,
    OpOr           => "|"  ,
    OpOrOr         => "||" ,
    OpParenL       => "("  ,
    OpParenR       => ")"  ,
    OpPlus         => "+"  ,
    OpPlusEq       => "+=" ,
    OpQm           => "?"  ,
    OpSemicolon    => ";"  ,
    OpTilde        => "~"  ,
    OpXor          => "^"  ,
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
