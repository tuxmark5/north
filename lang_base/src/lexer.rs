use {
  crate::token,
  north_core::{
    compiler::{Diag, DiagVec},
    pos::{BytePos, Span},
    util::token::{Token, TokenOpaque},
  },
  north_lexer::{
    KeywordDyn, Lexer, OperatorDyn,
    decode_utf8::DecodeUtf8,
    trie::Trie,
  },
  std::{
    any::Any,
    collections::HashMap,
    fmt::Debug,
    mem, result
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

type Result<T> = result::Result<T, ()>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LexerError {
  message: &'static str,
  span: Span,
}

impl Diag for LexerError {
  fn message(&self) -> &str { self.message }
  fn span(&self) -> Option<Span> { Some(self.span) }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct StandardLexer<'a> {
  completed: bool,
  current_token: String,
  error_start: Option<BytePos>,
  errors: Vec<Box<dyn Diag>>,
  keywords: HashMap<&'static str, &'static dyn KeywordDyn>,
  operators: Trie<char, &'static dyn OperatorDyn>,
  source: DecodeUtf8<'a>,
}

impl<'a> StandardLexer<'a> {
  pub fn new(base_pos: BytePos, src: &'a str) -> Self {
    let source = DecodeUtf8::new(base_pos, src.as_bytes());

    let mut lexer = StandardLexer {
      completed: false,
      current_token: String::new(),
      error_start: None,
      errors: Vec::new(),
      keywords: HashMap::new(),
      operators: Trie::new(),
      source,
    };

    for keyword in token::KEYWORDS {
      lexer.keywords.insert(keyword.repr(), *keyword);
    }

    for operator in token::OPERATORS {
      let key = operator.repr().chars();
      lexer.operators.insert(key, *operator);
    }

    lexer
  }

  pub fn add_error(&mut self, span: Span, message: &'static str) {
    let error = LexerError { span, message };
    self.errors.push(box error);
  }

  pub fn advance(&mut self) {
    if let Some(c) = self.source.next() {
      self.current_token.push(c);
    }
  }

  pub fn advance_complete<T: Any + Debug>(&mut self, token: T) -> Result<TokenOpaque> {
    self.advance();
    self.complete(token)
  }

  pub fn advance_current(&mut self) -> Result<char> {
    self.advance();
    self.current()
  }

  pub fn advance_skip(&mut self) {
    self.complete_error();
    let _ = self.source.next();
  }

  pub fn call<F>(&mut self, f: F) -> Result<TokenOpaque> where
    F: FnOnce(&mut Self) -> Result<TokenOpaque>
  {
    let start_pos = self.source.current_pos();
    let mut result = f(self);
    let end_pos = self.source.current_pos();

    if let Ok(ref mut token) = result {
      token.set_span(Span::new(start_pos, end_pos));
      self.complete_error();
    } else {
      let _ = self.source.next();
      if self.error_start.is_none() {
        self.error_start = Some(start_pos);
      }
    }

    result
  }

  pub fn complete<T: Any + Debug>(&mut self, token: T) -> Result<TokenOpaque> {
    let token_opaque = Token::new(token).into_opaque();
    self.complete_opaque(token_opaque)
  }

  pub fn complete_error(&mut self) {
    if let Some(error_start) = self.error_start.take() {
      let error_end = self.source.current_pos();
      let error_span = Span::new(error_start, error_end);
      self.add_error(error_span, "invalid character sequence");
    }
  }

  pub fn complete_opaque(&mut self, token: TokenOpaque) -> Result<TokenOpaque> {
    self.current_token = String::new();
    Ok(token)
  }

  pub fn current(&mut self) -> Result<char> {
    match self.source.peek() {
      Some(c) => Ok(c),
      _ => Err(()),
    }
  }

  pub fn into_tokens(self) -> Tokens<'a> {
    Tokens { lexer: self }
  }

  pub fn lex(&mut self) -> Result<TokenOpaque> {
    loop {
      let chr = match self.current() {
        Ok(chr)   => chr,
        Err(_)    => {
          self.completed = true;
          break self.complete(token::EOF)
        },
      };

      match chr {
        ' '       => self.advance_skip(),
        '\t'      => self.advance_skip(),
        '\n'      => self.advance_skip(),

        '0'..='9' => break self.call(Self::lex_lit_int),
        'a'..='z' => break self.call(Self::lex_ident),
        'A'..='Z' => break self.call(Self::lex_ident),
        '_'       => break self.call(Self::lex_ident),
        '\"'      => break self.call(Self::lex_lit_str),

        _         => {
          match self.call(Self::lex_operator) {
            Ok(ref op) if op.is_a::<token::OpDivDiv>() => self.lex_comment(),
            op => break op,
          }
        },
      }
    }
  }

  pub fn lex_comment(&mut self) {
    while let Ok(chr) = self.current() {
      match chr {
        '\n' => break,
        _    => self.advance_skip(),
      }
    }
  }

  pub fn lex_hex_escape(&mut self) -> Result<char> {
    let hi = self.current()?;
    let lo = self.advance_current()?;
    let _  = self.advance();
    let hi = hi.to_digit(16).ok_or(())? as u8;
    let lo = lo.to_digit(16).ok_or(())? as u8;
    Ok((hi * 16 + lo) as char)
  }

  pub fn lex_ident(&mut self) -> Result<TokenOpaque> {
    let start_pos = self.source.current_pos();

    loop {
      match self.current()? {
        'a'..='z' => self.advance(),
        'A'..='Z' => self.advance(),
        '0'..='9' => self.advance(),
        '_'       => self.advance(),

        '\"' => match self.take_current().as_str() {
          "r"   => break self.lex_lit_char_class(),
          _     => {
            let span = Span::new(start_pos, self.source.current_pos());
            self.add_error(span, "invalid string literal prefix");
            break self.lex_lit_str();
          }
        },

        _ => break {
          let current = self.take_current();
          let token = match self.keywords.get(current.as_str()) {
            Some(keyword) => keyword.to_token(),
            None => Token::new(token::Ident { name: current }).into_opaque(),
          };
          self.complete_opaque(token)
        },
      }
    }
  }

  pub fn lex_lit_char_class(&mut self) -> Result<TokenOpaque> {
    self.advance_skip();

    let mut result = token::LitCharClass { invert: false, groups: Vec::new() };

    if self.current()? == '^' {
      self.advance_skip();
      result.invert = true;
    }

    loop {
      match self.current()? {
        '.'   => {
          self.advance_skip();
          result.groups.push(token::LitCharClassGroup::Any);
        },

        '\"'  => {
          break self.advance_complete(result)
        },

        _     => {
          let first = self.lex_lit_char_class_single()?;
          let entry = if self.current()? == '-' {
            self.advance_skip();
            let second = self.lex_lit_char_class_single()?;
            token::LitCharClassGroup::Range(first, second)
          } else {
            token::LitCharClassGroup::Single(first)
          };
          result.groups.push(entry);
        }
      }
    }
  }

  pub fn lex_lit_char_class_single(&mut self) -> Result<char> {
    match self.current()? {
      '\"'  => Err(()),
      '\\'  => { self.advance_skip(); self.lex_lit_char_escape() },
      c     => { self.advance_skip(); Ok(c) },
    }
  }

  pub fn lex_lit_char_escape(&mut self) -> Result<char> {
    match self.current()? {
      '\\'  => { self.advance_skip(); Ok('\\') },
      '\''  => { self.advance_skip(); Ok('\'') },
      '\"'  => { self.advance_skip(); Ok('\"') },
      'n'   => { self.advance_skip(); Ok('\n') },
      'r'   => { self.advance_skip(); Ok('\r') },
      't'   => { self.advance_skip(); Ok('\t') },
      'x'   => { self.advance_skip(); self.lex_hex_escape() },
      _     => { Err(()) },
    }
  }

  pub fn lex_lit_int(&mut self) -> Result<TokenOpaque> {
    loop {
      match self.current()? {
        '0'..='9' => self.advance(),
        '_'       => self.advance_skip(),
        _         => break {
          let result = token::LitInt { value: self.take_current() };
          self.complete(result)
        },
      }
    }
  }

  pub fn lex_lit_str(&mut self) -> Result<TokenOpaque> {
    self.advance_skip();

    loop {
      match self.current()? {
        '\\' => {
          self.advance_skip();
          let chr = self.lex_lit_char_escape()?;
          self.current_token.push(chr);
        },

        '\"' => break {
          self.advance_skip();
          let result = token::LitStr { value: self.take_current() };
          self.complete(result)
        },

        _ => self.advance(),
      }
    }
  }

  pub fn lex_operator(&mut self) -> Result<TokenOpaque> {
    let mut trie = &self.operators;

    loop {
      let chr = match self.source.peek() {
        Some(chr) => chr,
        _ => break,
      };

      match trie.subtrie(&chr) {
        Some(new_trie) => {
          let _ = self.source.next();
          trie = new_trie;
        },
        None => break,
      }
    }

    trie.value()
      .map(|v| v.to_token())
      .ok_or(())
  }

  pub fn next_token(&mut self) -> Result<TokenOpaque> {
    self.lex()
  }

  pub fn take_current(&mut self) -> String {
    mem::replace(&mut self.current_token, String::new())
  }
}

impl<'b> Lexer for StandardLexer<'b> {
  fn lex<'a>(base_pos: BytePos, input: &'a str) -> (Vec<TokenOpaque>, DiagVec) {
    let mut lexer = StandardLexer::new(base_pos, input);
    let mut tokens = Vec::new();

    while !lexer.completed {
      if let Ok(token) = lexer.lex() {
        tokens.push(token);
      }
    }

    (tokens, lexer.errors)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Tokens<'a> {
  lexer: StandardLexer<'a>,
}

impl<'a> Iterator for Tokens<'a> {
  type Item = TokenOpaque;

  fn next(&mut self) -> Option<Self::Item> {
    if self.lexer.completed {
      return None;
    }

    match self.lexer.next_token() {
      Ok(token) => Some(token),
      Err(_) => None,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
