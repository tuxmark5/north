
use {
  proc_macro2::{TokenStream},
  quote::ToTokens,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ReduceResult {
  streams: Vec<(isize, TokenStream)>
}

impl ReduceResult {
  pub fn new(stream: TokenStream) -> Self {
    Self { streams: vec![(0, stream)] }
  }

  // pub fn new_empty() -> Self {
  //   Self { streams: Vec::new() }
  // }

  pub fn new_multi<I>(iter: I) -> Self where
    I: IntoIterator<Item=ReduceResult>
  {
    let mut result = Self { streams: Vec::new() };
    for item in iter { result.merge(item); }
    result
  }

  pub fn new_pair(early: TokenStream, late: TokenStream) -> Self {
    Self { streams: vec![(0, early), (10, late)] }
  }

  pub fn merge(&mut self, other: ReduceResult) {
    let streams = other.streams.into_iter();
    self.streams.extend(streams);
  }
}

impl ToTokens for ReduceResult {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let mut streams = self.streams.iter().collect::<Vec<_>>();
    streams.sort_by_key(|s| s.0);
    for (_, s) in streams { s.to_tokens(tokens); }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
