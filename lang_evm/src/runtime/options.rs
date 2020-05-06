////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct Options {
  pub dump_parse_tree: bool,
  pub enable_gc: bool,
  pub incorporate_reductions: bool,
  pub show_opt_mir: bool,
  pub show_reductions: bool,
}

impl Default for Options {
  fn default() -> Self {
    Self {
      dump_parse_tree: false,
      enable_gc: true,
      incorporate_reductions: true,
      show_opt_mir: false,
      show_reductions: false,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
