use {
  north_core::prelude::*,
  std::fmt::{Debug, Display},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct PrintOutput {
  pub buffer: String,
  pub indent: usize,
  pub newline: bool,
}

impl PrintOutput {
  pub fn new() -> Self {
    Self {
      buffer: String::new(),
      indent: 0,
      newline: true
    }
  }

  pub fn print<T: AsRef<str>>(&mut self, data: T) {
    if self.newline {
      self.newline = false;
      self.print_indent();
    }

    let mut lines = data.as_ref().lines();

    if let Some(line) = lines.next() {
      self.buffer.push_str(line);
    }

    for line in lines {
      self.buffer.push('\n');
      self.print_indent();
      self.buffer.push_str(line);
    }
  }

  pub fn print_debug<T: Debug>(&mut self, data: &T) {
    let data = format!("{:#?}", data);
    self.print(data);
  }

  pub fn print_display<T: Display>(&mut self, data: &T) {
    let data = format!("{}", data);
    self.print(data);
  }

  pub fn print_id<I: ToNodeId>(&mut self, node: I) {
    let data = format!("#{:?}", node.to_node_id());
    self.print(data);
  }

  pub fn print_indent(&mut self) {
    for _ in 0..self.indent {
      self.buffer.push_str("    ");
    }
  }

  pub fn print_nl(&mut self) {
    self.buffer.push('\n');
    self.newline = true;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
