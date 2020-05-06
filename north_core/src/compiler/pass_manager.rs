use {
  crate::compiler::Pass,
  std::rc::Rc,
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct PassManager {
  pub passes: Vec<Rc<dyn Pass>>
}

impl PassManager {
  pub fn new() -> Self {
    Self {
      passes: Vec::new(),
    }
  }

  pub fn add_pass(&mut self, pass: Rc<dyn Pass>) {
    self.passes.push(pass);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
