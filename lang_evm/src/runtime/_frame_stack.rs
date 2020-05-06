use {
  crate::{
    runtime::{
      Chart, GrammarVec, TaskBox,
    },
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct FrameStack {
  indices: Vec<usize>,
}

impl FrameStack {
  pub fn new() -> Self {
    Self { indices: Vec::new() }
  }

  pub fn pop_task(
    &mut self, 
    chart: &mut Chart, 
    grammars: &GrammarVec, 
  ) -> Option<TaskBox> {
    while let Some(index) = self.indices.last() {
      // Try to pop a running task
      let entry = chart.entry_mut(*index);
      let task = entry.pop_task();
      if task.is_some() { return task; }

      // Attempt to fail a task within the entry
      let task = entry.fail_task(grammars);
      if task.is_some() { return task; }

      // Finally, fail the whole entry
      let _ = self.indices.pop();
    }

    None
  }

  pub fn push(&mut self, index: usize) {
    self.indices.push(index);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
