pub trait Task {
  fn clone(&self) -> Box<dyn Task>;
  fn eq(&self, other: &dyn Task) -> bool;
  fn hash(&self) -> u32;
  fn resume(&mut self) -> TaskState;
}

pub trait TaskFactory {
  // Jit invocation here
  fn create(&self) -> Box<dyn Task>;
}

pub enum TaskState {
  Completed,
  Failed,
  Suspended,
}

pub struct SimpleTask<D> {
  origin: u32,
  offset: u32,
  state: u32, // ???; part of generator?
  data: D,
  // label, env
}

fn run_simple() {
  match self.state {
    0 => task_match_char!('0' => 1),
    1 => start
  }

  // do macros for each of those:
  // 1. match a single char & advance:
  // match ctx.input[self.offset] {
  //   'x' => { self.offset += 1; }
  //   _ => return Failed;
  // }


  // 2. advance only -> self.offset += 1;
  // 3. start a task -> ctx.spawn_task(task_descr);
  // 4. match result of a task
  // 5. fail -> return Failed
  // 6. fork -> ctx.fork_to(self, target_state)
  // 7. reduce -> ctx.reduce(self.origin, self.offset, what_to_reduce)
}
