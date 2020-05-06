use {
  north_core::{
    flame
  },
  std::{
    collections::BTreeMap,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct Profiler {
  groups: BTreeMap<flame::StrCow, (u64, usize)>,
}

impl Profiler {
  pub fn dump() {
    let mut dumper = Profiler::default();

    for thread in flame::threads() {
      dumper.visit_thread(thread);
    }

    println!("PROFILER MEASUREMENTS:");
    for (name, (delta, count)) in dumper.groups {
      println!("  {:16}: {:10.2} ms [{:4}]", name, (delta as f64) / 1e6f64, count);
    }
  }

  fn visit_span(&mut self, span: flame::Span) {
    let entry = self.groups.entry(span.name).or_default();
    entry.0 += span.delta;
    entry.1 += 1;

    for span in span.children { 
      self.visit_span(span); 
    }
  }

  fn visit_thread(&mut self, thread: flame::Thread) {
    for span in thread.spans {
      self.visit_span(span);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
