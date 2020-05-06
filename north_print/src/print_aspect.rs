use {
  crate::{
    print::{
      PrintCtxCore,
      PrintRuleMap
    },
    print_output::{
      PrintOutput
    },
  },
  north_core::{
    compiler::Aspect,
    structure::StorageExt,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct PrintAspect {
  pub print_rules: PrintRuleMap,
}

impl PrintAspect {
  pub fn print(&self, core: &mut PrintCtxCore, out: &mut PrintOutput) {
    let model_cell = core.model_cell.clone();
    let model = model_cell.borrow();
    let node = &*model.data(core.node_id);
    let print_rule = self.print_rules.get_with_node(node);
    print_rule.print_dyn((core, node, out));
  }
}

impl Aspect for PrintAspect {
  fn name(&self) -> &str {
    "print"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
