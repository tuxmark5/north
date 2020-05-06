use {
  crate::{
    mir,
    collections::IntervalMap,
    optimizer::prelude::*,
  },
  north_core::{
    Node, 
    visitor::prelude::*,
  },
  north_derive::{
    aspect_rules
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct ErrorRule;
struct DefaultRule;

aspect_rules! {
  <N: Node + ?Sized> visit_r![BuildDFA, N => ErrorRule] {
    fn visit_ok(self, ctx, _node, dbs) {
      dbs.add_rest(ctx.node_id());
    }
  }

  visit_r![BuildDFA, mir::CtlMatchChar => DefaultRule] {
    fn visit_ok(self, ctx, node, dbs) {
      let chr = node.char as u8;
      let ok_seed = ClosureSeed::new_single_with_priority(&node.ok, node.dom_group);
      let fail_seed = ClosureSeed::new_single(&node.fail);

      let mut new_map = IntervalMap::new();
      new_map.insert(chr..=chr, Some(ok_seed));
      // new_map.add_negative(fail_seed.clone());

      dbs.add_fail(fail_seed);
      dbs.add_map(ctx.node_id(), new_map);
      dbs.add_prefer_shift(node.prefer_shift);
    }
  }

  visit_r![BuildDFA, mir::CtlMatchClass => DefaultRule] {
    fn visit_ok(self, ctx, node, dbs) {
      let fail_seed = ClosureSeed::new_single_with_priority(&node.fail, node.dom_group);

      let new_map = node.ok.iter_raw()
        .map(|e| e.map_value_ref(|v| ClosureSeed::new_single_with_priority(v, node.dom_group)))
        .collect();

      dbs.add_fail(fail_seed);
      dbs.add_map(ctx.node_id(), new_map);
      dbs.add_prefer_shift(node.prefer_shift);
    }
  }
}

lang_part! {
  BuildDFAPart {
    visit_r![BuildDFA, _ => ErrorRule],
    visit_r![BuildDFA, mir::CtlMatchChar => DefaultRule],
    visit_r![BuildDFA, mir::CtlMatchClass => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
