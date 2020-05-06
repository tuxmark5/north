use {
  crate::{
    ast,
    base_ext::{
      gen_mir::{FailBlock},
      sema::collect_impls,
    },
    mir::{self, ReduceKind, RValLinkOpt},
    runtime::{MatchId, ReduceId},
  },
  lang_base::gen_mir::{type_of_reduced},
  lang_mir::{CodegenCtxExt, ValueRed},
  north_core::{
    context::{ModelNodeIdCtxExt, NodeIdCtxExt},
    node_id::ToNodeId
  },
  north_derive::aspect_rules,
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ValueRed, ast::ItemBuiltins => DefaultRule] {
    fn reduce(self, _ctx, _node, _cur) -> RValLinkOpt {
      None
    }
  }

  reduce_r![ValueRed, ast::ItemGrammar => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> RValLinkOpt {
      let map_entry = |entry: collect_impls::RuleImpl| {
        let rule = ctx.reduce_ref(entry.impl_id, cur).unwrap();
        mir::GrammarEntry { 
          match_id: MatchId::new(entry.rule_id),
          reduce_id: ReduceId::new(entry.impl_id),
          prec: entry.prec,
          rule: rule.to_node_id().cast() 
        }
      };

      let grammar_scope = ctx.parent_id().unwrap();
      let impls = collect_impls::collect_impls(grammar_scope)
        .into_iter()
        .map(map_entry)
        .collect();

      let out = ctx.make_item(mir::ItemGrammar { impls });
      Some(out.up().into())
    }
  }

  reduce_r![ValueRed, ast::ItemGroup => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      for item in &node.items { let _ = ctx.reduce_cld(item, cur); }
      None
    }
  }

  reduce_r![ValueRed, ast::ItemRule => DefaultRule] {
    fn reduce(self, ctx, node, _cur) -> RValLinkOpt {
      let name = node.name.data.to_string();
      let rule_ty = type_of_reduced(ctx, ctx.node_id()).cast().into();
      let out = ctx.make_item(mir::ItemRule::new(name, rule_ty));
      Some(out.up().into())
    }

    fn reduce_late(self, ctx, node, cur, fn_id) {
      let fn_id = fn_id.unwrap().to_node_id().cast();
      ctx.set_func_id(fn_id);

      let _ = ctx.reduce_cld(node.params, cur);

      let mut block_main = ctx.block_cur();

      let mut block_complete = ctx.block_cur();
      block_complete.ctl_ret(None);

      let mut block_reduce = ctx.block_cur();
      block_reduce.build_ctl(mir::CtlReduce { 
        kind: ReduceKind::Normal,
        reduce_id: ReduceId::new(ctx.node_id()),
        short: false,
        next: block_complete.orig_block_id().into(),
        fail: block_complete.orig_block_id().into(),
      });

      let fail_block = FailBlock::Total(block_complete.orig_block_id());
      let _ret_value = FailBlock::set(fail_block, || {
        ctx.reduce_cld(node.body, &mut block_main)
      });
      
      if !block_main.is_completed() {
        block_main.ctl_br(&block_reduce);
      }

      // Register dynamically created blocks as rule children
      ctx.model_mut().relink_children(fn_id);
    }
  }

  reduce_r![ValueRed, ast::ItemRuleDyn => DefaultRule] {
    fn reduce(self, _ctx, _node, _cur) -> RValLinkOpt {
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceItemPart {
    reduce_r![ValueRed, ast::ItemBuiltins => DefaultRule],
    reduce_r![ValueRed, ast::ItemGrammar  => DefaultRule],
    reduce_r![ValueRed, ast::ItemGroup    => DefaultRule],
    reduce_r![ValueRed, ast::ItemRule     => DefaultRule],
    reduce_r![ValueRed, ast::ItemRuleDyn  => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

// fn get_rule_reductions(
//   model: &'m Model,
//   rule: &ast::ItemRule,
// ) -> impl 'm + Iterator<Item=(u16, u16)> {
//   rule.attrs_filter::<ast::AttrPartOf>(model).map(|a| {
//     let reduce_id = a.rule.target().idx() as u16;
//     let prec = a.prec.data.value.parse::<u16>().unwrap();
//     (reduce_id, prec)
//   })
// }

////////////////////////////////////////////////////////////////////////////////////////////////
