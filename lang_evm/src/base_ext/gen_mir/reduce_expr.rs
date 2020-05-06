use {
  crate::{
    ast,
    base_ext::gen_mir::{FailBlock},
    mir::{self, ReduceKind},
    runtime::{ReduceId},
  },
  lang_mir::{CodegenCtxExt, ValueRed, mir::RValLinkOpt},
  north_derive::aspect_rules,
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ValueRed, ast::ExprParse => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      ctx.reduce_cld(&node.value, cur)
    }
  }

  reduce_r![ValueRed, ast::ExprReject => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let block_fail = FailBlock::get_any(ctx);
      let block_ok = ctx.block();
      let block_reject = ctx.block();
      let reduce_id = ctx.find_ancestor_id::<ast::ItemRule>().unwrap();
      let reduce_id = ReduceId::new(reduce_id);

      cur.build_ctl(mir::CtlFork {
        blocks: vec![block_reject.into(), block_ok.into()]
      });

      cur.seek(block_reject);
      ctx.reduce_cld(&node.value, cur);

      // cur.build_stmt(mir::StmtReduce { 
      //   kind: ReduceKind::Reject,
      //   reduce_id,
      // });
      // cur.ctl_br(block_fail);

      cur.build_ctl(mir::CtlReduce { 
        kind: ReduceKind::Reject,
        reduce_id,
        short: false,
        next: block_fail.clone(),
        fail: block_fail,
      });

      cur.seek(block_ok);

      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceExprPart {
    reduce_r![ValueRed, ast::ExprParse => DefaultRule],
    reduce_r![ValueRed, ast::ExprReject => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
