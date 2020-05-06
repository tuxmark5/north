use {
  crate::ast,
  north_core::{
    iter::ModelIterator,
    prelude::*,
    visitor::prelude::*,
  },
  north_derive::{aspect_rules},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct CollectImpls;

impl Visitor for CollectImpls {
  type ImplicitArgs = RuleImpls;
  type ExplicitArgs = ();
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  visit_r![CollectImpls, ast::ItemGroup => DefaultRule] {
    fn visit(self, ctx, node) {
      let rule_id = node.rule.target().to_top();
      let prec = node.prec.as_ref()
        .map(|p| p.data.value.parse::<u16>().unwrap())
        .unwrap_or(0);

      let model = ctx.model_chain();
      let rules = model.iter(&node.items).cast_ids::<ast::ItemRule>();
      for impl_id in rules {
        ctx.imp_args.push(RuleImpl { rule_id, impl_id, prec });
      }

      ctx.visit_children(node, &mut ())
    }
  }

  visit_r![CollectImpls, ast::ItemRule => DefaultRule] {
    fn visit(self, ctx, node) {
      let impl_id = ctx.node_id().cast();

      let dyn_rules = node.attrs_filter::<ast::AttrPartOf>(&*ctx.model())
        .map(|attr| (attr.rule.target().to_top(), attr.prec()))
        .collect::<Vec<_>>();

      ctx.imp_args.push(RuleImpl { rule_id: ctx.node_id(), impl_id, prec: 0 });

      for (rule_id, prec) in dyn_rules {
        ctx.imp_args.push(RuleImpl { rule_id, impl_id, prec });
      }

      ctx.visit_children(node, &mut ())
    }
  }
}

lang_part! {
  CollectImplsPart {
    visit_r![CollectImpls, _ => VisitChildren],
    visit_r![CollectImpls, ast::ItemGroup => DefaultRule],
    visit_r![CollectImpls, ast::ItemRule => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct RuleImpl {
  pub rule_id: NodeId,
  pub impl_id: NodeId<ast::ItemRule>,
  pub prec: u16,
}

pub type RuleImpls = Vec<RuleImpl>;

pub fn collect_impls(scope: NodeId) -> RuleImpls {
  let mut impls = RuleImpls::new();
  visit::<CollectImpls, _>(scope, &mut impls, &mut ());
  impls
}

////////////////////////////////////////////////////////////////////////////////////////////////
