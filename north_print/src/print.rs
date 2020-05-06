use {
  crate::{
    print_aspect::PrintAspect,
    print_output::PrintOutput,
  },
  north_core::{
    prelude::*,
    rule_prelude::*
  },
  north_derive::context,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Print<N>: 'static where
  N: Node + ?Sized
{
  fn print<'c, 'a, 'o, 'n>(&self, ctx: PrintCtx<'c, 'a, 'o, 'n, N>);
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type PrintCtx<'c, 'a, 'o, 'n, N> = (
  &'c mut PrintCtxCore<'a>, &'n N, &'o mut PrintOutput
);

////////////////////////////////////////////////////////////////////////////////////////////////

#[context(ModelCellCtxExt, model_cell)]
#[context(ModelNodeIdCtxExt)]
#[context(NodeIdCtxExt, node_id)]
pub struct PrintCtxCore<'a> {
  pub aspect     : &'a PrintAspect,
  pub model_cell : ModelCell,
  pub node_id    : NodeId,
}

impl<'a> PrintCtxCore<'a> {
  pub fn new(
    comp    : &Compiler,
    aspect  : &'a PrintAspect,
    node_id : NodeId,
  ) -> Self {
    Self {
      aspect,
      model_cell: comp.model_cell.clone(),
      node_id,
    }
  }

  pub fn fork(&self, node_id: NodeId) -> Self {
    Self {
      aspect: self.aspect,
      model_cell: self.model_cell.clone(),
      node_id,
    }
  }

  pub fn print_cld<I: ToNodeId>(&mut self, cld: I, out: &mut PrintOutput) {
    let node_id = cld.to_node_id().cast();
    let mut core = self.fork(node_id);
    self.aspect.print(&mut core, out);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait PrintDyn {
  fn print_dyn(&self, ctx: PrintCtx<dyn Node>);
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type PrintRuleKey = RuleKeySingleNode;
pub type PrintRuleMap = RuleMap<PrintRuleKey, dyn PrintDyn>;

impl<N, R> PrintDyn for RuleDynAdapter<N, R> where
  N: Node + ?Sized, R: Print<N>
{
  fn print_dyn(&self, (ctx, node, out): PrintCtx<dyn Node>) {
    let node = dynamic_cast(node).ok().unwrap();
    self.rule.print((ctx, node, out))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! print_r {
  (__add, $aspect:expr, [$node_ty:ty => $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<PrintAspect>($aspect) {
      aspect.print_rules.add::<$node_ty, _>($rule);
    }
  };

  (__macro, print_rule!,
    ($node_ty:ty),
    { $($body:tt)* }
  ) => {
    #[allow(unused_variables)]
    fn print(&self, (ctx, node, out): PrintCtx<$node_ty>) {
      print_rule!(ctx, node, out, $($body)*);
    }
  };

  (__trait, ($($generics:tt)*), ($($params:tt)*), $target:ty, $body:tt) => {
    impl $($generics)* Print<$($params)*> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
