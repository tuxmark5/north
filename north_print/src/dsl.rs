use {
  crate::{
    print::PrintCtxCore,
    print_output::PrintOutput,
  },
  north_core::{
    Node, NodeId,
    model::{Child, Link},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Printable {
  fn print(&self, ctx: &mut PrintCtxCore, out: &mut PrintOutput);
}

impl<N: Node + ?Sized> Printable for Child<N> {
  fn print(&self, ctx: &mut PrintCtxCore, out: &mut PrintOutput) {
    ctx.print_cld(self, out);
  }
}


impl<N: Node + ?Sized> Printable for Link<N> {
  fn print(&self, ctx: &mut PrintCtxCore, out: &mut PrintOutput) {
    match self.owned {
      true => ctx.print_cld(self, out),
      false => out.print_id(self),
    }
  }
}

impl<N: Node + ?Sized> Printable for NodeId<N> {
  fn print(&self, ctx: &mut PrintCtxCore, out: &mut PrintOutput) {
    ctx.print_cld(self, out);
  }
}

/*impl<T: Printable> Printable for Option<T> {
  fn print(&self, ctx: &mut PrintCtxCore, out: &mut PrintOutput) {
    match self 
    ctx.print_cld(self, out);
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! print_rule {
  ($ctx:expr, $node:expr, $out:expr, ) => {
    // last
  };

  ($ctx:expr, $node:expr, $out:expr, { $($body:tt)* } $($rest:tt)*) => {
    $out.print("{");
    $out.print_nl();
    $out.indent += 1;
    print_rule!($ctx, $node, $out, $($body)*);
    $out.indent -= 1;
    $out.print("}");
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, C $arg:tt $($rest:tt)*) => {
    $ctx.print_cld(print_rule_value!($node, $arg), $out);
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, D $arg:tt $($rest:tt)*) => {
    $out.print_debug(print_rule_value!($node, $arg));
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, P $arg:tt $($rest:tt)*) => {
    $out.print_display(print_rule_value!($node, $arg));
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, ID($($arg:tt)*) $($rest:tt)*) => {
    $out.print_id(print_rule_value!($node, ($($arg)*)));
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, ID $($rest:tt)*) => {
    let node_id = $ctx.node_id;
    $out.print_id(node_id);
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, NL $($rest:tt)*) => {
    $out.print_nl();
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, $value:ident $($rest:tt)*) => {
    Printable::print($value, $ctx, $out);
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, $lit:literal $($rest:tt)*) => {
    $out.print($lit);
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr, #$member:ident $($rest:tt)*) => {
    Printable::print(&$node.$member, $ctx, $out);
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr,
    #($i:pat in #$member:ident: $($body:tt)*)* $($rest:tt)*
  ) => {
    for $i in &$node.$member {
      print_rule!($ctx, $node, $out, $($body)*);
    }
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr,
    #(($i:pat, $v:pat) in E(#$member:ident): $($body:tt)*)* $($rest:tt)*
  ) => {
    for ($i, $v) in $node.$member.iter().enumerate() {
      print_rule!($ctx, $node, $out, $($body)*);
    }
    print_rule!($ctx, $node, $out, $($rest)*);
  };

  ($ctx:expr, $node:expr, $out:expr,
    #($member:ident)* $($rest:tt)*
  ) => {
    for member in &$node.$member {
      $ctx.print_cld(member, $out);
    }
    print_rule!($ctx, $node, $out, $($rest)*);
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! print_rule_value {
  ($node:expr, (#$member:ident)) => {
    &$node.$member
  };

  ($node:expr, ($user_node:ident @ $value:expr)) => {
    { let $user_node = $node; $value }
  };

  ($node:expr, ($value:expr)) => {
    $value
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////
