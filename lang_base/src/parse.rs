use {
  crate::{ast, token},
  north_core::{
    NodeId,
    compiler::AspectPart,
  },
  north_parser::prelude::*,
  std::{
    fmt::Debug,
    marker::PhantomData,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct AssocRule<A> { assoc: PhantomData<A> }

impl<A> AssocRule<A> {
  fn new(_assoc: A) -> Self {
    Self { assoc: PhantomData }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

////////////////////////////////////////////////////////////////////////////////////////////////

/*struct ParseRuleToken<T, N> {
  token_type: PhantomData<T>,
  node: N
}

impl<T, N> ParseRuleToken<T, N> {
  fn new(node: N) -> Self {
    Self { token_type: PhantomData, node }
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::Attrs> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::Attrs>> {
    let _     = p.expect::<token::OpHash>()?;
    let _     = p.expect::<token::OpBracketL>()?;
    let attrs = p.parse_list_sep::<dyn ast::Attr, token::OpComma>()?;
    let _     = p.expect::<token::OpBracketR>()?;
    p.build(ast::Attrs { attrs })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::DeclLet> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::DeclLet>> {
    let _     = p.expect::<token::KwLet>()?;
    let var   = p.expect::<token::Ident>()?;
    let ty    = match p.accept::<token::OpColon>() {
      Some(_) => Some(p.parse::<dyn ast::Type>()?),
      None => None,
    };
    let init  = match p.accept::<token::OpEq>() {
      Some(_) => Some(p.parse::<dyn ast::Expr>()?),
      None => None,
    };
    let _     = p.expect::<token::OpSemicolon>()?;
    p.build(ast::DeclLet { var, ty, init })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::DeclRuleParam> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::DeclRuleParam>> {
    let var   = p.expect::<token::Ident>()?;
    let _     = p.expect::<token::OpColon>()?;
    let ty    = p.parse::<dyn ast::Type>()?;
    p.build(ast::DeclRuleParam { var, ty })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::DeclRuleParams> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::DeclRuleParams>> {
    let _       = p.expect::<token::OpParenL>()?;
    let params  = p.parse_list_sep::<ast::DeclRuleParam, token::OpComma>()?;
    let _       = p.expect::<token::OpParenR>()?;
    p.build(ast::DeclRuleParams { params })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprBlock> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprBlock>> {
    let _     = p.expect::<token::OpBlockStart>()?;
    let elems = p.parse_list::<dyn ast::Stmt, token::OpBlockEnd>()?;
    let _     = p.expect::<token::OpBlockEnd>()?;
    p.build(ast::ExprBlock { elems })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprBreak> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprBreak>> {
    let _     = p.expect::<token::KwBreak>()?;
    let value = p.parse::<dyn ast::Expr>().ok();
    p.build(ast::ExprBreak { value })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprCall> for DefaultRule {
  const LEFT_RECURSIVE: bool = true;

  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprCall>> {
    let target  = p.parse::<dyn ast::Expr>()?;
    let _       = p.expect::<token::OpParenL>()?;
    let args    = p.parse_list_sep::<dyn ast::Expr, token::OpComma>()?;
    let _       = p.expect::<token::OpParenR>()?;
    p.build(ast::ExprCall { target, args })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprContinue> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprContinue>> {
    let _ = p.expect::<token::KwContinue>()?;
    p.build(ast::ExprContinue { })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprIf> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprIf>> {
    let _     = p.expect::<token::KwIf>()?;
    let cond  = p.parse::<dyn ast::Expr>()?;
    let body1 = p.parse::<ast::ExprBlock>()?;
    let body0 = match p.accept::<token::KwElse>() {
      Some(_) => Some(p.parse::<ast::ExprBlock>()?),
      None => None,
    };

    p.build(ast::ExprIf {
      cond,
      body1,
      body0,
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<L: Clone + Debug> Parse<ast::ExprLit<L>> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprLit<L>>> {
    let lit = p.expect::<L>()?;
    p.build(ast::ExprLit { lit })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprLoop> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprLoop>> {
    let _     = p.expect::<token::KwLoop>()?;
    let body  = p.parse::<ast::ExprBlock>()?;
    p.build(ast::ExprLoop { body })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O: Clone + Debug> Parse<ast::ExprOpBin<O>> for AssocRule<LeftAssoc> {
  const LEFT_RECURSIVE: bool = true;

  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprOpBin<O>>> {
    let left  = p.parse_rec::<dyn ast::Expr>(0)?;
    let op    = p.expect::<O>()?;
    let right = p.parse_rec::<dyn ast::Expr>(1)?;
    p.build(ast::ExprOpBin { op, left, right })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<O: Clone + Debug> Parse<ast::ExprOpBin<O>> for AssocRule<RightAssoc> {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprOpBin<O>>> {
    let left  = p.parse_rec::<dyn ast::Expr>(1)?;
    let op    = p.expect::<O>()?;
    let right = p.parse_rec::<dyn ast::Expr>(0)?;
    p.build(ast::ExprOpBin { op, left, right })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprReturn> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprReturn>> {
    let _     = p.expect::<token::KwReturn>()?;
    let value = p.parse::<dyn ast::Expr>().ok();
    p.build(ast::ExprReturn { value })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprVar> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprVar>> {
    let var   = p.expect::<token::Ident>()?;
    p.build(ast::ExprVar { var: var.into() })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ExprWhile> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ExprWhile>> {
    let _     = p.expect::<token::KwWhile>()?;
    let cond  = p.parse::<dyn ast::Expr>()?;
    let body  = p.parse::<ast::ExprBlock>()?;
    p.build(ast::ExprWhile { cond, body })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::Items> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::Items>> {
    let mut items = Vec::<NodeId<dyn ast::Item>>::new();

    loop {
      if p.peek::<token::EOF>() { break; }
      items.push(p.parse::<dyn ast::Item>()?);
    }

    p.build(ast::Items { items })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ItemBuiltins> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ItemBuiltins>> {
    let _ = p.expect::<token::KwBuiltins>()?;
    let node = ast::ItemBuiltins::new(p.model_mut());
    p.build(node)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::ItemFn> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::ItemFn>> {
    let _       = p.expect::<token::KwFn>()?;
    let name    = p.expect::<token::Ident>()?;
    let params  = p.parse::<ast::DeclRuleParams>()?;
    let ret_ty  = match p.accept::<token::OpMinusGt>() {
      Some(_) => Some(p.parse::<dyn ast::Type>()?),
      None => None,
    };
    let body    = p.parse::<ast::ExprBlock>()?;
    p.build(ast::ItemFn { name, params, ret_ty, body })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::LitStr> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::LitStr>> {
    let token = p.expect::<String>()?;
    p.build(ast::LitStr { token })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::StmtDecl> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::StmtDecl>> {
    let decl  = p.parse::<dyn ast::Decl>()?;
    p.build(ast::StmtDecl { decl })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::StmtExpr> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::StmtExpr>> {
    let expr  = p.parse::<dyn ast::Expr>()?;
    let semi  = p.accept::<token::OpSemicolon>();
    p.build(ast::StmtExpr { expr, semi })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::StmtItem> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::StmtItem>> {
    let item  = p.parse::<dyn ast::Item>()?;
    p.build(ast::StmtItem { item })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<T: Clone + Debug> Parse<ast::TypePrim<T>> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::TypePrim<T>>> {
    let token = p.expect::<T>()?;
    p.build(ast::TypePrim { token })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Parse<ast::TypeVar> for DefaultRule {
  fn parse(&self, p: &mut ParseCtx) -> Result<NodeId<ast::TypeVar>> {
    let var = p.expect::<token::Ident>()?;
    p.build(ast::TypeVar { var: var.into() })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

parse_part! {
  ParsePart {
    dyn ast::Decl => {
      ast::DeclLet          => 100; DefaultRule,
    },

    dyn ast::Expr => {
      ast::ExprBlock        => 100; DefaultRule,
      ast::ExprBreak        => 100; DefaultRule,
      ast::ExprContinue     => 100; DefaultRule,
      ast::ExprIf           => 100; DefaultRule,
      ast::ExprLitInt       => 100; DefaultRule,
      ast::ExprLoop         => 100; DefaultRule,
      ast::ExprReturn       => 100; DefaultRule,
      ast::ExprVar          => 100; DefaultRule,
      ast::ExprWhile        => 100; DefaultRule,

      ast::ExprCall         =>  95; DefaultRule,

      ast::ExprOpAriDiv     =>  90; AssocRule::new(LeftAssoc),
      ast::ExprOpAriMod     =>  90; AssocRule::new(LeftAssoc),
      ast::ExprOpAriMul     =>  90; AssocRule::new(LeftAssoc),

      ast::ExprOpAriAdd     =>  80; AssocRule::new(LeftAssoc),
      ast::ExprOpAriSub     =>  80; AssocRule::new(LeftAssoc),

      ast::ExprOpBitShl     =>  75; AssocRule::new(LeftAssoc),
      ast::ExprOpBitShr     =>  75; AssocRule::new(LeftAssoc),

      ast::ExprOpBitAnd     =>  70; AssocRule::new(LeftAssoc),
      ast::ExprOpBitXor     =>  65; AssocRule::new(LeftAssoc),
      ast::ExprOpBitOr      =>  60; AssocRule::new(LeftAssoc),

      ast::ExprOpCmpEq      =>  50; AssocRule::new(LeftAssoc),
      ast::ExprOpCmpGt      =>  50; AssocRule::new(LeftAssoc),
      ast::ExprOpCmpGtEq    =>  50; AssocRule::new(LeftAssoc),
      ast::ExprOpCmpLt      =>  50; AssocRule::new(LeftAssoc),
      ast::ExprOpCmpLtEq    =>  50; AssocRule::new(LeftAssoc),
      ast::ExprOpCmpNotEq   =>  50; AssocRule::new(LeftAssoc),

      ast::ExprOpLogAnd     =>  40; AssocRule::new(LeftAssoc),
      ast::ExprOpLogOr      =>  35; AssocRule::new(LeftAssoc),

      ast::ExprOpAriAddSet  =>  30; AssocRule::new(RightAssoc),
      ast::ExprOpAriDivSet  =>  30; AssocRule::new(RightAssoc),
      ast::ExprOpAriModSet  =>  30; AssocRule::new(RightAssoc),
      ast::ExprOpAriMulSet  =>  30; AssocRule::new(RightAssoc),
      ast::ExprOpAriSet     =>  30; AssocRule::new(RightAssoc),
      ast::ExprOpAriSubSet  =>  30; AssocRule::new(RightAssoc),
    },

    dyn ast::Item => {
      ast::ItemBuiltins     => 100; DefaultRule,
      ast::ItemFn           => 100; DefaultRule,
    },

    dyn ast::Stmt => {
      ast::StmtDecl         => 100; DefaultRule,
      ast::StmtExpr         => 100; DefaultRule,
      ast::StmtItem         => 100; DefaultRule,
    },

    dyn ast::Type => {
      ast::TypeVar          => 100; DefaultRule,
    },

    ast::Attrs          => (DefaultRule),
    ast::DeclRuleParam  => (DefaultRule),
    ast::DeclRuleParams => (DefaultRule),
    ast::ExprBlock      => (DefaultRule),
    ast::Items          => (DefaultRule),
    ast::ItemFn         => (DefaultRule),
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
