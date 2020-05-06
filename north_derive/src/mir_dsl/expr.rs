use {
  mir_dsl::{mac, Macro, Punctuated, Quote, ReduceResult, Type},
  proc_macro2::{TokenStream},
  quote::{ToTokens},
  syn::{
    self,
    buffer::Cursor,
    spanned::Spanned,
    synom::{PResult, Synom},
    token,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Case {
  key: Expr,
  arrow_token: Token![=>],
  value: Expr,
}

impl Case {
  pub fn reduce(&self, ctx: ExprCtx, sb: &syn::Ident) -> TokenStream {
    let key = self.key.reduce(ctx);
    let value = self.value.reduce(ctx);
    quote! { #sb.add_case(#key, #value); }
  }
}

impl Synom for Case {
  named!(parse -> Self, do_parse!(
    key         : syn!(Expr) >>
    arrow_token : punct!(=>) >>
    value       : syn!(Expr) >>
    (Self { key, arrow_token, value })
  ));
}

impl ToTokens for Case {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.key.to_tokens(tokens);
    self.arrow_token.to_tokens(tokens);
    self.value.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ExprCtx<'a> = (&'a syn::Ident, &'a syn::Ident, &'a syn::Ident);

pub enum Expr {
  AddrOf(ExprAddrOf),
  Call(ExprCall),
  Field(ExprField),
  Ident(syn::Ident),
  Lit(ExprLit),
  Macro(ExprMacro),
  Match(ExprMatch),
  Quote(ExprQuote),
  Return(ExprReturn),
  Tuple(ExprTuple),
  Type(ExprType),
}

impl Expr {
  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    match self {
      Expr::AddrOf(e) => e.reduce(ctx),
      Expr::Call(e) => e.reduce(ctx),
      Expr::Field(e) => e.reduce(ctx),
      Expr::Ident(e) => {
        let span = e.span();
        quote_spanned! { span => #e.clone() }
      },
      Expr::Lit(e) => e.reduce_const(ctx.0),
      Expr::Macro(e) => e.reduce(ctx),
      Expr::Match(e) => e.reduce(ctx),
      Expr::Quote(e) => e.reduce_const(ctx.0),
      Expr::Return(e) => e.reduce(ctx),
      Expr::Tuple(e) => e.reduce(ctx),
      Expr::Type(e) => e.reduce_const(ctx.0),
    }
  }

  pub fn reduce_const(&self, b: &syn::Ident) -> TokenStream {
    match self {
      Expr::Ident(e) => {
        let span = e.span();
        quote_spanned! { span => #e.clone() } 
      },
      Expr::Lit(e) => e.reduce_const(b),
      Expr::Macro(e) => e.reduce_const(b),
      Expr::Quote(e) => e.reduce_const(b),
      Expr::Tuple(e) => e.reduce_const(b),
      Expr::Type(e) => e.reduce_const(b),
      _ => panic!("invalid const expr"),
    }
  }

  pub fn reduce_lval(&self, ctx: ExprCtx) -> TokenStream {
    match self {
      Expr::Field(e) => e.reduce_lval(ctx),
      _ => panic!("invalid lval expr"),
    }
  }
}

impl Synom for Expr {
  named!(parse -> Self, alt!(
    syn!(ExprAddrOf) => {|e| Expr::AddrOf(e)} |
    call!(member_expr)
  ));
}

impl ToTokens for Expr {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match self {
      Expr::AddrOf(e) => e.to_tokens(tokens),
      Expr::Call(e) => e.to_tokens(tokens),
      Expr::Field(e) => e.to_tokens(tokens),
      Expr::Ident(e) => e.to_tokens(tokens),
      Expr::Lit(e) => e.to_tokens(tokens),
      Expr::Macro(e) => e.to_tokens(tokens),
      Expr::Match(e) => e.to_tokens(tokens),
      Expr::Quote(e) => e.to_tokens(tokens),
      Expr::Return(e) => e.to_tokens(tokens),
      Expr::Tuple(e) => e.to_tokens(tokens),
      Expr::Type(e) => e.to_tokens(tokens),
    }
  }
}

named!(member_expr -> Expr, do_parse!(
  mut expr: call!(bottom_expr) >>
  many0!(alt!(
    tap!(field: and_field => {
      let (dot_token, member) = field;
      let inner = ExprField { base: box expr, dot_token, member };
      expr = Expr::Field(inner);
    }) 
    |
    tap!(args: and_call => {
      let inner = ExprCall { func: box expr, args };
      expr = Expr::Call(inner);
    })
  )) >> (expr)
));

named!(and_call -> (Punctuated<Expr, Token![,]>), 
  map!(parens!(syn!(Punctuated<Expr, Token![,]>)), |r| r.1)
);

named!(and_field -> (Token![.], syn::Member), 
  tuple!(punct!(.), syn!(syn::Member))
);

named!(bottom_expr -> Expr, alt!(
  syn!(ExprLit) => {|e| Expr::Lit(e)} |
  syn!(ExprMacro) => {|e| Expr::Macro(e)} |
  syn!(ExprMatch) => {|e| Expr::Match(e)} |
  syn!(ExprQuote) => {|e| Expr::Quote(e)} |
  syn!(ExprReturn) => {|e| Expr::Return(e)} |
  syn!(ExprTuple) => {|e| Expr::Tuple(e)} |
  syn!(ExprType) => {|e| Expr::Type(e)} |
  syn!(syn::Ident) => {|e| Expr::Ident(e)}
));

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprAddrOf {
  pub and_token: Token![&],
  pub mutability: Option<Token![mut]>,
  pub expr: Box<Expr>,
}

impl ExprAddrOf {
  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    let expr = self.expr.reduce_lval(ctx);
    quote! { #expr }
  }
}

impl Synom for ExprAddrOf {
  named!(parse -> Self, do_parse!(
    and_token   : punct!(&) >>
    mutability  : option!(keyword!(mut)) >>
    expr        : syn!(Expr) >>
    (Self { and_token, mutability, expr: box expr })
  ));
}

impl ToTokens for ExprAddrOf {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.and_token.to_tokens(tokens);
    self.mutability.to_tokens(tokens);
    self.expr.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

// pub struct ExprBlock {
//   pub stmts: Seq<Stmt>,
// }

// impl ExprBlock {
//   pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
//     let (b, _, _) = ctx;

//     let stmts = self.stmts.reduce(b, 
//       &|stmt| stmt.reduce(ctx),
//       &|_| quote! { XXX }
//     );

//     quote! { #stmts }
//   }
// }

// impl Synom for ExprBlock {
//   named!(parse -> Self, do_parse!(
//     stmts: braces!(syn!(Seq<Stmt>)) >>
//     (Self { stmts: stmts.1 })
//   ));
// }

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprCall {
  pub func: Box<Expr>,
  pub args: Punctuated<Expr, Token![,]>,
}

impl ExprCall {
  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let func = self.func.reduce(ctx);
    let args = self.args.iter()
      .map(|a| a.unwrap_normal())
      .map(|a| a.reduce(ctx));
    quote_spanned! { span => {
      let mut __cb = #bb.stmt_call(#func);
      #(__cb.add_arg(#args);)*
      __cb.complete(&mut #bb)
    }}
  }
}

impl ToTokens for ExprCall {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.func.to_tokens(tokens);
    self.args.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

// LEFT_RECURSION
pub struct ExprField {
  pub base: Box<Expr>,
  pub dot_token: Token![.],
  pub member: syn::Member,
}

impl ExprField {
  pub fn index(&self) -> &syn::Index {
    match self.member {
      syn::Member::Named(_) => panic!("invalid named member"),
      syn::Member::Unnamed(ref index) => index,
    }
  }

  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let base = self.base.reduce(ctx);
    let index = self.index();
    quote_spanned! { span => #bb.stmt_field(#base, #index) }
  }

  pub fn reduce_lval(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let base = self.base.reduce(ctx);
    let index = self.index();
    quote_spanned! { span => #bb.lval_field(#base, #index) }
  }
}

impl ToTokens for ExprField {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.base.to_tokens(tokens);
    self.dot_token.to_tokens(tokens);
    self.member.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprLit {
  pub lit: syn::ExprLit
}

impl ExprLit {
  pub fn reduce_const(&self, b: &syn::Ident) -> TokenStream {
    let span = self.span();
    let lit = &self.lit;
    quote_spanned! { span => #b.quote_const(#lit) }
  }
}

impl Synom for ExprLit {
  named!(parse -> Self, 
    map!(syn!(syn::ExprLit), |lit| ExprLit { lit })
  );
}

impl ToTokens for ExprLit {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.lit.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprMacro {
  pub name: syn::Ident,
  pub bang_token: Token![!],
  pub mac: Box<dyn Macro>,
}

impl ExprMacro {
  pub fn parse<'a>(cursor: Cursor<'a>, name: &str) -> PResult<'a, Box<dyn Macro>> {
    match name {
      "builtin"   => mac::MacroBuiltin::parse(cursor),
      _           => mac::MacroGeneric::parse(cursor),
    }
  }

  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    self.mac.reduce(&self.name, ctx)
  }

  pub fn reduce_const(&self, b: &syn::Ident) -> TokenStream {
    self.mac.reduce_const(&self.name, b)
  }
}

impl Synom for ExprMacro {
  named!(parse -> Self, do_parse!(
    name        : syn!(syn::Ident) >>
    bang_token  : punct!(!) >>
    mac         : parens!(call!(ExprMacro::parse, name.to_string().as_ref())) >>
    (Self { name, bang_token, mac: mac.1 })
  ));
}

impl ToTokens for ExprMacro {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.name.to_tokens(tokens);
    self.bang_token.to_tokens(tokens);
    self.mac.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprMatch {
  pub match_token   : Result<Token![match], syn::Ident>,
  pub key           : Box<Expr>,
  pub default_token : Token![default],
  pub arrow_token   : Token![=>],
  pub default_case  : Box<Expr>,
  pub comma_token   : Token![,],
  pub cases         : Punctuated<Case, Token![,]>
}

impl ExprMatch {
  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    let (b, fb, bb) = ctx;
    let span = self.span();
    let sb = syn::Ident::new("__sb", span);
    let key = self.key.reduce(ctx);

    let switch_type = match self.match_token {
      Ok(_) => quote! { ctl_switch },
      Err(_) => quote! { ctl_switch_range },
    };

    let default_case = self.default_case.reduce(ctx);
    let cases = self.cases.reduce(b, 
      &|case| {
        let case = case.reduce(ctx, &sb);
        ReduceResult::new(case)
      },
      &|multi| quote! {
        #sb.add_cases(#multi);
      }
    );

    quote_spanned! { span => {
      let mut #sb = #bb.#switch_type(#key, #default_case);
      #cases
      #sb.complete(&mut #fb, &mut #bb)
    }}
  }
}

impl Synom for ExprMatch {
  named!(parse -> Self, do_parse!(
    match_token: alt!(
      keyword!(match) => {|t| Ok(t)}
      |
      custom_keyword!(match_range) => {|t| Err(t)}
    ) >>
    key: syn!(Expr) >>
    other: braces!(tuple!(
      keyword!(default),
      punct!(=>),
      syn!(Expr),
      punct!(,),
      syn!(Punctuated<Case, Token![,]>)
    )) >>
    ({
      let other = other.1;
      Self { 
        match_token, 
        key: box key,
        default_token: other.0,
        arrow_token: other.1,
        default_case: box other.2,
        comma_token: other.3,
        cases: other.4,
      }
    })
  ));
}

impl ToTokens for ExprMatch {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    //self.match_token.to_tokens(tokens);
    self.key.to_tokens(tokens);
    self.default_token.to_tokens(tokens);
    self.arrow_token.to_tokens(tokens);
    self.default_case.to_tokens(tokens);
    self.comma_token.to_tokens(tokens);
    self.cases.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprQuote {
  pub hash: Token![#],
  pub quote: Quote,
}

impl ExprQuote {
  pub fn reduce_const(&self, b: &syn::Ident) -> TokenStream {
    match &self.quote {
      Quote::Expr(q) => {
        let expr = &q.expr;
        let span = self.span();
        quote_spanned! { span => #b.quote_const(#expr) }
      },
      Quote::Block(q) => {
        let block = &q.block;
        quote! { #block }
      },
      Quote::Ident(q) => {
        let result = q.reduce(b);
        quote! { #result }
      }
    }
  }
}

impl Synom for ExprQuote {
  named!(parse -> Self, do_parse!(
    hash: punct!(#) >>
    quote: syn!(Quote) >>
    (Self { hash, quote })
  ));
}

impl ToTokens for ExprQuote {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.hash.to_tokens(tokens);
    self.quote.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprReturn {
  pub return_token: Token![return],
  pub value: Option<Box<Expr>>,
}

impl ExprReturn {
  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let value = match self.value {
      Some(ref value) => {
        let value = value.reduce(ctx);
        quote! { Some(#value) }
      },

      None => {
        quote! { None }
      }
    };

    quote! { 
      #bb.ctl_ret(#value)
    }
  }
}

impl Synom for ExprReturn {
  named!(parse -> Self, do_parse!(
    return_token: keyword!(return) >>
    value: option!(map!(syn!(Expr), Box::new)) >>
    (Self { return_token, value })
  ));
}

impl ToTokens for ExprReturn {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.return_token.to_tokens(tokens);
    self.value.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprTuple {
  pub ty: Option<Type>,
  pub parens: token::Paren,
  pub elements: Punctuated<Expr, Token![,]>,
}

impl ExprTuple {
  pub fn reduce(&self, ctx: ExprCtx) -> TokenStream {
    let (b, _, bb) = ctx;
    let span = self.span();

    let ty = match &self.ty {
      Some(ty) => { let r = ty.reduce(b); quote! { Some(#r) } },
      None => quote! { None },
    };

    let elements = self.elements.reduce(b, 
      &|elem| {
        let elem = elem.reduce(ctx);
        let elem = quote! { __sb.add_element(#elem); };
        ReduceResult::new(elem)
      },
      &|multi| quote!{
        __sb.add_elements(#multi);
      }
    );

    quote_spanned! { span => {
      let mut __sb = #bb.stmt_struct(#ty);
      #elements
      __sb.complete(&mut #bb)
    }}
  }

  pub fn reduce_const(&self, b: &syn::Ident) -> TokenStream {
    let span = self.span();
    let elements = self.elements.iter()
      .map(|e| e.unwrap_normal())
      .map(|e| e.reduce_const(b));

    quote_spanned! { span => {
      let mut __tb = #b.const_tuple();
      #(__tb.add_element(#elements);)*
      __tb.complete()
    }}
  }
}

impl Synom for ExprTuple {
  named!(parse -> Self, do_parse!(
    ty: option!(tuple!(punct!(<), syn!(Type), punct!(>))) >>
    elements : parens!(syn!(Punctuated<Expr, Token![,]>)) >>
    (Self { 
      ty: ty.map(|t| t.1),
      parens: elements.0, 
      elements: elements.1 
    })
  ));
}

impl ToTokens for ExprTuple {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    //tokens.append(self.parens.clone());
    self.elements.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ExprType {
  pub ty: Type,
}

impl ExprType {
  pub fn reduce_const(&self, b: &syn::Ident) -> TokenStream {
    self.ty.reduce(b)
  }
}

impl Synom for ExprType {
  named!(parse -> Self, do_parse!(
    ty: tuple!(punct!(<), syn!(Type), punct!(>)) >>
    (Self { ty: ty.1 })
  ));
}

impl ToTokens for ExprType {
  fn to_tokens(&self, _tokens: &mut TokenStream) {
    //self.ty.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
