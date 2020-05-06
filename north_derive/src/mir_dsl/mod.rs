use {
  proc_macro2::{Span, TokenStream},
  quote::ToTokens,
  std::{
    marker::PhantomData,
    slice
  },
  syn::{
    self,
    buffer::Cursor,
    punctuated::{Punctuated as SynPunctuated},
    synom::{PResult, Synom},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod expr;
pub mod item;
pub mod mac;
pub mod reduce_result;
pub mod ty;

pub use self::{
  expr::*,
  item::*,
  mac::*,
  reduce_result::ReduceResult,
  ty::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Reduce {
  fn reduce(&self, b: &syn::Ident) -> TokenStream;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Block {
  pub name: Ident,
  pub colon_token: Token![:],
  pub stmts: Seq<Stmt>,
}

impl Block {
  pub fn ident(&self) -> &syn::Ident {
    self.name.to_ident()
  }

  pub fn reduce(&self, b: &syn::Ident, fb: &syn::Ident) -> ReduceResult {
    let name = &self.name;
    let (builder, name_lit) = name.builder_lit();
    let stmts = self.stmts.reduce(b, 
      &|stmt| stmt.reduce((b, fb, &builder)),
      &|_| quote! { XXX }
    );

    let init = match name.is_curr() {
      true => quote! { #fb.build_block_resume(curr_block) },
      false => quote! { #fb.build_block(#name_lit) },
    };
  
    ReduceResult::new_pair(
      quote! { #[allow(non_snake_case)] let (#name, mut #builder) = #init; },
      quote! { #stmts }
    )
  }
}

impl Synom for Block {
  named!(parse -> Self, do_parse!(
    name        : syn!(Ident) >>
    colon_token : punct!(:) >>
    stmts       : braces!(syn!(Seq<Stmt>)) >>
    (Self { name, colon_token, stmts: stmts.1 })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Elem<E, S> {
  For(ElemFor<E, S>),
  If(ElemIf<E, S>),
  Normal(E),
  Quote(ElemQuote),
}

impl<E, S> Elem<E, S> {
  pub fn reduce<A, M>(&self, b: &syn::Ident, single: &A, multi: &M) -> ReduceResult where
    A: Fn(&E) -> ReduceResult,
    M: Fn(TokenStream) -> TokenStream,
  {
    match self {
      Elem::For(e) => e.reduce(b, single, multi),
      Elem::If(e) => e.reduce(b, single, multi),
      Elem::Normal(e) => single(e),
      Elem::Quote(q) => match &q.quote {
        Quote::Block(q) => q.reduce(b),
        Quote::Expr(q) => q.reduce_multi(b, multi),
        Quote::Ident(q) => q.reduce(b),
      },
    }
  }

  pub fn unwrap_normal(&self) -> &E {
    match self {
      Elem::For(_) =>  panic!("invalid unwrap"),
      Elem::If(_) =>  panic!("invalid unwrap"),
      Elem::Normal(e) => e,
      Elem::Quote(_) => panic!("invalid unwrap")
    }
  }
}

impl<E: Synom, S: Synom> Synom for Elem<E, S> {
  named!(parse -> Self, alt!(
    syn!(ElemFor<E, S>) => {|e| Elem::For(e)} 
    |
    syn!(ElemIf<E, S>) => {|e| Elem::If(e)} 
    |
    syn!(ElemQuote) => {|e| Elem::Quote(e)} 
    |
    syn!(E) => {|e| Elem::Normal(e)}
  ));
}

impl<E: ToTokens, S: ToTokens> ToTokens for Elem<E, S> {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match self {
      Elem::For(e) => e.to_tokens(tokens),
      Elem::If(e) => e.to_tokens(tokens),
      Elem::Normal(e) => e.to_tokens(tokens),
      Elem::Quote(q) => q.to_tokens(tokens),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ElemFor<E, S> {
  pub hash_token_0 : Token![#],
  pub for_token    : Token![for],
  pub hash_token_1 : Token![#],
  pub pat          : Box<syn::Pat>,
  pub in_token     : Token![in],
  pub expr         : Box<syn::Expr>,
  pub body         : Punctuated<E, S>,
}

impl<E, S> ElemFor<E, S> {
  pub fn reduce<A, M>(&self, b: &syn::Ident, single: &A, multi: &M) -> ReduceResult where
    A: Fn(&E) -> ReduceResult,
    M: Fn(TokenStream) -> TokenStream,
  {
    let pat = &self.pat;
    let expr = &self.expr;
    let body = self.body.reduce(b, single, multi);

    ReduceResult::new(quote! { 
      for #pat in #expr { #body }
    })
  }
}

impl<E, S> Synom for ElemFor<E, S> where
  E: Synom, S: Synom,
{
  named!(parse -> Self, do_parse!(
    hash_token_0 : punct!(#) >>
    for_token    : keyword!(for) >>
    hash_token_1 : punct!(#) >>
    pat          : map!(syn!(syn::Pat), |p| box p) >>
    in_token     : keyword!(in) >>
    expr         : map!(syn!(syn::Expr), |e| box e) >>
    body         : map!(braces!(syn!(Punctuated<E, S>)), |e| e.1) >>
    (Self { 
      hash_token_0,
      for_token,
      hash_token_1,
      pat,
      in_token,
      expr,
      body,
    })
  ));
}

impl<E, S> ToTokens for ElemFor<E, S> where
  E: ToTokens, S: ToTokens,
{
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.hash_token_0.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ElemIf<E, S> {
  pub hash_token_0 : Token![#],
  pub if_token     : Token![if],
  pub hash_token_1 : Token![#],
  pub let_token    : Token![let],
  pub pats         : SynPunctuated<syn::Pat, Token![|]>,
  pub eq_token     : Token![=],
  pub expr         : Box<syn::Expr>,
  pub then_branch  : Punctuated<E, S>,
  pub else_branch  : Option<(Token![else], Punctuated<E, S>)>,
}

impl<E, S> ElemIf<E, S> {
  pub fn reduce<A, M>(&self, b: &syn::Ident, single: &A, multi: &M) -> ReduceResult where
    A: Fn(&E) -> ReduceResult,
    M: Fn(TokenStream) -> TokenStream,
  {
    let pats = &self.pats;
    let expr = &self.expr;
    let then_branch = self.then_branch.reduce(b, single, multi);
    let else_branch = self.else_branch.as_ref().map(|e| {
      let block = e.1.reduce(b, single, multi);
      quote! { else { #block } }
    });

    ReduceResult::new(quote! { 
      if let #pats = #expr { #then_branch } #else_branch
    })
  }
}

impl<E, S> Synom for ElemIf<E, S> where
  E: Synom, S: Synom,
{
  named!(parse -> Self, do_parse!(
    hash_token_0 : punct!(#) >>
    if_token     : keyword!(if) >>
    hash_token_1 : punct!(#) >>
    let_token    : keyword!(let) >>
    pats         : call!(SynPunctuated::<syn::Pat, Token![|]>::parse_separated) >>
    eq_token     : punct!(=) >>
    expr         : map!(syn!(syn::Expr), |e| box e) >>
    then_branch  : braces!(syn!(Punctuated<E, S>)) >>
    else_branch  : option!(tuple!(
      keyword!(else),
      map!(braces!(syn!(Punctuated<E, S>)), |e| e.1)
    )) >>
    (Self { 
      hash_token_0,
      if_token,
      hash_token_1,
      let_token,
      pats,
      eq_token,
      expr,
      then_branch: then_branch.1,
      else_branch
    })
  ));
}

impl<E, S> ToTokens for ElemIf<E, S> where
  E: ToTokens, S: ToTokens,
{
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.hash_token_0.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ElemQuote {
  pub hash_token: Token![#],
  pub quote: Quote,
  pub star_token: Token![*],
}

// impl ElemQuote {
//   pub fn reduce(&self, b: &syn::Ident) -> ReduceResult {
//     self.quote.reduce(b)
//   }
// }

impl Synom for ElemQuote {
  named!(parse -> Self, do_parse!(
    hash_token  : punct!(#) >>
    quote       : syn!(Quote) >>
    star_token  : punct!(*) >>
    (Self { hash_token, quote, star_token })
  ));
}

impl ToTokens for ElemQuote {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.hash_token.to_tokens(tokens);
    self.quote.to_tokens(tokens);
    self.star_token.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Epsilon;

impl Synom for Epsilon {
  named!(parse -> Self, map!(
    epsilon!(),
    |_| Epsilon
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct FnBody {
  pub blocks: Seq<Block>,
}

impl FnBody {
  pub fn last_block_ident(&self) -> &syn::Ident {
    let block = self.blocks.last_normal().unwrap();
    block.ident()
  }

  pub fn reduce(&self, b: &syn::Ident, fb: &syn::Ident) -> TokenStream {
    let blocks = self.blocks.reduce(b, 
      &|block| block.reduce(b, fb),
      &|_| quote! { YYY }
    );
    quote! { #blocks }
  }
}

impl Synom for FnBody {
  named!(parse -> Self, do_parse!(
    blocks: syn!(Seq<Block>) >>
    (Self { blocks })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct FnParam {
  pub mut_token   : Option<Token![mut]>,
  pub name        : Ident,
  pub colon_token : Token![:],
  pub param_type  : Type,
}

impl FnParam {
  pub fn reduce(&self, b: &syn::Ident, fb: &syn::Ident) -> TokenStream {
    let name = &self.name;
    let name_lit = name.to_lit();
    let ty = self.param_type.reduce(b);

    quote! {
      #fb.add_param(#name_lit, #ty);
    }
  }

  pub fn reduce_late(
    &self, _b: &syn::Ident, fb: &syn::Ident, idx: usize
  ) -> TokenStream {
    let name = &self.name;

    quote! {
      let #name = #fb.get_param(#idx);
    }
  }
}

impl Synom for FnParam {
  named!(parse -> Self, do_parse!(
    mut_token   : option!(keyword!(mut)) >>
    name        : syn!(Ident) >>
    colon_token : punct!(:) >>
    param_type  : syn!(Type) >>
    (Self { mut_token, name, colon_token, param_type })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Ident {
  Normal(syn::Ident),
  Quote(IdentQuote),
  Resume(IdentResume),
}

impl Ident {
  pub fn builder_lit(&self) -> (syn::Ident, TokenStream) {
    let ident = match self {
      Ident::Normal(i) => &i,
      Ident::Quote(i) => &i.ident,
      Ident::Resume(_) => panic!("BAD"),
    };

    let span = self.span();
    let builder_str = format!("__b_{}", ident);
    let builder = syn::Ident::new(builder_str.as_ref(), span);

    let name_lit = match self {
      Ident::Normal(i) => {
        let name = i.to_string();
        let lit = syn::LitStr::new(name.as_ref(), span);
        quote! { #lit }
      },
      Ident::Quote(IdentQuote { expr, .. }) => {
        quote! { #expr }
      },
      Ident::Resume(_) => {
        panic!("BAD")
      },
    };

    (builder, name_lit)
  }

  pub fn is_curr(&self) -> bool {
    match self {
      Ident::Normal(ref ident) => ident.to_string().ends_with("curr"),
      Ident::Quote(_) => false,
      Ident::Resume(_) => false,
    }
  }

  pub fn span(&self) -> Span {
    match self {
      Ident::Normal(i) => i.span(),
      Ident::Quote(i) => i.ident.span(),
      Ident::Resume(i) => i.ident.span(),
    }
  }

  pub fn to_ident(&self) -> &syn::Ident {
    match self {
      Ident::Normal(ident) => ident,
      _ => panic!("not a normal identifier"),
    }
  }

  pub fn to_lit(&self) -> TokenStream {
    let name = self.to_string();
    let lit = syn::LitStr::new(name.as_ref(), self.span());
    quote! { #lit }
  }

  pub fn to_string(&self) -> String {
    match self {
      Ident::Normal(ident) => ident.to_string(),
      _ => unimplemented!(),
    }
  }
}

impl Synom for Ident {
  named!(parse -> Self, alt!(
    syn!(syn::Ident) => {|e| Ident::Normal(e)} 
    |
    syn!(IdentQuote) => {|e| Ident::Quote(e)}
    |
    syn!(IdentResume) => {|e| Ident::Resume(e)}
  ));
}

impl ToTokens for Ident {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match self {
      Ident::Normal(i) => i.to_tokens(tokens),
      Ident::Quote(i) => i.ident.to_tokens(tokens),
      _ => unimplemented!(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct IdentQuote {
  pub hash  : Token![#],
  pub ident : syn::Ident,
  pub expr  : syn::Expr,
}

impl Synom for IdentQuote {
  named!(parse -> Self, do_parse!(
    hash: punct!(#) >>
    tuple: parens!(tuple!(
      syn!(syn::Ident),
      punct!(:),
      syn!(syn::Expr)
    )) >>
    (Self { 
      hash, 
      ident: (tuple.1).0,
      expr: (tuple.1).2,
    })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct IdentResume {
  pub hash      : Token![#],
  pub at_token  : Token![@],
  pub ident     : syn::Ident,
}

impl Synom for IdentResume {
  named!(parse -> Self, do_parse!(
    hash      : punct!(#) >>
    at_token  : punct!(@) >>
    ident     : syn!(syn::Ident) >>
    (Self { hash, at_token, ident })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Quote {
  Block(QuoteBlock),
  Expr(QuoteExpr),
  Ident(QuoteIdent),
}

impl Quote {
  pub fn reduce(&self, b: &syn::Ident) -> ReduceResult {
    match self {
      Quote::Block(q) => q.reduce(b),
      Quote::Expr(q) => q.reduce(b),
      Quote::Ident(q) => q.reduce(b),
    }
  }
}

impl Synom for Quote {
  named!(parse -> Self, alt!(
    syn!(QuoteBlock) => {|q| Quote::Block(q)} 
    |
    syn!(QuoteExpr) => {|q| Quote::Expr(q)} 
    |
    syn!(QuoteIdent) => {|q| Quote::Ident(q)}
  ));
}

impl ToTokens for Quote {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match self {
      Quote::Block(q) => q.to_tokens(tokens),
      Quote::Expr(q) => q.to_tokens(tokens),
      Quote::Ident(q) => q.to_tokens(tokens),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct QuoteBlock {
  pub block: syn::ExprBlock,
}

impl QuoteBlock {
  pub fn reduce(&self, _b: &syn::Ident) -> ReduceResult {
    let stmts = &self.block.block.stmts;
    ReduceResult::new(quote! { #(#stmts)* })
  }
}

impl Synom for QuoteBlock {
  named!(parse -> Self, do_parse!(
    block: syn!(syn::ExprBlock) >>
    (Self { block })
  ));
}

impl ToTokens for QuoteBlock {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.block.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct QuoteExpr {
  pub bind: Option<syn::Ident>,
  pub expr: syn::Expr,
}

impl QuoteExpr {
  pub fn reduce(&self, _b: &syn::Ident) -> ReduceResult {
    let expr = &self.expr;
    ReduceResult::new(match self.bind {
      Some(ref var) => quote! { let #var = #expr; },
      None => quote! { #expr },
    })
  }

  pub fn reduce_multi<M>(&self, _b: &syn::Ident, multi: M) -> ReduceResult where
    M: FnOnce(TokenStream) -> TokenStream,
  {
    let expr = &self.expr;
    let expr = multi(quote! { #expr });
    ReduceResult::new(match self.bind {
      Some(ref var) => quote! { let #var = #expr; },
      None => quote! { #expr },
    })
  }
}

impl Synom for QuoteExpr {
  named!(parse -> Self, map!(
    parens!(tuple!(
      option!(tuple!(syn!(syn::Ident), punct!(:))),
      syn!(syn::Expr)
    )),
    |(_, r)| {
      let bind = r.0.map(|r| r.0);
      let expr = r.1;
      Self { bind, expr }
    }
  ));
}

impl ToTokens for QuoteExpr {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.bind.to_tokens(tokens);
    self.expr.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct QuoteIdent {
  pub ident: syn::Ident,
}

impl QuoteIdent {
  pub fn reduce(&self, _b: &syn::Ident) -> ReduceResult {
    let ident = &self.ident;
    ReduceResult::new(quote! { 
      #ident.clone() 
    })
  }
}

impl Synom for QuoteIdent {
  named!(parse -> Self, do_parse!(
    ident : syn!(syn::Ident) >>
    (Self { ident })
  ));
}

impl ToTokens for QuoteIdent {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.ident.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Punctuated<T, P> {
  pub elements: Vec<Elem<T, P>>,
  pub punct_type: PhantomData<P>,
}

impl<T, P> Punctuated<T, P> {
  pub fn iter(&self) -> slice::Iter<Elem<T, P>> {
    self.elements.iter()
  }

  pub fn is_empty(&self) -> bool {
    self.elements.is_empty()
  }

  pub fn reduce<R, M>(&self, b: &syn::Ident, single: &R, multi: &M) -> ReduceResult where
    R: Fn(&T) -> ReduceResult, 
    M: Fn(TokenStream) -> TokenStream,
  {
    let elements = self.iter().map(|e| e.reduce(b, single, multi));
    ReduceResult::new_multi(elements)
  }
}

impl<T, P> Synom for Punctuated<T, P> where
  T: Synom, P: Synom
{
  default fn parse(input: Cursor) -> PResult<Self> {
    SynPunctuated::<Elem<T, P>, P>::parse_terminated(input).map(|(elements, cursor)| {
      let value = Self { 
        elements: elements.into_pairs().map(|p| p.into_value()).collect(),
        punct_type: PhantomData,
      };
      (value, cursor)
    })
  }
}

impl<T> Synom for Punctuated<T, Epsilon> where
  T: Synom
{
  named!(parse -> Self, do_parse!(
    elements: many0!(syn!(Elem<T, Epsilon>)) >>
    (Self { 
      elements: elements,
      punct_type: PhantomData,
    })
  ));
}

impl<T, P> ToTokens for Punctuated<T, P> where
  T: ToTokens, P: ToTokens 
{
  fn to_tokens(&self, tokens: &mut TokenStream) {
    for elem in &self.elements { elem.to_tokens(tokens); }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Seq<E> {
  pub elements: Vec<Elem<E, Epsilon>>,
}

impl<E> Seq<E> {
  pub fn iter(&self) -> slice::Iter<Elem<E, Epsilon>> {
    self.elements.iter()
  }

  pub fn last_normal(&self) -> Option<&E> {
    match self.elements.last() {
      Some(Elem::Normal(e)) => Some(e),
      _ => None,
    }
  }

  pub fn reduce<S, M>(&self, b: &syn::Ident, single: &S, multi: &M) -> ReduceResult where
    S: Fn(&E) -> ReduceResult,
    M: Fn(TokenStream) -> TokenStream
  {
    let elements = self.iter().map(|e| e.reduce(b, single, multi));
    ReduceResult::new_multi(elements)
  }
}

impl<E: Synom> Synom for Seq<E> {
  named!(parse -> Self, do_parse!(
    elements : many0!(syn!(Elem<E, Epsilon>)) >>
    (Self { elements })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Stmt {
  Expr(StmtExpr),
  Item(StmtItem),
  Let(StmtLet),
}

impl Stmt {
  pub fn reduce(&self, ctx: ExprCtx) -> ReduceResult {
    match self {
      Stmt::Expr(s) => s.reduce(ctx),
      Stmt::Item(s) => s.reduce(ctx),
      Stmt::Let(s) => s.reduce(ctx),
    }
  }
}

impl Synom for Stmt {
  named!(parse -> Self, alt!(
    syn!(StmtExpr) => {|e| Stmt::Expr(e)} |
    syn!(StmtItem) => {|e| Stmt::Item(e)} |
    syn!(StmtLet) => {|e| Stmt::Let(e)}
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct StmtExpr {
  pub expr: Expr,
  pub semi_token: Token![;],
}

impl StmtExpr {
  pub fn reduce(&self, ctx: ExprCtx) -> ReduceResult {
    let expr = self.expr.reduce(ctx);
    let semi = &self.semi_token;
    ReduceResult::new(quote! { #expr #semi })
  }
}

impl Synom for StmtExpr {
  named!(parse -> Self, do_parse!(
    expr: syn!(Expr) >>
    semi_token: punct!(;) >>
    (Self { expr, semi_token })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct StmtItem {
  pub item: Item,
}

impl StmtItem {
  pub fn reduce(&self, ctx: ExprCtx) -> ReduceResult {
    let (b, _, _) = ctx;
    let (early, late) = self.item.reduce(b);
    ReduceResult::new_pair(early, late)
  }
}

impl Synom for StmtItem {
  named!(parse -> Self, do_parse!(
    item: syn!(Item) >>
    (Self { item })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct StmtLet {
  pub let_token: Token![let],
  pub name: Ident,
  pub assign_token: Token![=],
  pub init: Expr,
  pub semi_token: Token![;],
}

impl StmtLet {
  pub fn reduce(&self, ctx: ExprCtx) -> ReduceResult {
    let name = &self.name;
    let init = self.init.reduce(ctx);
    ReduceResult::new(quote! { let #name = #init; })
  }
}

impl Synom for StmtLet {
  named!(parse -> Self, do_parse!(
    let_token: keyword!(let) >>
    name: syn!(Ident) >>
    assign_token: punct!(=) >>
    init: syn!(Expr) >>
    semi_token: punct!(;) >>
    (Self { let_token, name, assign_token, init, semi_token })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn output_ts(name: &str, s: TokenStream) {
  use std::{
    io::Write,
    fs::OpenOptions
  };
  let mut file = OpenOptions::new()
    .append(true)
    .create(true)
    .open(name)
    .expect("can't open macro file");
  let content = s.to_string();
  file.write_all(content.as_bytes())
    .unwrap();
}

pub fn quote_mir(input: TokenStream) -> TokenStream {
  let ast = syn::parse2::<Items>(input).unwrap();
  let builder = syn::Ident::new("b", Span::call_site());
  let result = ast.reduce(&builder);

  output_ts("/tmp/macro.rs", quote! { fn foo() { #result } });
  
  result
}

pub fn quote_mir_block(input: TokenStream) -> TokenStream {
  let ast = syn::parse2::<FnBody>(input).unwrap();
  let b = syn::Ident::new("__b", Span::call_site());
  let fb = syn::Ident::new("__fb", Span::call_site());
  let last_block = ast.last_block_ident();

  let result = ast.reduce(&b, &fb);
  let result = quote! {
    |curr_block| {
      let (mut #b, mut #fb) = ctx.resume_fn();
      let #b = &mut #b;
      #result
      #last_block
    }
  };

  output_ts("/tmp/block.rs", quote! { fn block() { #result } });

  result
}

////////////////////////////////////////////////////////////////////////////////////////////////
