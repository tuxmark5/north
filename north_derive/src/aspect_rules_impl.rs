use {
  proc_macro2::{TokenStream, TokenTree},
  quote::ToTokens,
  syn::{
    self,
    punctuated::Punctuated,
    synom::Synom,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type TokenSelf = Token![self];

#[derive(Debug)]
pub enum Param {
  NormalParam(syn::Ident),
  SelfParam(TokenSelf),
}

impl Synom for Param {
  named!(parse -> Self, alt!(
    syn!(syn::Ident) => {|e| Param::NormalParam(e)} |
    syn!(TokenSelf) => {|e| Param::SelfParam(e)}
  ));
}

impl ToTokens for Param {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    match self {
      Param::NormalParam(param) => param.to_tokens(tokens),
      Param::SelfParam(param) => param.to_tokens(tokens),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct ParamList {
  params: Punctuated<Param, TokenComma>,
}

impl Synom for ParamList {
  named!(parse -> Self, do_parse!(
    params : call!(Punctuated::<Param, TokenComma>::parse_separated_nonempty) >>
    (Self { params })
  ));
}

impl ToTokens for ParamList {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.params.to_tokens(tokens)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type TokenArrow1 = Token![->];
pub type TokenArrow2 = Token![=>];
pub type TokenComma = Token![,];
pub type TokenExclaim = Token![!];
pub type TokenFn = Token![fn];
pub type TypeList = Punctuated<syn::Type, TokenComma>;

#[derive(Debug)]
pub struct RuleImpl {
  generics      : syn::Generics,
  rule_name     : syn::Ident,
  tok_exclaim   : TokenExclaim,
  rule_params   : TypeList,
  tok_arrow     : TokenArrow2,
  user_rule     : syn::Type,
  items         : Vec<RuleItem>,
}

impl RuleImpl {
  fn descr(&self) -> TokenStream {
    let macro_name = &self.rule_name;
    quote! { #macro_name! }
  }

  fn reduce(&self) -> TokenStream {
    let descr = self.descr();
    let generics = &self.generics;
    let trait_params = &self.rule_params;
    let user_rule = &self.user_rule;
    let items = self.items.iter().map(|r| r.reduce(self));
    let body = quote! { { #(#items)* } };

    quote! {
      #descr(__trait, (#generics), (#trait_params), #user_rule, #body);
    }
  }
}

impl Synom for RuleImpl {
  named!(parse -> Self, do_parse!(
    generics    : syn!(syn::Generics) >>
    rule_name   : syn!(syn::Ident) >>
    tok_exclaim : syn!(TokenExclaim) >>
    inner       : brackets!(do_parse!(
      rule_params : call!(TypeList::parse_separated_nonempty) >>
      tok_arrow   : syn!(TokenArrow2) >>
      user_rule   : syn!(syn::Type) >>
      ((rule_params, tok_arrow, user_rule))
    )) >>
    items       : braces!(many0!(syn!(RuleItem))) >>
    (Self {
      generics,
      rule_name,
      tok_exclaim,
      rule_params: (inner.1).0,
      tok_arrow: (inner.1).1,
      user_rule: (inner.1).2,
      items: items.1
    })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct RuleImpls {
  impls: Vec<RuleImpl>
}

impl RuleImpls {
  fn reduce(&self) -> TokenStream {
    let impls = self.impls.iter().map(RuleImpl::reduce);
    quote! { #(#impls)* }
  }
}

impl Synom for RuleImpls {
  named!(parse -> Self, do_parse!(
    impls : many0!(syn!(RuleImpl)) >>
    (Self { impls })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub enum RuleItem {
  Const(RuleItemConst),
  Macro(RuleItemMacro),
  Method(RuleItemMethod),
}

impl RuleItem {
  fn reduce(&self, rule: &RuleImpl) -> TokenStream {
    match self {
      RuleItem::Const(const_) => const_.reduce(rule),
      RuleItem::Macro(macro_) => macro_.reduce(rule),
      RuleItem::Method(method) => method.reduce(rule)
    }
  }
}

impl Synom for RuleItem {
  named!(parse -> Self, alt!(
    syn!(RuleItemConst) => {|e| RuleItem::Const(e)} |
    syn!(RuleItemMacro) => {|e| RuleItem::Macro(e)} |
    syn!(RuleItemMethod) => {|e| RuleItem::Method(e)}
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct RuleItemConst {
  name    : syn::Ident,
  value   : syn::Expr,
}

impl RuleItemConst {
  fn reduce(&self, rule: &RuleImpl) -> TokenStream {
    let descr = rule.descr();
    let name = &self.name;
    let rule_params = &rule.rule_params;
    let value = &self.value;

    quote! {
      #descr(__const, #name, (#rule_params), #value);
    }
  }
}

impl Synom for RuleItemConst {
  named!(parse -> Self, do_parse!(
    _const  : keyword!(const) >>
    name    : syn!(syn::Ident) >>
    _bang   : punct!(=) >>
    value   : syn!(syn::Expr) >>
    _semi   : punct!(;) >>
    (Self { name, value })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct RuleItemMacro {
  name    : syn::Ident,
  body    : TokenTree,
}

impl RuleItemMacro {
  fn reduce(&self, rule: &RuleImpl) -> TokenStream {
    let descr = rule.descr();
    let name = &self.name;
    let rule_params = &rule.rule_params;
    let body = &self.body;

    quote! {
      #descr(__macro, #name!, (#rule_params), #body);
    }
  }
}

impl Synom for RuleItemMacro {
  named!(parse -> Self, do_parse!(
    name    : syn!(syn::Ident) >>
    _bang   : punct!(!) >>
    body    : syn!(TokenTree) >>
    (Self { name, body })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct RuleItemMethod {
  tok_fn  : TokenFn,
  name    : syn::Ident,
  params  : ParamList,
  ret_ty  : Option<syn::Type>,
  body    : TokenTree,
}

impl RuleItemMethod {
  fn reduce(&self, rule: &RuleImpl) -> TokenStream {
    let descr = rule.descr();
    let name = &self.name;
    let params = &self.params;
    let rule_params = &rule.rule_params;
    let ret_ty = match self.ret_ty {
      Some(ref ret_ty) => quote! { #ret_ty },
      None => quote! { },
    };
    let body = &self.body;

    quote! {
      #descr(__method, #name, (#rule_params), (#params), (#ret_ty), #body);
    }
  }
}

impl Synom for RuleItemMethod {
  named!(parse -> Self, do_parse!(
    tok_fn  : keyword!(fn) >>
    name    : syn!(syn::Ident) >>
    params  : parens!(syn!(ParamList)) >>
    ret_ty  : option!(tuple!(syn!(TokenArrow1), syn!(syn::Type))) >>
    body    : syn!(TokenTree) >>
    (Self {
      tok_fn,
      name,
      params: params.1,
      ret_ty: ret_ty.map(|e| e.1),
      body
    })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn aspect_rules(input: TokenStream) -> TokenStream {
  let ast = syn::parse2::<RuleImpls>(input).unwrap();
  let result = ast.reduce();
  //println!("{}", result);
  result
}

////////////////////////////////////////////////////////////////////////////////////////////////
