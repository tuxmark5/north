use {
  mir_dsl::{Expr, FnBody, FnParam, Ident, Punctuated, Reduce, Seq, Type},
  proc_macro2::{TokenStream},
  syn::{
    self,
    synom::Synom,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Item {
  Const(ItemConst),
  Fn(ItemFn),
  Static(ItemStatic),
  Type(ItemType),
}

impl Item {
  pub fn reduce(&self, b: &syn::Ident) -> (TokenStream, TokenStream) {
    match self {
      Item::Const(e) => e.reduce(b),
      Item::Fn(e) => e.reduce(b),
      Item::Static(e) => e.reduce(b),
      Item::Type(e) => e.reduce(b),
    }
  }
}

impl Synom for Item {
  named!(parse -> Self, alt!(
    syn!(ItemConst)   => {|e| Item::Const(e)} |
    syn!(ItemFn)      => {|e| Item::Fn(e)} |
    syn!(ItemStatic)  => {|e| Item::Static(e)} |
    syn!(ItemType)    => {|e| Item::Type(e)} 
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ItemConst {
  pub const_token   : Token![const],
  pub name          : Ident,
  pub assign_token  : Token![=],
  pub init          : Expr,
  pub semi_token    : Token![;],
}

impl ItemConst {
  pub fn reduce(&self, b: &syn::Ident) -> (TokenStream, TokenStream) {
    let name = &self.name;
    let init = self.init.reduce_const(b);

    let early = quote! {
      #[allow(non_snake_case)]
      let #name = #init;
    };

    let late = quote! { };

    (early, late)
  }
}

impl Synom for ItemConst {
  named!(parse -> Self, do_parse!(
    const_token     : keyword!(const) >>
    name            : syn!(Ident) >>
    assign_token    : punct!(=) >>
    init            : syn!(Expr) >>
    semi_token      : punct!(;) >>
    (Self { const_token, name, assign_token, init, semi_token })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ItemFn {
  pub fn_token  : Token![fn],
  pub name      : Ident,
  pub params    : Punctuated<FnParam, Token![,]>,
  pub ret_type  : Option<Type>,
  pub body      : FnBody,
}

impl ItemFn {
  pub fn reduce(&self, b: &syn::Ident) -> (TokenStream, TokenStream) {
    match &self.name {
      Ident::Normal(_) => self.reduce_begin(b),
      Ident::Quote(_) => self.reduce_begin(b),
      Ident::Resume(q) => self.reduce_resume(b, &q.ident),
    }
  }

  pub fn reduce_begin(&self, b: &syn::Ident) -> (TokenStream, TokenStream) {
    let name = &self.name;
    let (builder, name_lit) = name.builder_lit();

    let params = self.params.iter().map(|p| p.unwrap_normal());
    let params_early = params.clone().map(|p| p.reduce(b, &builder));
    let params_late = params.enumerate().map(|(id, p)| p.reduce_late(b, &builder, id));

    let ret_ty = match &self.ret_type {
      Some(ret_ty) => ret_ty.reduce(b),
      None => quote! { #b.type_void() },
    };

    let body = self.body.reduce(b, &builder);

    let early = quote! {
      let (#name, mut #builder) = {
        let mut #builder = #b.item_fn(#name_lit, #ret_ty);
        #(#params_early)*
        #builder.complete()
      };
    };

    let late = quote! {{
      #(#params_late)*
      #body 
    }};

    (early, late)
  }

  pub fn reduce_resume(&self, b: &syn::Ident, name: &syn::Ident) -> (TokenStream, TokenStream) {
    let builder = syn::Ident::new("__fb", name.span());
    let params = self.params.iter().map(|p| p.unwrap_normal());
    let params_late = params.enumerate().map(|(id, p)| p.reduce_late(b, &builder, id));
    let body = self.body.reduce(b, &builder);

    let early = quote! { };
    let late = quote! {{
      let mut #builder = #b.item_fn_resume(#name);
      #(#params_late)*
      #body 
    }};

    (early, late)
  }
}

impl Synom for ItemFn {
  named!(parse -> Self, do_parse!(
    fn_token    : keyword!(fn) >>
    name        : syn!(Ident) >>
    params      : parens!(syn!(Punctuated<FnParam, Token![,]>)) >>
    ret_type    : option!(map!(tuple!(punct!(->), syn!(Type)), |t| t.1)) >>
    body        : braces!(syn!(FnBody)) >>
    (Self { fn_token, name, params: params.1, ret_type, body: body.1 })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ItemStatic {
  pub static_token  : Token![static],
  pub name          : Ident,
  pub colon_token   : Token![:],
  pub type_         : Type,
  pub assign_token  : Token![=],
  pub init          : Expr,
  pub semi_token    : Token![;],
}

impl ItemStatic {
  pub fn reduce(&self, b: &syn::Ident) -> (TokenStream, TokenStream) {
    let name = &self.name;
    let (builder, name_lit) = name.builder_lit();
    let ty = self.type_.reduce(b);
    let init = self.init.reduce_const(b);

    let early = quote! {
      #[allow(non_snake_case)]
      let (#name, mut #builder) = {
        let __ty = #ty;
        #b.item_static(#name_lit, __ty)
      };
    };

    let late = quote! {
      { #builder.set_init(#init); }
    };

    (early, late)
  }
}

impl Synom for ItemStatic {
  named!(parse -> Self, do_parse!(
    static_token    : keyword!(static) >>
    name            : syn!(Ident) >>
    colon_token     : punct!(:) >>
    type_           : syn!(Type) >>
    assign_token    : punct!(=) >>
    init            : syn!(Expr) >>
    semi_token      : punct!(;) >>
    (Self { static_token, name, colon_token, type_, assign_token, init, semi_token })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ItemType {
  pub type_token: Token![type],
  pub name: Ident,
  pub assign_token: Token![=],
  pub ty: Type,
  pub semi_token: Token![;],
}

impl ItemType {
  pub fn reduce(&self, b: &syn::Ident) -> (TokenStream, TokenStream) {
    let name = &self.name;
    let name_lit = name.to_lit();

    let ty = if let Type::Tuple(ref t) = self.ty {
      let name = Some(quote! { #name_lit });
      t.reduce_named(b, name)
    } else {
      self.ty.reduce(b)
    };

    let early = quote! {
      let __t = #ty;
      #[allow(non_snake_case)]
      let #name = #b.item_type(#name_lit, __t);
    };

    let late = quote! { };

    (early, late)
  }
}

impl Synom for ItemType {
  named!(parse -> Self, do_parse!(
    type_token: keyword!(type) >>
    name: syn!(Ident) >>
    assign_token: punct!(=) >>
    ty: syn!(Type) >>
    semi_token: punct!(;) >>
    (Self { type_token, name, assign_token, ty, semi_token })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Items {
  pub items: Seq<Item>,
}

impl Reduce for Items {
  fn reduce(&self, b: &syn::Ident) -> TokenStream {
    let (early, late) = self.items.elements.iter()
      .map(|e| e.unwrap_normal().reduce(b))
      .unzip::<_, _, Vec<_>, Vec<_>>();

    quote! {
      #(#early)*
      #(#late)*
    }
  }
}

impl Synom for Items {
  named!(parse -> Self, do_parse!(
    items: syn!(Seq<Item>) >>
    (Self { items })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////
