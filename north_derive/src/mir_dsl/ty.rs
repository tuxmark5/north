use {
  mir_dsl::{Expr, Punctuated, Quote, ReduceResult},
  proc_macro2::TokenStream,
  syn::{
    self,
    spanned::Spanned,
    synom::Synom,
    token,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Type {
  Array(TypeArray),
  Path(TypePath),
  Ptr(TypePtr),
  Quote(TypeQuote),
  Ref(TypeRef),
  Tuple(TypeTuple),
}

impl Type {
  pub fn reduce(&self, b: &syn::Ident) -> TokenStream {
    match self {
      Type::Array(t)  => t.reduce(b),
      Type::Path(t)   => t.reduce(b),
      Type::Ptr(t)    => t.reduce(b),
      Type::Quote(t)  => t.reduce(b),
      Type::Ref(t)    => t.reduce(b),
      Type::Tuple(t)  => t.reduce(b),
    }
  }
}

impl Synom for Type {
  named!(parse -> Self, alt!(
    syn!(TypeArray) => {|e| Type::Array(e)} |
    syn!(TypePath)  => {|e| Type::Path(e)} |
    syn!(TypePtr)   => {|e| Type::Ptr(e)} |
    syn!(TypeQuote) => {|e| Type::Quote(e)} |
    syn!(TypeRef)   => {|e| Type::Ref(e)} |
    syn!(TypeTuple) => {|e| Type::Tuple(e)}
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypeArray {
  pub elem_ty: Box<Type>,
  pub semi_token: Token![;],
  pub elem_num: Box<Expr>,
}

impl TypeArray {
  pub fn reduce(&self, b: &syn::Ident) -> TokenStream {
    let elem_ty = self.elem_ty.reduce(b);
    let elem_num = self.elem_num.reduce_const(b);
    quote! {{
      let __ty = #elem_ty;
      let __len = #elem_num;
      #b.type_array(__ty, __len) 
    }}
  }
}

impl Synom for TypeArray {
  named!(parse -> Self, map!(
    brackets!(do_parse!(
      elem_ty: syn!(Type) >>
      semi_token: punct!(;) >>
      elem_num: syn!(Expr) >>
      (Self { 
        elem_ty: box elem_ty, 
        semi_token, 
        elem_num: box elem_num 
      })
    )),
    |r| r.1
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypeQuote {
  pub hash_token: Token![#],
  pub quote: Quote
}

impl TypeQuote {
  pub fn reduce(&self, b: &syn::Ident) -> TokenStream {
    let result = self.quote.reduce(b);
    quote! { #result }
  }
}

impl Synom for TypeQuote {
  named!(parse -> Self, do_parse!(
    hash_token: punct!(#) >>
    quote: syn!(Quote) >>
    (Self { hash_token, quote })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypePath {
  pub path: syn::TypePath
}

impl TypePath {
  pub fn reduce(&self, b: &syn::Ident) -> TokenStream {
    let path = &self.path;
    quote! {
      #b.quote_type::<#path>()
    }
  }
}

impl Synom for TypePath {
  named!(parse -> Self, do_parse!(
    path: syn!(syn::TypePath) >>
    (Self { path })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypePtr {
  pub star_token: Token![*],
  pub mutability: Result<token::Const, token::Mut>,
  pub elem: Box<Type>,
}

impl TypePtr {
  pub fn reduce(&self, b: &syn::Ident) -> TokenStream {
    let elem = self.elem.reduce(b);
    let mutable = syn::LitBool {
      value: self.mutability.is_err(),
      span: match self.mutability {
        Ok(c) => c.span(),
        Err(m) => m.span(),
      },
    };
    quote! {{
      let __elem = #elem;
      #b.type_ref(#mutable, __elem)
    }}
  }
}

impl Synom for TypePtr {
  named!(parse -> Self, do_parse!(
    star_token: punct!(*) >>
    mutability: alt!(
      keyword!(const) => {|c| Ok(c)}
      |
      keyword!(mut) => {|m| Err(m)}
    ) >>
    elem: syn!(Type) >>
    (Self { 
      star_token, 
      mutability,
      elem: box elem 
    })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypeRef {
  pub and_token: Token![&],
  pub mutability: Option<token::Mut>,
  pub elem: Box<Type>,
}

impl TypeRef {
  pub fn reduce(&self, b: &syn::Ident) -> TokenStream {
    let elem = self.elem.reduce(b);
    let mutable = syn::LitBool {
      value: self.mutability.is_some(),
      span: self.and_token.span(),
    };
    quote! { #b.type_ref(#mutable, #elem) }
  }
}

impl Synom for TypeRef {
  named!(parse -> Self, do_parse!(
    and_token: punct!(&) >>
    mutability: option!(keyword!(mut)) >>
    elem: syn!(Type) >>
    (Self { 
      and_token, 
      mutability, 
      elem: box elem 
    })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypeTuple {
  pub elements: Punctuated<Type, Token![,]>,
}

impl TypeTuple {
  pub fn reduce(&self, b: &syn::Ident) -> TokenStream {
    self.reduce_named(b, None)
  }

  pub fn reduce_named(&self, b: &syn::Ident, name: Option<TokenStream>) -> TokenStream {
    if self.elements.is_empty() {
      quote! {
        #b.type_void()
      }
    } else {
      let name = match name {
        Some(name) => quote! { Some(#name) },
        None => quote! { None },
      };

      let elements = self.elements.reduce(b, 
        &|elem| {
          let elem = elem.reduce(b);
          let result = quote! { __tb.add_element(#elem); };
          ReduceResult::new(result)
        },
        &|multi| {
          quote! { __tb.add_elements(#multi); }
        }
      );
        
      quote! {{
        let mut __tb = #b.type_struct(#name);
        #elements
        __tb.complete(#b)
      }}
    }
  }
}

impl Synom for TypeTuple {
  named!(parse -> Self, do_parse!(
    elements: parens!(syn!(Punctuated<Type, Token![,]>)) >>
    (Self { elements: elements.1 })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////
