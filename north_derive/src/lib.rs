#![feature(box_syntax)]
#![feature(nll)]
#![feature(specialization)]
#![recursion_limit="128"]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

////////////////////////////////////////////////////////////////////////////////////////////////

mod aspect_rules_impl;
mod cell_macro;
mod derive_context;
mod derive_node;
mod mir_dsl;
mod reflect_fn;

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  derive_context::{gen_context_impl},
  derive_node::derive_node_impl,
  proc_macro::{TokenStream as TokenStreamRaw},
  proc_macro2::TokenStream,
  syn::{
    {braces, do_parse, named, punct, syn},
    punctuated::Punctuated,
    synom::{Synom},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[proc_macro]
pub fn aspect_rules(input: TokenStreamRaw) -> TokenStreamRaw {
  aspect_rules_impl::aspect_rules(input.into()).into()
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[proc_macro_attribute]
pub fn cell(attr: TokenStreamRaw, input: TokenStreamRaw) -> TokenStreamRaw {
  cell_macro::cell(attr.into(), input.into()).into()
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[proc_macro_attribute]
pub fn context(attr: TokenStreamRaw, input: TokenStreamRaw) -> TokenStreamRaw {
  let attr: TokenStream = attr.into();
  let derive_input = syn::parse(input.clone()).unwrap();
  let context_impl = gen_context_impl(&derive_input, &attr);
  let rest: TokenStream = input.into();

  let result = quote! {
    #context_impl
    #rest
  };

  result.into()
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[proc_macro_derive(Node)]
pub fn derive_node(input: TokenStreamRaw) -> TokenStreamRaw {
  derive_node_impl(input)
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct ConceptMap {
  lang_ty: syn::Type,
  body: Vec<ConceptMapEntry>,
}

impl Synom for ConceptMap {
  named!(parse -> Self, do_parse!(
    lang_ty: syn!(syn::Type) >>
    body: braces!(many0!(syn!(ConceptMapEntry))) >>
    (Self { lang_ty, body: body.1 })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
struct ConceptMapEntry {
  node_ty: syn::Type,
  trait_types: Vec<syn::Type>,
}

named!(ty_list -> Vec<syn::Type>, do_parse!(
  x: call!(Punctuated::<syn::Type, Token!(,)>::parse_separated) >>
  (x.into_pairs().map(|p| p.into_value()).collect())
));

impl Synom for ConceptMapEntry {
  named!(parse -> Self, do_parse!(
    node_ty: syn!(syn::Type) >>
    _arrow_token: punct!(=>) >>
    trait_types: alt!(
      syn!(syn::Type) => { |e| vec![e] } |
      braces!(ty_list) => { |e| e.1 }
    ) >>
    _comma: punct!(,) >>
    (Self { node_ty, trait_types })
  ));
}

#[proc_macro]
pub fn concept_map(input: TokenStreamRaw) -> TokenStreamRaw {
  let ast: ConceptMap = syn::parse(input).unwrap();

  let lang_ty = ast.lang_ty;
  let node_tys = ast.body.iter()
    .map(|b| &b.node_ty);

  let impls = ast.body.iter().flat_map(|e| {
    let node_ty = &e.node_ty;
    e.trait_types.iter().map(move |trait_ty| quote! {
      tm.register::<#node_ty, #trait_ty>();
    })
  });

  let gen = quote! {
    use north_core::concept_map_prelude::*;

    impl ConceptMap for #lang_ty {
      fn concepts(&self) -> Box<dyn Iterator<Item=&dyn Concept>> {
        let concepts = vec![ #(#node_tys::concept()),* ];
        box concepts.into_iter()
      }

      fn setup_traits(&self, tm: &mut TraitManager) {
        #(#impls)*
      }
    }
  };

  //println!("KKK {}", gen); 
  gen.into()
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[proc_macro]
pub fn quote_mir(input: TokenStreamRaw) -> TokenStreamRaw {
  mir_dsl::quote_mir(input.into()).into()
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[proc_macro]
pub fn quote_mir_block(input: TokenStreamRaw) -> TokenStreamRaw {
  mir_dsl::quote_mir_block(input.into()).into()
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[proc_macro_attribute]
pub fn reflect_fn(_attr: TokenStreamRaw, input: TokenStreamRaw) -> TokenStreamRaw {
  reflect_fn::reflect_fn(input.into()).into()
}

////////////////////////////////////////////////////////////////////////////////////////////////
