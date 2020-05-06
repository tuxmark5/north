use {
  proc_macro2::{Ident, TokenStream, TokenTree},
  syn::{self},
};

////////////////////////////////////////////////////////////////////////////////////////////////

/*pub fn derive_context_impl(input: TokenStreamRaw) -> TokenStreamRaw {
  let ast = syn::parse(input).unwrap();
  let gen = gen_context_impls(&ast);
  println!("! {}", gen);
  gen.into()
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

//  attr: &syn::Attribute
pub fn gen_context_impl(ast: &syn::DeriveInput, args: &TokenStream) -> TokenStream {
  let name = &ast.ident;
  let trait_name = get_context_trait_name(args);
  let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
  //let macro_args = get_context_macro_args(args);

  quote! {
    impl #impl_generics #trait_name for #name #type_generics #where_clause {
      context_impl!(#args);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

/*fn gen_context_impls(ast: &syn::DeriveInput) -> TokenStream {
  let context_attrs = ast.attrs.iter()
    .filter(|attr| test_attr_name(&attr.path))
    .map(|attr| gen_context_impl(ast, attr))
    .collect::<Vec<_>>();

  quote! {
    #(#context_attrs)*
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

/*fn get_context_macro_args(args: &TokenStream) -> TokenStream {
  let mut iter = args.clone().into_iter();
  let _trait_name = iter.next();
  let _comma = iter.next();
  TokenStream::from_iter(iter)
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

fn get_context_trait_name(args: &TokenStream) -> Ident {
  let mut iter = args.clone().into_iter();
  match iter.next().unwrap() {
    TokenTree::Ident(ident) => ident,
    _ => panic!("bad argument 2")
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

/*fn test_attr_name(path: &syn::Path) -> bool {
  if path.segments.len() != 1 { return false; }

  let pair = path.segments.first().unwrap();
  let segment = pair.value();

  segment.ident.to_string()
    .starts_with("context")
}*/

////////////////////////////////////////////////////////////////////////////////////////////////
