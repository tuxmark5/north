use {
  proc_macro2::{Span, TokenStream},
  syn::{
    self,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn fn_meta_ident(ident: &syn::Ident) -> syn::Ident {
  let name = ident.to_string();
  let name = format!("__fn_{}", name);
  let span = ident.span();
  syn::Ident::new(name.as_ref(), span)
}

pub fn fn_meta_path(path: &syn::Path) -> syn::Path {
  let mut path = path.clone();
  let mut last = path.segments.last_mut().unwrap();
  let ident = &mut last.value_mut().ident;
  *ident = fn_meta_ident(ident);
  path
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn reflect_fn(input: TokenStream) -> TokenStream {
  let ast = syn::parse2::<syn::ItemFn>(input).unwrap();
  let name = &ast.ident;
  let meta = fn_meta_ident(name);
  let vis = &ast.vis;
  let quote_fn = format!("quote_fn_{}", ast.decl.inputs.len());
  let quote_fn = syn::Ident::new(quote_fn.as_ref(), Span::call_site());
  let quote_mod = quote! { ::north_core::quote };

  quote! {
    #ast
    #vis fn #meta() -> #quote_mod::RustFn {
      #quote_mod::#quote_fn(Self::#name)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
