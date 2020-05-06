use {
  proc_macro::{TokenStream as TokenStreamRaw},
  proc_macro2::{Ident, TokenStream},
  syn::{
    self,
    spanned::Spanned,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn derive_node_impl(input: TokenStreamRaw) -> TokenStreamRaw {
  let ast = syn::parse(input).unwrap();
  let gen = impl_node(&ast);
  // println!("KKK {}", gen);
  gen.into()
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct Field<'a> {
  field: &'a syn::Field,
  name: Ident,
  offset_name: Ident,
}

fn get_field(index: usize, field: &syn::Field) -> Field {
  let name = get_field_name(index, field);
  let offset_name = format!("__OFFSET_{}", index);
  let offset_name = Ident::new(offset_name.as_ref(), name.span());

  Field {
    field,
    name,
    offset_name
  }
}

fn get_fields(data: &syn::Data) -> Vec<Field> {
  let struct_ = match data {
    syn::Data::Struct(ref s) => s,
    _ => panic!("bads"),
  };

  let fields = match struct_.fields {
    syn::Fields::Named(ref named) => &named.named,
    syn::Fields::Unnamed(ref unnamed) => &unnamed.unnamed,
    _ => panic!("bads2"),
  };

  fields.iter()
    .enumerate()
    .map(|(i, field)| get_field(i, field))
    .collect()
}

fn get_field_name(index: usize, field: &syn::Field) -> Ident {
  match field.ident {
    Some(ref name) => name.clone(),
    None => Ident::new(format!("{}", index).as_str(), field.span()),
  }
}

fn impl_node(ast: &syn::DeriveInput) -> TokenStream {
  let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
  let fields = get_fields(&ast.data);
  let member_descrs = impl_member_descrs(&fields);
  //let member_offsets = impl_member_offsets(&fields);
  let name = &ast.ident;
  let name_str = name.to_string();
  let mod_name_str = format!("_node_{}", name_str.to_lowercase());
  let mod_name = Ident::new(mod_name_str.as_str(), ast.span());

  quote! {
    mod #mod_name {
      use north_core::derive_prelude::*;
      use super::*;

      /*impl #impl_generics #name #type_generics #where_clause {
        #(#member_offsets)*
      }*/

      impl #impl_generics Node for #name #type_generics #where_clause {
        fn as_any(&self) -> &dyn Any { self }
        fn as_any_mut(&mut self) -> &mut dyn Any { self }

        fn concept() -> &'static dyn Concept where Self: Sized {
          &ConceptDescr::<Self> {
            members: &[ #member_descrs ],
            name: #name_str,
            node_type: PhantomData
          }
        }

        fn instance_concept(&self) -> &'static dyn Concept { Self::concept() }
      }
    }
  }
}

fn impl_member_descr(field: &Field) -> TokenStream {
  let field_name = &field.name;
  let field_name_str = field.name.to_string();
  let field_offset_name = &field.offset_name;
  let field_type = &field.field.ty;

  quote! {
    &MemberDescr::<Self, #field_type> {
      name: #field_name_str,
      //field_offset: Self::#field_offset_name,
      field_offset: field_offset_of!(Self, #field_name),
      object_type: PhantomData,
      target_type: PhantomData,
    }
  }
}

fn impl_member_descrs(fields: &Vec<Field>) -> TokenStream {
  let descrs = fields.iter()
    .map(|x| impl_member_descr(x));

  quote! {
    #(#descrs),*
  }
}

fn impl_member_offset(field: &Field) -> TokenStream {
  let field_name = &field.name;
  let field_offset_name = &field.offset_name;

  quote! {
    const #field_offset_name: usize = field_offset_of!(Self, #field_name);
  }
}

fn impl_member_offsets(fields: &Vec<Field>) -> Vec<TokenStream> {
  fields.iter()
    .map(|x| impl_member_offset(x))
    .collect()
}

////////////////////////////////////////////////////////////////////////////////////////////////
