use {
  proc_macro2::TokenStream,
  syn::{
    self,
    punctuated::Punctuated,
    synom::Synom
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

type AttrList   = Punctuated<Attr, OpComma>;
type IdentList  = Punctuated<syn::Ident, OpComma>;
type KwStatic   = Token![static];
type OpAssign   = Token![=];
type OpColon    = Token![:];
type OpComma    = Token![,];
type OpSemi     = Token![;];

////////////////////////////////////////////////////////////////////////////////////////////////

struct Attr {
  name: syn::Ident,
  args: Vec<String>,
}

impl Synom for Attr {
  named!(parse -> Self, do_parse!(
    name  : syn!(syn::Ident) >>
    args  : option!(parens!(call!(IdentList::parse_separated_nonempty))) >>
    (Self {
      name,
      args: match args {
        Some(args) => args.1.into_pairs()
          .map(|p| p.into_value().to_string())
          .collect(),
        None => Vec::new(),
      }
    })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct Attrs {
  attrs: AttrList
}

impl Attrs {
  fn process(self) -> CellArgs {
    let mut args = CellArgs::new();

    for attr in self.attrs {
      args.add_attr(attr);
    }

    args
  }
}

impl Synom for Attrs {
  named!(parse -> Self, do_parse!(
    attrs : call!(AttrList::parse_separated_nonempty) >>
    (Self { attrs } )
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct CellArgs {
  borrow        : bool,
  init_default  : bool,
  scope         : String,
}

impl CellArgs {
  pub fn new() -> Self {
    Self {
      borrow: false,
      init_default: false,
      scope: String::new(),
    }
  }

  pub fn add_attr(&mut self, attr: Attr) {
    let name = attr.name.to_string();
    match name.as_ref() {
      "default" => { self.init_default = true; },
      "borrow" => { self.borrow = true; },
      "scope" => { self.scope = Self::make_scope(attr.args); },
      _ => panic!("bad attr {:?}", name)
    }
  }

  pub fn cell_init(&self) -> TokenStream {
    let init = match self.scope.as_ref() {
      "compiler"      => quote! { CompilerCell::new::<__M>() },
      "compiler:node" => quote! { CompilerCell::new::<__M>() },
      "model"         => quote! { ModelKey::new::<__M>() },
      "model:node"    => quote! { ModelKey::new::<__M>() },
      _ => panic!("bad scope: {:?}", self.scope),
    };

    match self.borrow {
      true => quote! { BorrowCell::new(#init) },
      false => init,
    }
  }

  pub fn cell_type(&self, ty: &syn::Type) -> TokenStream {
    let init = match self.init_default {
      true => quote! { DefaultInit },
      false => quote! { ManualInit },
    };

    let inner_type = match self.borrow {
      true => quote! { Rc<RefCell<#ty>> },
      false => quote! { #ty },
    };

    let cell_type = match self.scope.as_ref() {
      "compiler"      => quote! { CompilerCell<()    , #inner_type, #init> },
      "compiler:node" => quote! { CompilerCell<NodeId, #inner_type, #init> },
      "model"         => quote! { ModelKey<()    , #inner_type, #init> },
      "model:node"    => quote! { ModelKey<NodeId, #inner_type, #init> },
      _ => panic!("bad scope: {:?}", self.scope),
    };

    match self.borrow {
      true => quote! { BorrowCell<#cell_type> },
      false => cell_type,
    }
  }

  pub fn make_scope(scope: Vec<String>) -> String {
    scope.as_slice().join(":")
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct ItemStatic {
  vis         : syn::Visibility,
  _kw_static  : KwStatic,
  name        : syn::Ident,
  _op_colon   : OpColon,
  var_ty      : syn::Type,
  _op_assign  : OpAssign,
  _init       : syn::Expr,
  _op_semi    : OpSemi,
}

impl ItemStatic {
  fn reduce(&self, args: &CellArgs) -> TokenStream {
    let vis = &self.vis;
    let name = &self.name;
    let ty = args.cell_type(&self.var_ty);
    let init = args.cell_init();

    quote! {
      #vis static #name: #ty = {
        struct __M;
        #init
      };
    }
  }
}

impl Synom for ItemStatic {
  named!(parse -> Self, do_parse!(
    vis         : syn!(syn::Visibility) >>
    _kw_static  : syn!(KwStatic) >>
    name        : syn!(syn::Ident) >>
    _op_colon   : syn!(OpColon) >>
    var_ty      : syn!(syn::Type) >>
    _op_assign  : syn!(OpAssign) >>
    _init       : syn!(syn::Expr) >>
    _op_semi    : syn!(OpSemi) >>
    (Self {
      vis, _kw_static, name, _op_colon, var_ty, _op_assign, _init, _op_semi,
    })
  ));
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn cell(attrs: TokenStream, input: TokenStream) -> TokenStream {
  let attrs   = syn::parse2::<Attrs>(attrs).unwrap();
  let ast     = syn::parse2::<ItemStatic>(input).unwrap();

  let args    = attrs.process();
  let result  = ast.reduce(&args);
  //println!("{}", result);
  result
}

////////////////////////////////////////////////////////////////////////////////////////////////
