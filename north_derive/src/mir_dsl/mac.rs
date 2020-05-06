use {
  mir_dsl::{Elem, Expr, ExprCtx, Punctuated},
  proc_macro2::TokenStream,
  quote::ToTokens,
  reflect_fn::fn_meta_path,
  syn::{
    self,
    spanned::Spanned,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Macro: ToTokens {
  fn reduce(&self, path: &syn::Ident, ctx: ExprCtx) -> TokenStream;
  fn reduce_const(&self, path: &syn::Ident, b: &syn::Ident) -> TokenStream;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct MacroBuiltin {
  pub path: syn::Path,
}

impl MacroBuiltin {
  named!(pub parse -> Box<dyn Macro>, do_parse!(
    path: syn!(syn::Path) >>
    (box Self { path })
  ));
}

impl Macro for MacroBuiltin {
  fn reduce(&self, path: &syn::Ident, ctx: ExprCtx) -> TokenStream {
    let (b, _, _) = ctx;
    self.reduce_const(path, b)
  }

  fn reduce_const(&self, _path: &syn::Ident, b: &syn::Ident) -> TokenStream {
    let span = self.span();
    let meta_path = fn_meta_path(&self.path);
    quote_spanned! { span => #b.quote_fn(#meta_path) }
  }
}

impl ToTokens for MacroBuiltin {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.path.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct MacroGeneric {
  pub args: Punctuated<Expr, Token![,]>,
}

impl MacroGeneric {
  named!(pub parse -> Box<dyn Macro>, do_parse!(
    args: syn!(Punctuated<Expr, Token![,]>) >>
    (box Self { args })
  ));

  fn arg(&self, index: usize) -> &Expr {
    match self.args.elements[index] {
      Elem::For(_) => panic!("bad arg"),
      Elem::If(_) => panic!("bad arg"),
      Elem::Normal(ref e) => e,
      Elem::Quote(_) => panic!("bad arg")
    }
  }

  fn reduce_align_of(&self, b: &syn::Ident) -> TokenStream {
    let span = self.span();
    let arg = self.arg(0).reduce_const(b);
    quote_spanned! { span => #b.const_align_of(#arg) }
  }

  fn reduce_ctl_br(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let next = self.arg(0).reduce(ctx);
    quote_spanned! { span => {
      let __next = #next;
      #bb.ctl_br(__next) 
    }}
  }

  fn reduce_ctl_if(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let cond = self.arg(0).reduce(ctx);
    let then_b = self.arg(1).reduce(ctx);
    let else_b = self.arg(2).reduce(ctx);
    quote_spanned! { span => {
      let __cond = #cond;
      let __then_b = #then_b;
      let __else_b = #else_b;
      #bb.ctl_if(__cond, __then_b, __else_b) 
    }}
  }

  fn reduce_generic(
    &self, _path: &syn::Ident, _ctx: ExprCtx
  ) -> TokenStream {
    /*let span = self.span();
    let args = self.args.reduce(b, |arg| {
      let result = arg.reduce(b, bb);
      //let result = quote! { __mb.add_}
      //ReduceResult::new()
    });
    quote_spanned! { span => { 
      let mut __mb = #path!(#b, #bb);
      #args
      __mb.complete()
    }}*/

    quote! { DISABLED }
  }

  fn reduce_lval(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let arg = self.arg(0).reduce(ctx);
    quote_spanned! { span => {
      let __arg = #arg;
      #bb.stmt_lval(__arg) 
    }}
  }

  fn reduce_lval_cast(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let src = self.arg(0).reduce(ctx);
    let ty = self.arg(1).reduce(ctx);
    quote_spanned! { span => {
      let __src = #src;
      let __ty = #ty;
      #bb.stmt_lval_cast(__src, __ty) 
    }}
  }

  fn reduce_rval_cast(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let src = self.arg(0).reduce(ctx);
    let ty = self.arg(1).reduce(ctx);
    quote_spanned! { span => {
      let __src = #src;
      let __ty = #ty;
      #bb.stmt_rval_cast(__src, __ty) 
    }}
  }

  fn reduce_rval_cast_const(&self, b: &syn::Ident) -> TokenStream {
    let span = self.span();
    let src = self.arg(0).reduce_const(b);
    let ty = self.arg(1).reduce_const(b);
    quote_spanned! { span => {
      let __src = #src;
      let __ty = #ty;
      #b.const_rval_cast(__src, __ty) 
    }}
  }

  fn reduce_size_of(&self, b: &syn::Ident) -> TokenStream {
    let span = self.span();
    let arg = self.arg(0).reduce_const(b);
    quote_spanned! { span => #b.const_size_of(#arg) }
  }

  fn reduce_stmt_bin_op(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let op = self.arg(0).reduce(ctx);
    let lhs = self.arg(1).reduce(ctx);
    let rhs = self.arg(2).reduce(ctx);
    quote_spanned! { span => {
      let __lhs = #lhs;
      let __rhs = #rhs;
      #bb.stmt_bin_op(#op, __lhs, __rhs)
    }}
  }

  fn reduce_stmt_set(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let dst = self.arg(0).reduce(ctx);
    let src = self.arg(1).reduce(ctx);
    quote_spanned! { span => {
      let __dst = #dst;
      let __src = #src;
      #bb.stmt_set(__dst, __src)
    }}
  }

  fn reduce_stmt_use(&self, ctx: ExprCtx) -> TokenStream {
    let (_, _, bb) = ctx;
    let span = self.span();
    let src = self.arg(0).reduce(ctx);
    quote_spanned! { span => {
      let __src = #src;
      #bb.stmt_use(__src) 
    }}
  }
}

impl Macro for MacroGeneric {
  fn reduce(&self, path: &syn::Ident, ctx: ExprCtx) -> TokenStream {
    let (b, _, _) = ctx;
    let name = path.to_string();
    match name.as_ref() {
      "align_of"    => self.reduce_align_of(b),
      "ctl_br"      => self.reduce_ctl_br(ctx),
      "ctl_if"      => self.reduce_ctl_if(ctx),
      "lval"        => self.reduce_lval(ctx),
      "lval_cast"   => self.reduce_lval_cast(ctx),
      "rval_cast"   => self.reduce_rval_cast(ctx),
      "size_of"     => self.reduce_size_of(b),
      "stmt_bin_op" => self.reduce_stmt_bin_op(ctx),
      "stmt_set"    => self.reduce_stmt_set(ctx),
      "stmt_use"    => self.reduce_stmt_use(ctx),
      _             => self.reduce_generic(path, ctx)
    }
  }

  fn reduce_const(&self, path: &syn::Ident, b: &syn::Ident) -> TokenStream {
    let name = path.to_string();
    match name.as_ref() {
      "align_of"  => self.reduce_align_of(b),
      "rval_cast" => self.reduce_rval_cast_const(b),
      "size_of"   => self.reduce_size_of(b),
      _           => panic!("unknown const macro")
    }
  }
}

impl ToTokens for MacroGeneric {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    self.args.to_tokens(tokens);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
