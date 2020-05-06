use {
  lang_base::{
    token::{self, Ident},
  },
  north_core::{
    Model, Token,
    model::member::Reference,
    prelude::*,
    util::chain::Chain2,
  },
  std::{
    cell::Ref,
    fmt::Debug,
    iter,
    marker::PhantomData,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub use {
  lang_base::ast::*
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait GExpr: Node { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct AttrPartOf {
  pub rule: Reference<dyn Item, Token<Ident>>,
  pub prec: Option<Token<token::LitInt>>,
}

impl AttrPartOf {
  pub fn prec(&self) -> u16 {
    match &self.prec {
      Some(token) => token.data.value.parse::<u16>().unwrap(),
      None => 0,
    }
  }
}

impl Attr for AttrPartOf { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct AttrTokenGroup { }

impl Attr for AttrTokenGroup { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)] pub struct BuiltinAny { }
#[derive(Node)] pub struct BuiltinAnyUtf8 { }
#[derive(Node)] pub struct BuiltinBoundary { }
#[derive(Node)] pub struct BuiltinOperatorEnd { }
#[derive(Node)] pub struct BuiltinWhitespaceEnd { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprParse {
  pub greedy: Option<bool>,
  pub value: NodeId<dyn GExpr>
}

impl Expr for ExprParse { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprReject {
  //pub greedy: Option<bool>,
  pub value: NodeId<dyn GExpr>
}

impl Expr for ExprReject { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprBind {
  pub target: Token<Ident>,
  pub value: NodeId<dyn GExpr>,
}

impl GExpr for GExprBind { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprCall {
  pub target: Reference<dyn Node, Token<Ident>>,
  pub recursive: bool,
  pub precedence: Option<u16>,
}

impl GExpr for GExprCall { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprDom {
  pub scope: u32,
  pub inner: NodeId<dyn GExpr>,
}

impl GExpr for GExprDom { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprList {
  pub min_elems: usize,
  pub trailing_sep: bool,
  pub left: NodeId<dyn GExpr>,
  pub right: NodeId<dyn GExpr>,
}

impl GExpr for GExprList { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprLit<L> where L: 'static + Debug {
  pub lit: Token<L>
}

impl<L> GExpr for GExprLit<L> where L: Debug { }

pub type GExprLitClass  = GExprLit<token::LitCharClass>;
pub type GExprLitStr    = GExprLit<token::LitStr>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprOpBin<O> where O: 'static + Debug {
  pub op: Token<O>,
  pub left: NodeId<dyn GExpr>,
  pub right: NodeId<dyn GExpr>,
}

impl<O> GExpr for GExprOpBin<O> where O: Debug { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprOpNAry<O> where O: 'static + Debug {
  pub op: PhantomData<O>,
  pub args: Vec<NodeId<dyn GExpr>>,
}

impl<O> GExpr for GExprOpNAry<O> where O: Debug { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprOpU1<O> where O: 'static + Debug {
  pub op: Token<O>,
  pub inner: NodeId<dyn GExpr>,
}

impl<O> GExpr for GExprOpU1<O> where O: Debug { }

////////////////////////////////////////////////////////////////////////////////////////////////

pub type GExprOpAlt   = GExprOpNAry<token::OpOr>;
pub type GExprOpOpt   = GExprOpU1<token::OpQm>;
pub type GExprOpPlus  = GExprOpU1<token::OpPlus>;
pub type GExprOpSome  = GExprOpNAry<token::OpTilde>;
pub type GExprOpStar  = GExprOpU1<token::OpMult>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprPrefShift {
  pub inner: NodeId<dyn GExpr>,
}

impl GExpr for GExprPrefShift { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct GExprSeq {
  pub args: Vec<NodeId<dyn GExpr>>,
}

impl GExpr for GExprSeq { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemBuiltins {
  pub elems: Vec<NodeId<dyn Node>>,
}

impl Item for ItemBuiltins { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemGrammar {
}

impl Item for ItemGrammar { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemGroup {
  pub attrs: Option<NodeId<Attrs>>,
  pub name: Token<Ident>,
  pub rule: Reference<dyn Item, Token<Ident>>,
  pub prec: Option<Token<token::LitInt>>,
  pub items: Vec<NodeId<dyn Item>>,
}

impl Decl for ItemGroup { }
impl Item for ItemGroup { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemRule {
  pub attrs: Option<NodeId<Attrs>>,
  pub name: Token<Ident>,
  pub params: NodeId<DeclRuleParams>,
  pub ret_ty: Option<NodeId<dyn Type>>,
  pub body: NodeId<ExprBlock>
}

impl ItemRule {
  pub fn attrs<'a>(&self, model: &'a Model) -> Option<Ref<'a, Attrs>> {
    self.attrs.map(|id| model.node(id))
  }

  pub fn attrs_filter<'a, A: Attr>(&self, model: &'a Model) 
    -> impl 'a + Iterator<Item=Ref<'a, A>>
  {
    let pred = move |id| model.try_get::<_, A>(id);
    self.attrs_iter(model).filter_map(pred)
  }

  pub fn attrs_iter<'a>(&self, model: &'a Model)
    -> Box<dyn 'a + Iterator<Item=NodeId<dyn Attr>>> 
  {
    match self.attrs {
      Some(attrs_id) => {
        let attrs = model.node(attrs_id);
        box Chain2::new(attrs, |a| a.attrs.iter().cloned())
      },
      None => {
        box iter::empty()
      }
    }
  }
}

impl Decl for ItemRule { }
impl Item for ItemRule { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemRuleDyn {
  pub name: Token<Ident>,
  pub params: NodeId<DeclRuleParams>,
  pub ret_ty: Option<NodeId<dyn Type>>,
}

impl Decl for ItemRuleDyn { }
impl Item for ItemRuleDyn { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, Hash, Node, PartialEq)]
pub struct TypeRule {
  pub param_types: Vec<NodeId<dyn Type>>,
  pub result_type: NodeId<dyn Type>
}

impl Type for TypeRule { }

////////////////////////////////////////////////////////////////////////////////////////////////

concept_map_part! {
  ConceptMapPart {
    BuiltinAny            => {},
    BuiltinAnyUtf8        => {},
    BuiltinBoundary       => {},
    BuiltinOperatorEnd    => {},
    BuiltinWhitespaceEnd  => {},

    AttrPartOf      => {dyn Attr},
    AttrTokenGroup  => {dyn Attr},
    ExprReject      => {dyn Expr},
    ExprParse       => {dyn Expr},
    GExprBind       => {dyn GExpr},
    GExprCall       => {dyn GExpr},
    GExprDom        => {dyn GExpr},
    GExprList       => {dyn GExpr},
    GExprOpAlt      => {dyn GExpr},
    GExprOpOpt      => {dyn GExpr},
    GExprOpPlus     => {dyn GExpr},
    GExprOpSome     => {dyn GExpr},
    GExprOpStar     => {dyn GExpr},
    GExprPrefShift  => {dyn GExpr},
    GExprSeq        => {dyn GExpr},
    ItemBuiltins    => {dyn Item},
    ItemGrammar     => {dyn Item},
    ItemGroup       => {dyn Decl, dyn Item},
    ItemRule        => {dyn Decl, dyn Item},
    ItemRuleDyn     => {dyn Decl, dyn Item},
    TypeRule        => {dyn Type},
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
