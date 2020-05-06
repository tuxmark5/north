use {
  crate::token::{self, Ident},
  north_core::{
    Token,
    model::member::Reference,
    model::value::{MRef},
    prelude::*,
  },
  north_typesys::TypeRef,
  std::{
    cell::Ref,
    fmt::Debug,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Attr: Node { }

pub trait Decl: Node { }

pub trait Expr: Node { }

pub trait Item: Node { }

pub trait Lit: Node { } // ???

pub trait Loop: Expr {
  fn is_loop_ctl_allowed(&self, _link_id: usize) -> bool { false }
}

pub trait Stmt: Node { }

pub trait Type: Node { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct Attrs {
  pub attrs: Vec<NodeId<dyn Attr>>,
}

impl Attrs {
  pub fn contains_attr<A: Attr>(&self, model: &Model) -> bool {
    let pred = move |id| model.try_get::<_, A>(id);
    self.attrs.iter().find_map(pred).is_some()
  }

  pub fn contains_attr_<A: Attr>(self: &MRef<Self>) -> bool {
    let attrs = self.map(|s| s.attrs.iter());
    let attr = attrs.find_map(|a| a.try_as::<A>());
    attr.is_some()
  }

  pub fn filter_attrs<'a, A: Attr>(&'a self, model: &'a Model) 
    -> impl 'a + Iterator<Item=Ref<'a, A>>
  {
    let pred = move |id| model.try_get::<_, A>(id);
    self.attrs.iter().filter_map(pred)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct DeclLet {
  pub var: Token<Ident>,
  pub ty: Option<NodeId<dyn Type>>,
  pub init: Option<NodeId<dyn Expr>>,
}

impl Decl for DeclLet { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct DeclRuleParam {
  pub var: Token<Ident>,
  pub ty: NodeId<dyn Type>,
}

impl Decl for DeclRuleParam { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct DeclRuleParams {
  pub params: Vec<NodeId<DeclRuleParam>>,
}

impl Decl for DeclRuleParams { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Node)]
pub struct ExprBlock {
  pub elems: Vec<NodeId<dyn Stmt>>
}

impl Expr for ExprBlock { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprBreak {
  pub value: Option<NodeId<dyn Expr>>
}

impl Expr for ExprBreak { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprCall {
  pub target: NodeId<dyn Expr>,
  pub args: Vec<NodeId<dyn Expr>>,
}

impl Expr for ExprCall { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Node)]
pub struct ExprContinue { }
impl Expr for ExprContinue { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprIf {
  pub cond: NodeId<dyn Expr>,
  pub body1: NodeId<ExprBlock>,
  pub body0: Option<NodeId<ExprBlock>>,
}

impl Expr for ExprIf { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprLit<L: 'static + Debug> {
  pub lit: Token<L>
}

impl<L: Debug> Expr for ExprLit<L> { }

pub type ExprLitInt = ExprLit<token::LitInt>;
pub type ExprLitStr = ExprLit<token::LitStr>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprLoop {
  pub body: NodeId<ExprBlock>
}

impl Expr for ExprLoop { }

impl Loop for ExprLoop {
  fn is_loop_ctl_allowed(&self, _link_id: usize) -> bool { true }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprOpBin<O: 'static + Debug> {
  pub op: Token<O>,
  pub left: NodeId<dyn Expr>,
  pub right: NodeId<dyn Expr>,
}

impl<O: Debug> Expr for ExprOpBin<O> { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprOpPre<O: 'static + Debug> {
  pub op: Token<O>,
  pub inner: NodeId<dyn Expr>,
}

impl<O: Debug> Expr for ExprOpPre<O> { }

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ExprOpAriAdd     = ExprOpBin<token::OpPlus>;
pub type ExprOpAriAddSet  = ExprOpBin<token::OpPlusEq>;
pub type ExprOpAriDiv     = ExprOpBin<token::OpDiv>;
pub type ExprOpAriDivSet  = ExprOpBin<token::OpDivEq>;
pub type ExprOpAriMod     = ExprOpBin<token::OpMod>;
pub type ExprOpAriModSet  = ExprOpBin<token::OpModEq>;
pub type ExprOpAriNeg     = ExprOpPre<token::OpMinus>;
pub type ExprOpAriMul     = ExprOpBin<token::OpMult>;
pub type ExprOpAriMulSet  = ExprOpBin<token::OpMultEq>;
pub type ExprOpAriSet     = ExprOpBin<token::OpEq>;
pub type ExprOpAriSub     = ExprOpBin<token::OpMinus>;
pub type ExprOpAriSubSet  = ExprOpBin<token::OpMinusEq>;

pub type ExprOpBitAnd     = ExprOpBin<token::OpAnd>;
pub type ExprOpBitOr      = ExprOpBin<token::OpOr>;
pub type ExprOpBitNot     = ExprOpPre<token::OpTilde>;
pub type ExprOpBitShl     = ExprOpBin<token::OpLtLt>;
pub type ExprOpBitShr     = ExprOpBin<token::OpGtGt>;
pub type ExprOpBitXor     = ExprOpBin<token::OpXor>;

pub type ExprOpCmpEq      = ExprOpBin<token::OpEqEq>;
pub type ExprOpCmpGt      = ExprOpBin<token::OpGt>;
pub type ExprOpCmpGtEq    = ExprOpBin<token::OpGtEq>;
pub type ExprOpCmpLt      = ExprOpBin<token::OpLt>;
pub type ExprOpCmpLtEq    = ExprOpBin<token::OpLtEq>;
pub type ExprOpCmpNotEq   = ExprOpBin<token::OpExEq>;

pub type ExprOpLogAnd     = ExprOpBin<token::OpAndAnd>;
pub type ExprOpLogNot     = ExprOpPre<token::OpEx>;
pub type ExprOpLogOr      = ExprOpBin<token::OpOrOr>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprReturn {
  pub value: Option<NodeId<dyn Expr>>
}

impl Expr for ExprReturn { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprVar {
  pub var: Reference<dyn Decl, Token<Ident>>
}

impl Expr for ExprVar { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ExprWhile {
  pub cond: NodeId<dyn Expr>,
  pub body: NodeId<ExprBlock>
}

impl Expr for ExprWhile { }

impl Loop for ExprWhile {
  fn is_loop_ctl_allowed(&self, link_id: usize) -> bool { link_id == 1 }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct Items {
  pub items: Vec<NodeId<dyn Item>>,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemBuiltins {
  pub nodes: Vec<NodeId<dyn Node>>,
}

impl Item for ItemBuiltins { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemFn {
  pub name: Token<Ident>,
  pub params: NodeId<DeclRuleParams>,
  pub ret_ty: Option<NodeId<dyn Type>>,
  pub body: NodeId<ExprBlock>
}

impl Decl for ItemFn { }
impl Item for ItemFn { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemType {
  pub name: Token<Ident>,
  pub body: NodeId<dyn Type>,
}

impl ItemType {
  pub fn new<N: ToString>(name: N, body: NodeId<dyn Type>) -> Self {
    let name = Token::new(Ident { name: name.to_string() });
    Self { name, body }
  }
}

impl Item for ItemType { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct LitStr {
  pub token: Token<String>,
}

impl Lit for LitStr { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtDecl {
  pub decl: NodeId<dyn Decl>
}

impl Stmt for StmtDecl { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtExpr {
  pub expr: NodeId<dyn Expr>,
  pub semi: Option<Token<token::OpSemicolon>>,
}

impl Stmt for StmtExpr { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtItem {
  pub item: NodeId<dyn Item>
}

impl Stmt for StmtItem { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, Hash, Node, PartialEq)]
pub struct TypeBool { }

impl Type for TypeBool { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, Hash, Node, PartialEq)]
pub struct TypeFn {
  pub param_types: Vec<NodeId<dyn Type>>,
  pub result_type: NodeId<dyn Type>
}

impl Type for TypeFn { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, Hash, Node, PartialEq)]
pub struct TypeInt {
  pub signed: bool,
  pub size: usize
}

impl TypeInt {
  pub fn new(signed: bool, size: usize) -> Self {
    Self { signed, size }
  }
}

impl Type for TypeInt { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, Hash, Node, PartialEq)]
pub struct TypeNever { }

impl Type for TypeNever { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypePrim<T: 'static + Debug> {
  pub token: Token<T>
}

impl<T: Debug> Type for TypePrim<T> { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, Hash, Node, PartialEq)]
pub struct TypeUnit { }

impl Type for TypeUnit { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypeVar {
  pub var: Reference<dyn Type, Token<Ident>>
}

impl Type for TypeVar { }

impl TypeRef for TypeVar {
  fn target_type(&self) -> NodeId {
    self.var.target_node.unwrap().cast()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

concept_map_part! {
  ConceptMapPart {
    DeclLet         => {dyn Decl},
    DeclRuleParam   => {dyn Decl},
    DeclRuleParams  => {dyn Decl},
    ExprBlock       => {dyn Expr},
    ExprBreak       => {dyn Expr},
    ExprCall        => {dyn Expr},
    ExprContinue    => {dyn Expr},
    ExprIf          => {dyn Expr},
    ExprLitInt      => {dyn Expr},
    ExprLitStr      => {dyn Expr},
    ExprLoop        => {dyn Expr, dyn Loop},
    ExprOpAriAdd    => {dyn Expr},
    ExprOpAriAddSet => {dyn Expr},
    ExprOpAriDiv    => {dyn Expr},
    ExprOpAriDivSet => {dyn Expr},
    ExprOpAriMod    => {dyn Expr},
    ExprOpAriModSet => {dyn Expr},
    ExprOpAriNeg    => {dyn Expr},
    ExprOpAriMul    => {dyn Expr},
    ExprOpAriMulSet => {dyn Expr},
    ExprOpAriSet    => {dyn Expr},
    ExprOpAriSub    => {dyn Expr},
    ExprOpAriSubSet => {dyn Expr},
    ExprOpBitAnd    => {dyn Expr},
    ExprOpBitOr     => {dyn Expr},
    ExprOpBitNot    => {dyn Expr},
    ExprOpBitShl    => {dyn Expr},
    ExprOpBitShr    => {dyn Expr},
    ExprOpBitXor    => {dyn Expr},
    ExprOpCmpEq     => {dyn Expr},
    ExprOpCmpGt     => {dyn Expr},
    ExprOpCmpGtEq   => {dyn Expr},
    ExprOpCmpLt     => {dyn Expr},
    ExprOpCmpLtEq   => {dyn Expr},
    ExprOpCmpNotEq  => {dyn Expr},
    ExprOpLogAnd    => {dyn Expr},
    ExprOpLogNot    => {dyn Expr},
    ExprOpLogOr     => {dyn Expr},
    ExprReturn      => {dyn Expr},
    ExprVar         => {dyn Expr},
    ExprWhile       => {dyn Expr, dyn Loop},
    Items           => {},
    ItemBuiltins    => {},
    ItemFn          => {dyn Decl, dyn Item},
    LitStr          => {dyn Lit},
    StmtDecl        => {dyn Stmt},
    StmtExpr        => {dyn Stmt},
    StmtExpr        => {dyn Stmt},
    StmtItem        => {dyn Stmt},
    TypeBool        => {dyn Type},
    TypeFn          => {dyn Type},
    TypeInt         => {dyn Type},
    TypeNever       => {dyn Type},
    TypeUnit        => {dyn Type},
    TypeVar         => {dyn Type, dyn TypeRef},
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
