use {
  crate::ast,
  lang_base::{token},
  north_core::prelude::*,
  north_derive::aspect_rules,
  north_parser::prelude::*,
  std::{
    fmt::Debug,
    marker::PhantomData,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
struct AssocRule<A>(A);
struct DefaultRule;

aspect_rules! {
  parse_r![ast::AttrPartOf => DefaultRule] {
    fn parse(self, p) {
      let name = p.expect::<token::Ident>()?;
      if name.as_ref() != "part_of" { return Err(()); }
      
      let _    = p.expect::<token::OpParenL>()?;
      let rule = p.expect::<token::Ident>()?.into();
      let _    = p.expect::<token::OpComma>();
      let prec = p.expect::<token::LitInt>().ok();

      let _    = p.expect::<token::OpParenR>()?;
      p.build(ast::AttrPartOf { rule, prec })
    }
  }

  parse_r![ast::AttrTokenGroup => DefaultRule] {
    fn parse(self, p) {
      let name = p.expect::<token::Ident>()?;
      if name.as_ref() != "token_group" { return Err(()); }

      p.build(ast::AttrTokenGroup { })
    }
  }

  parse_r![ast::ExprParse => DefaultRule] {
    fn parse(self, p) {
      let greedy = if p.expect::<token::KwParse>().is_ok() { 
        None
      } else if p.expect::<token::KwParseG>().is_ok() {
        Some(true)
      } else if p.expect::<token::KwParseL>().is_ok() {
        Some(false)
      } else {
        return Err(())
      };

      let value = p.parse::<dyn ast::GExpr>()?;
      p.build(ast::ExprParse { greedy, value })
    }
  }

  parse_r![ast::ExprReject => DefaultRule] {
    fn parse(self, p) {
      let _ = p.expect::<token::KwReject>()?;
      let value = p.parse::<dyn ast::GExpr>()?;
      p.build(ast::ExprReject { value })
    }
  }

  parse_r![ast::GExprBind => DefaultRule] {
    fn parse(self, p) {
      let target = p.expect::<token::Ident>()?;
      let _      = p.expect::<token::OpColon>()?;
      let value  = p.parse_rec::<dyn ast::GExpr>(0)?;
      p.build(ast::GExprBind { target, value })
    }
  }

  parse_r![ast::GExprCall => DefaultRule] {
    fn parse(self, p) {
      let target = p.expect::<token::Ident>()?.into();
      let recursive = p.expect::<token::OpEx>().is_ok();
      let precedence = match recursive {
        true => p.expect::<token::LitInt>().ok()
          .map(|t| t.data.value.parse::<u16>().unwrap()),
        false => None,
      };
      p.build(ast::GExprCall { target, recursive, precedence })
    }
  }

  parse_r![ast::GExprDom => DefaultRule] {
    fn parse(self, p) {
      let _ = p.expect::<token::KwDomG>()?;
      let inner = p.parse::<dyn ast::GExpr>()?.into();
      p.build(ast::GExprDom { scope: 0, inner })
    }
  }

  parse_r![ast::GExprList => DefaultRule] {
    fn parse(self, p) {
      let _ = p.expect::<token::OpParenL>()?;
      let left = p.parse::<dyn ast::GExpr>()?;

      let trailing_sep = if p.expect::<token::OpMod>().is_ok() {
        false
      } else if p.expect::<token::OpModQm>().is_ok() {
        true
      } else {
        return Err(());
      };

      let right = p.parse::<dyn ast::GExpr>()?;
      let _ = p.expect::<token::OpParenR>()?;

      let min_elems = if p.expect::<token::OpMult>().is_ok() {
        0
      } else if p.expect::<token::OpPlus>().is_ok() {
        1
      } else {
        return Err(());
      };

      p.build(ast::GExprList { min_elems, trailing_sep, left, right })
    }
  }

  <L: Clone + Debug> parse_r![ast::GExprLit<L> => DefaultRule] {
    fn parse(self, p) {
      let lit = p.expect::<L>()?;
      p.build(ast::GExprLit { lit })
    }
  }

  <O: Clone + Debug> parse_r![ast::GExprOpBin<O> => AssocRule<LeftAssoc>] {
    const LEFT_RECURSIVE = true;

    fn parse(self, p) {
      let left  = p.parse_rec::<dyn ast::GExpr>(0)?;
      let op    = p.expect::<O>()?;
      let right = p.parse_rec::<dyn ast::GExpr>(1)?;
      p.build(ast::GExprOpBin { op, left, right })
    }
  }

  <O: Clone + Debug> parse_r![ast::GExprOpNAry<O> => DefaultRule] {
    const LEFT_RECURSIVE = true;

    fn parse(self, p) {
      let mut args = Vec::new();

      args.push(p.parse_rec::<dyn ast::GExpr>(1)?);
      while let Ok(_) = p.expect::<O>() {
        args.push(p.parse_rec::<dyn ast::GExpr>(1)?);
      }

      if args.len() >= 2 {
        p.build(ast::GExprOpNAry { op: PhantomData, args })
      } else {
        Err(())
      }
    }
  }

  <T: 'static + Clone + Debug> parse_r![ast::GExprOpU1<T> => DefaultRule] {
    const LEFT_RECURSIVE = true;

    fn parse(self, p) {
      let inner   = p.parse_rec::<dyn ast::GExpr>(0)?;
      let op      = p.expect::<T>()?;
      p.build(ast::GExprOpU1 { op, inner })
    }
  }

  parse_r![ast::GExprPrefShift => DefaultRule] {
    fn parse(self, p) {
      let _ = p.expect::<token::KwShiftP>()?;
      let inner = p.parse::<dyn ast::GExpr>()?;
      p.build(ast::GExprPrefShift { inner })
    }
  }

  parse_r![ast::GExprSeq => DefaultRule] {
    fn parse(self, p) {
      let _       = p.expect::<token::OpParenL>()?;
      let args    = p.parse_list_sep::<dyn ast::GExpr, token::OpComma>()?;
      let _       = p.expect::<token::OpParenR>()?;
      p.build(ast::GExprSeq { args })
    }
  }

  parse_r![ast::ItemBuiltins => DefaultRule] {
    fn parse(self, p) {
      let _ = p.expect::<token::KwBuiltinsG>()?;
      let n = ast::ItemBuiltins::new(p.model_mut());
      p.build(n)
    }
  }

  parse_r![ast::ItemGrammar => DefaultRule] {
    fn parse(self, p) {
      let _ = p.expect::<token::KwGrammar>()?;
      p.build(ast::ItemGrammar { })
    }
  }

  parse_r![ast::ItemGroup => DefaultRule] {
    fn parse(self, p) {
      let attrs = p.parse::<ast::Attrs>().ok();
      let _     = p.expect::<token::KwGroup>()?;
      let name  = p.expect::<token::Ident>()?;
      let _     = p.expect::<token::OpColon>()?;
      let rule  = p.expect::<token::Ident>()?.into();

      let prec;
      if p.expect::<token::OpParenL>().is_ok() {
        prec = Some(p.expect::<token::LitInt>()?);
        let _ = p.expect::<token::OpParenR>()?;
      } else {
        prec = None;
      }

      let _     = p.expect::<token::OpBlockStart>()?;
      let items = p.parse_list::<dyn ast::Item, token::OpBlockEnd>()?;
      let _     = p.expect::<token::OpBlockEnd>()?;
      p.build(ast::ItemGroup { attrs, name, rule, prec, items })
    }
  }

  parse_r![ast::ItemRule => DefaultRule] {
    fn parse(self, p) {
      let attrs   = p.parse::<ast::Attrs>().ok();
      let _       = p.expect::<token::KwRule>()?;
      let name    = p.expect::<token::Ident>()?;
      let params  = p.parse::<ast::DeclRuleParams>()?;
      let ret_ty  = match p.accept::<token::OpMinusGt>() {
        Some(_) => Some(p.parse::<dyn ast::Type>()?),
        None => None,
      };
      let body    = p.parse::<ast::ExprBlock>()?;
      p.build(ast::ItemRule { attrs, name, params, ret_ty, body })
    }
  }

  parse_r![ast::ItemRuleDyn => DefaultRule] {
    fn parse(self, p) {
      let _       = p.expect::<token::KwRuleDyn>()?;
      let name    = p.expect::<token::Ident>()?;
      let params  = p.parse::<ast::DeclRuleParams>()?;
      let ret_ty  = match p.accept::<token::OpMinusGt>() {
        Some(_) => Some(p.parse::<dyn ast::Type>()?),
        None => None,
      };
      let _       = p.expect::<token::OpSemicolon>()?;
      p.build(ast::ItemRuleDyn { name, params, ret_ty })
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ParsePart {
    parse_r![dyn ast::Attr  , ast::AttrPartOf     => 100, DefaultRule],
    parse_r![dyn ast::Attr  , ast::AttrTokenGroup => 100, DefaultRule],
    parse_r![dyn ast::Expr  , ast::ExprParse      => 100, DefaultRule],
    parse_r![dyn ast::Expr  , ast::ExprReject     => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprCall      => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprDom       => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprList      => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprLitClass  => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprLitStr    => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprPrefShift => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprSeq       => 100, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprOpStar    =>  90, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprOpOpt     =>  90, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprOpPlus    =>  90, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprBind      =>  80, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprOpSome    =>  75, DefaultRule],
    parse_r![dyn ast::GExpr , ast::GExprOpAlt     =>  70, DefaultRule],
    parse_r![dyn ast::Item  , ast::ItemBuiltins   => 100, DefaultRule],
    parse_r![dyn ast::Item  , ast::ItemGrammar    => 100, DefaultRule],
    parse_r![dyn ast::Item  , ast::ItemGroup      => 100, DefaultRule],
    parse_r![dyn ast::Item  , ast::ItemRule       => 100, DefaultRule],
    parse_r![dyn ast::Item  , ast::ItemRuleDyn    => 100, DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
