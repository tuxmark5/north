use {
  crate::mir,
  north_core::prelude::*,
  north_derive::aspect_rules,
  north_print::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  print_r![mir::Block => DefaultRule] {
    print_rule! {
      ID ": " {
        #(h in #head: h ";" NL)*
        #tail ";" NL
      }
    }
  }

  print_r![mir::ConstInt => DefaultRule] {
    print_rule! {
      C(#ty) " " P(#value)
    }
  }

  print_r![mir::ConstStruct => DefaultRule] {
    print_rule! {
      "const_struct {" #(e in #elements: C(e) ", ")* "}"
    }
  }

  print_r![mir::CtlBr => DefaultRule] {
    print_rule! {
      "br " ID(#block)
    }
  }

  print_r![mir::CtlIf => DefaultRule] {
    print_rule! {
      "if " #cond ", " ID(#block1) ", " ID(#block0)
    }
  }

  print_r![mir::CtlRet => DefaultRule] {
    print_rule! {
      "ret" #(v in #value: " " v)*
    }
  }

  print_r![mir::CtlSwitch => DefaultRule] {
    print_rule! {
      "switch " {
        "value: " #value NL
        "default: " ID(#default_case) NL
        #((v, b) in #cases: v " => " ID(b) NL)*
      }
    }
  }

  print_r![mir::ItemFn => DefaultRule] {
    print_rule! {
      ID ": fn " {
        "name: " D(#name) NL
        "type: " C(#fn_ty) NL
        "params: " #(p in #params: p ", ")* NL
        "locals: " #(l in #locals: l ", ")* NL
        #(b in #blocks: b NL)*
      }
    }
  }

  print_r![mir::ItemGlob => DefaultRule] {
    print_rule! {
      ID ": glob " {
        "type: " C(#ty) NL
        "init: " D(#init) NL
      }
    }
  }

  print_r![mir::Local => DefaultRule] {
    print_rule! {
      "local " ID
    }
  }

  print_r![mir::LValDeref => DefaultRule] {
    print_rule! {
      "*" #value
    }
  }

  print_r![mir::LValMember => DefaultRule] {
    print_rule! {
      C(#base) #(e in #path: "." D(e))*
    }
  }

  print_r![mir::Mod => DefaultRule] {
    print_rule! {
      ID ": mod " { #(i in #items: i NL)* } NL
    }
  }

  print_r![mir::Param => DefaultRule] {
    print_rule! {
      "param " ID
    }
  }

  print_r![mir::RValMember => DefaultRule] {
    print_rule! {
      C(#base) #(e in #path: "." D(e))*
    }
  }

  print_r![mir::StmtCall => DefaultRule] {
    print_rule! {
      ID " = call " #target "(" #(a in #args: D(a) ", ")* ")"
    }
  }

  print_r![mir::StmtOpBin => DefaultRule] {
    print_rule! {
      ID " = " D(#op) " " #left ", " #right
    }
  }

  print_r![mir::StmtSet => DefaultRule] {
    print_rule! {
      #target " <- " D(#value)
    }
  }

  print_r![mir::StmtStruct => DefaultRule] {
    print_rule! {
      ID " = struct {" #(v in #values: v ", ")* "}"
    }
  }

  print_r![mir::StmtUse => DefaultRule] {
    print_rule! {
      ID " = use " #lvalue
    }
  }

  print_r![mir::TypeFn => DefaultRule] {
    print_rule! {
      "fn(" #(t in #param_tys: C(t) ", ")* ") -> " C(#result_ty)
    }
  }

  print_r![mir::TypeInt => DefaultRule] {
    print_rule! {
      "i" D(#bit_width)
    }
  }

  print_r![mir::TypePtr => DefaultRule] {
    print_rule! {
      "*" C(#target_ty)
    }
  }

  print_r![mir::TypeStruct => DefaultRule] {
    print_rule! {
      "(" #(e in #elements: C(e) ",")* ")"
    }
  }

  print_r![mir::TypeUnit => DefaultRule] {
    print_rule! {
      "()"
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  PrintPart {
    print_r![mir::Block       => DefaultRule],
    print_r![mir::ConstInt    => DefaultRule],
    print_r![mir::ConstStruct => DefaultRule],
    print_r![mir::CtlBr       => DefaultRule],
    print_r![mir::CtlIf       => DefaultRule],
    print_r![mir::CtlRet      => DefaultRule],
    print_r![mir::CtlSwitch   => DefaultRule],
    print_r![mir::ItemFn      => DefaultRule],
    print_r![mir::ItemGlob    => DefaultRule],
    print_r![mir::Local       => DefaultRule],
    print_r![mir::LValDeref   => DefaultRule],
    print_r![mir::LValMember  => DefaultRule],
    print_r![mir::Mod         => DefaultRule],
    print_r![mir::Param       => DefaultRule],
    print_r![mir::RValMember  => DefaultRule],
    print_r![mir::StmtCall    => DefaultRule],
    print_r![mir::StmtOpBin   => DefaultRule],
    print_r![mir::StmtSet     => DefaultRule],
    print_r![mir::StmtStruct  => DefaultRule],
    print_r![mir::StmtUse     => DefaultRule],
    print_r![mir::TypeFn      => DefaultRule],
    print_r![mir::TypeInt     => DefaultRule],
    print_r![mir::TypePtr     => DefaultRule],
    print_r![mir::TypeStruct  => DefaultRule],
    print_r![mir::TypeUnit    => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
