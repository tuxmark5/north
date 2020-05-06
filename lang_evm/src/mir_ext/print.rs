use {
  crate::{mir},
  north_derive::aspect_rules,
  north_print::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  print_r![mir::CtlExecDFA => DefaultRule] {
    print_rule! {
      ID " exec_dfa " D(#dfa) " " {
        #((i, b) in E(#next): D(&i) ": " ID(b) NL)* 
      }
    }
  }

  print_r![mir::CtlFail => DefaultRule] {
    print_rule! {
      ID " fail"
    }
  }

  print_r![mir::CtlFork => DefaultRule] {
    print_rule! {
      ID " fork " #(b in #blocks: ID(b) ", ")*
    }
  }

  print_r![mir::CtlLoopBegin => DefaultRule] {
    print_rule! {
      ID " loop_begin var: " #var ", next: " ID(#next)
    }
  }

  print_r![mir::CtlLoopEnd => DefaultRule] {
    print_rule! {
      ID " loop_end " ID(#var) " " {
        "fail: " ID(#fail) NL
        #((r, b) in #ok: D(&r) " => " D(b) NL)*
      }
    }
  }

  print_r![mir::CtlLoopNext => DefaultRule] {
    print_rule! {
      ID " loop_next var: " #var
      ", max_iter: " D(#max_iter)
      ", ok: " ID(#ok)
      ", complete: " D(#complete)
    }
  }

  print_r![mir::CtlMatchClass => DefaultRule] {
    print_rule! {
      ID " match_class s=" D(#prefer_shift) " " {
        #((k, v) in #ok: D(&k) " => " ID(v) NL)*
      }
    }
  }

  print_r![mir::CtlMatchChar => DefaultRule] {
    print_rule! {
      ID " match_char " 
      "s=" D(#prefer_shift) ", "
      "ok=" D(#char) " => " ID(#ok) ", fail: " ID(#fail)
    }
  }

  print_r![mir::CtlMatchSym => DefaultRule] {
    print_rule! {
      ID " match_sym " {
        #(e in #pos: 
          "+ " D(&e.match_id) ":" D(&e.prec_range)
          " => " ID(&e.block) NL
        )*
        #(e in #neg: 
          "- " D(&e.match_id) ":" D(&e.prec_range)
          " => " ID(&e.block) NL
        )*
      }
    }
  }

  print_r![mir::CtlReduce => DefaultRule] {
    print_rule! {
      ID " reduce " 
        "kind: " D(#kind) ", "
        "id: " D(#reduce_id) ", "
        "short: " D(#short) ", "
        "next: " ID(#next)
    }
  }

  print_r![mir::ItemGrammar => DefaultRule] {
    print_rule! {
      ID ": grammar " {
        #(e in #impls: 
          D(&e.match_id) " => " 
          D(&e.reduce_id) ", " D(&e.prec) ", " ID(&e.rule) NL
        )*
      }
    }
  }

  print_r![mir::ItemRule => DefaultRule] {
    print_rule! {
      ID ": rule " {
        "type: " C(#rule_ty) NL
        "params: " #(p in #params: p ", ")* NL
        "locals: " #(l in #locals: l ", ")* NL
        #(b in #blocks: b NL)*
      }
    }
  }

  print_r![mir::RuleLocal => DefaultRule] {
    print_rule! {
      "rule_local " ID
    }
  }

  print_r![mir::RuleParam => DefaultRule] {
    print_rule! {
      "rule_param " ID
    }
  }

  print_r![mir::StmtCallRule => DefaultRule] {
    print_rule! {
      ID " = call_rule " D(#target) 
      ", prec: " D(#min_prec)
      ", args: (" #(a in #args: a ", ")* ")"
    }
  }

  print_r![mir::StmtCallRuleDyn => DefaultRule] {
    print_rule! {
      ID " = call_rule_dyn " D(#call_spec) 
      ", args: (" #(a in #args: a ", ")* ")"
    }
  }

  print_r![mir::StmtLoopBegin => DefaultRule] {
    print_rule! {
      ID " loop_begin var: " #var ", depth: " D(#depth)
    }
  }

  print_r![mir::StmtReduce => DefaultRule] {
    print_rule! {
      ID " reduce " 
        "kind: " D(#kind) ", "
        "id: " D(#reduce_id)
    }
  }

  print_r![mir::StmtRewind => DefaultRule] {
    print_rule! {
      ID " rewind " D(#delta)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  PrintPart {
    print_r![mir::CtlExecDFA      => DefaultRule],
    print_r![mir::CtlFail         => DefaultRule],
    print_r![mir::CtlFork         => DefaultRule],
    print_r![mir::CtlLoopBegin    => DefaultRule],
    print_r![mir::CtlLoopEnd      => DefaultRule],
    print_r![mir::CtlLoopNext     => DefaultRule],
    print_r![mir::CtlMatchClass   => DefaultRule],
    print_r![mir::CtlMatchChar    => DefaultRule],
    print_r![mir::CtlMatchSym     => DefaultRule],
    print_r![mir::CtlReduce       => DefaultRule],
    print_r![mir::ItemGrammar     => DefaultRule],
    print_r![mir::ItemRule        => DefaultRule],
    print_r![mir::RuleLocal       => DefaultRule],
    print_r![mir::RuleParam       => DefaultRule],
    print_r![mir::StmtCallRule    => DefaultRule],
    print_r![mir::StmtCallRuleDyn => DefaultRule],
    print_r![mir::StmtLoopBegin   => DefaultRule],
    print_r![mir::StmtReduce      => DefaultRule],
    print_r![mir::StmtRewind      => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
