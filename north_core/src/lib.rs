#![feature(associated_type_defaults)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(coerce_unsized)]
#![feature(const_fn)]
#![feature(const_fn_union)]
#![feature(const_type_id)]
#![feature(core_intrinsics)]
#![feature(decl_macro)]
#![feature(generators)]
#![feature(generator_trait)]
#![feature(raw)]
#![feature(specialization)]
#![feature(type_ascription)]
#![feature(unsize)]

#![feature(const_raw_ptr_to_usize_cast)]
#![feature(untagged_unions)]

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use]
extern crate scoped_tls;

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use] pub mod cell;
#[macro_use] pub mod compiler;
#[macro_use] pub mod context;
#[macro_use] pub mod util;

pub mod iter;
pub mod location;
pub mod model;
pub mod node_id;
pub mod pos;
pub mod quote;
pub mod structure;
pub mod trait_manager;
pub mod visitor;

pub use {
  crate::{
    location::Location,
    model::{
      Model,
      member::member_descr::{phantom, union_cast},
      node::Node,
    },
    node_id::NodeId,
    util::{
      downcast::Downcast,
      token::{Token, TokenExt, TokenOpaque},
    }
  },
  ::flame,
  ::futures,
  ::log,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod concept_map_prelude {
  pub use {
    crate::{
      model::{Concept, language::ConceptMap},
      trait_manager::TraitManager,
    },
    std::{
      iter,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod derive_prelude {
  pub use {
    crate::{
      model::{
        concept::Concept,
        concept_descr::ConceptDescr,
        member::MemberDescr,
        node::Node,
      },
    },
    std::{
      any::Any,
      marker::PhantomData,
    }
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod lang_prelude {
  pub use {
    crate::{
      language_parts,
      model::Language,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod macro_prelude {
  pub use {
    crate::{
      compiler::{
        Compiler,
        aspect::{Aspect, AspectPart, AspectPartDyn, VoidAspect}
      },
      model::language::{AspectParts},
      trait_manager::TraitManager,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      // Macros
      concept_map_part, context_impl, field_offset_of, 
      impl_prop, lang_part, language_parts,

      // Other stuff
      compiler::{COMPILER, Compiler},
      model::{
        Model, ModelCell,
        node::{Node},
      },
      node_id::{NodeId, ToNodeId},
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod rule_prelude {
  pub use {
    crate::{
      Node, NodeId,
      compiler::{
        rule::{
          RuleDynAdapter,
          RuleKeySingleNode,
          RuleMap,
        }
      },
      context::{
        ModelCellCtxExt,
        ModelNodeIdCtxExt,
        NodeIdCtxExt,
      },
      model::ModelCell,
      util::dynamic_cast,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////
