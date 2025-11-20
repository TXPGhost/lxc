use indexmap::IndexMap;

use crate::ssa::{Global, Ident, Lit};

/// Builds the prelude mapping of identifiers to builtin values.
pub fn prelude() -> IndexMap<Ident, Global> {
    let mut prelude = IndexMap::new();
    prelude.insert(Ident::tid("I64"), Global::Lit(Lit::I64T));
    prelude.insert(Ident::tid("F64"), Global::Lit(Lit::F64T));
    prelude
}
