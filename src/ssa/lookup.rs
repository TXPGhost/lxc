use thiserror::Error;

use crate::ast::IdentKind;

use super::*;

/// Failure to lookup an identifier.
#[derive(Debug, Error)]
#[error("identifier lookup failed")]
pub struct LookupError(Ident);

impl Prog {
    /// Returns the base object of the program.
    pub fn base(&self) -> &Object {
        let Global::Object(base) = self.globals.get(&self.base_id()).unwrap() else {
            unreachable!()
        };
        base
    }

    /// Returns the base object identifier of the program.
    pub fn base_id(&self) -> Ident {
        Ident {
            name: "_T1".to_owned(),
            kind: IdentKind::Type,
        }
    }

    /// Attempts to resolve the given qualified identifier.
    pub fn lookup(&self, ident: &Ident) -> Result<&Global, LookupError> {
        let mut id = ident.clone();
        loop {
            if let Some(global) = self.globals.get(&id) {
                // global lookup
                return Ok(global);
            } else if let Some(id) = self.base().fields.get(&id)
                && let Some(global) = self.globals.get(id)
            {
                // base object lookup
                return Ok(global);
            } else if let Some(global) = self.prelude.get(&id) {
                // prelude lookup
                return Ok(global);
            } else if let Some(idx) = id.name.rfind('@') {
                id = Ident {
                    name: id.name[..idx].to_owned(),
                    kind: id.kind,
                };
            } else {
                return Err(LookupError(ident.clone()));
            }
        }
    }
}
