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
            unreachable!("base identifier should always be an object")
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

    /// Finds the main function of the program.
    pub fn main_id(&self) -> Option<Ident> {
        let base = self.base();
        let mut main = None;
        for (ident, field) in &base.fields {
            if ident.name == "main" {
                main = Some(field);
                break;
            }
        }
        main.cloned()
    }

    /// Attempts to resolve the given qualified identifier.
    /// TODO: location-dependent lookup (i.e. lookup "before" something is defined shouldn't work)
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
                // relax search to parent scope
                id = Ident {
                    name: id.name[..idx].to_owned(),
                    kind: id.kind,
                };
            } else {
                // lookup has failed
                return Err(LookupError(ident.clone()));
            }
        }
    }
}
