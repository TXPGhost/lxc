use thiserror::Error;

use super::*;

/// Failure to lookup an identifier.
#[derive(Debug, Error)]
#[error("identifier lookup failed")]
pub struct LookupError(Ident);

impl Prog {
    /// Attempts to resolve the given qualified identifier.
    pub fn lookup(&self, ident: &Ident) -> Result<&Global, LookupError> {
        let mut id = ident.clone();
        loop {
            if let Some(global) = self.globals.get(&id) {
                return Ok(global);
            } else if let Some(idx) = id.name.find('@') {
                id = Ident {
                    name: id.name[idx + 1..].to_owned(),
                    kind: id.kind,
                };
            } else {
                return Err(LookupError(ident.clone()));
            }
        }
    }
}
