//! Constant Evalation
//!
//! Takes known value types and materializes them into SSA constants. This removes dependencies on
//! intermediate values thus enabling more aggressive dead code elimination down the line.

use crate::ssa::{Global, Ident, Lit, Prog, Stmt, lowering::LoweringCtxt, type_checking::Type};

impl LoweringCtxt {
    /// Evaluates constants with known types, materializing them into new constant literals.
    pub fn const_eval(&mut self) {
        let idents: Vec<Ident> = self.prog().globals.keys().cloned().collect();
        for ident in idents {
            if let Some(types) = &self.prog().types
                && let Some(Global::Stmt(_)) = self.prog().globals.get(&ident)
                && let Ok(ty) = types.lookup(&ident)
                && ty.is_value()
            {
                let lit = match ty {
                    Type::ConstI64(i) => Lit::Integer(i.to_string()),
                    Type::ConstF64(f) => Lit::Float(f.to_string()),
                    Type::ConstBool(b) => match b {
                        true => Lit::True,
                        false => Lit::False,
                    },
                    Type::Object(fields) => todo!(),
                    _ => unimplemented!(),
                };
                let vid = self.push_const(lit);
                self.types_mut().unwrap().types.insert(vid.clone(), ty);
                self.prog_mut()
                    .globals
                    .insert(ident, Global::Stmt(Stmt::Decl(vid)));
            }
        }
    }
}
