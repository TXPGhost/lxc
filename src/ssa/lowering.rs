use indexmap::IndexMap;

use super::*;
use crate::ast::{self, IdentKind};

/// Context for the lowering process.
#[derive(Debug)]
pub struct Ctxt {
    /// The program being built up.
    prog: Prog,

    /// Some identifier to prefix to all child identifiers (guarantees uniqueness).
    ident_suffix: Vec<String>,

    /// The current function being lowered.
    func: Option<Func>,

    /// The current identifier generator index.
    ident_idx: usize,
}

impl Ctxt {
    /// Name-mangling separator for identifiers.
    pub const MANGLE: char = '@';

    /// Constructs a new context.
    pub fn new() -> Self {
        Self {
            prog: Prog {
                globals: IndexMap::new(),
            },
            ident_suffix: Vec::new(),
            func: None,
            ident_idx: 0,
        }
    }

    /// Returns the currently built program.
    pub fn prog(&self) -> &Prog {
        &self.prog
    }

    /// Joins an identifier with the current suffix to generate a unique identifier.
    fn make_unique(&mut self, ident: Ident) -> Ident {
        if matches!(ident.kind, IdentKind::Void) {
            return self.gen_vid();
        }
        if matches!(ident.kind, IdentKind::BuiltinValue | IdentKind::BuiltinType)
            || self.ident_suffix.is_empty()
        {
            return ident;
        }
        Ident {
            name: std::iter::once(ident.name)
                .chain(self.ident_suffix.iter().rev().cloned())
                .reduce(|acc, s| format!("{}{}{}", acc, Self::MANGLE, s))
                .unwrap_or_default(),
            kind: ident.kind,
        }
    }

    /// Generates a unique value identifier
    fn gen_vid(&mut self) -> Ident {
        self.ident_idx += 1;
        self.make_unique(Ident {
            name: format!("_v{}", self.ident_idx),
            kind: ast::IdentKind::Value,
        })
    }

    /// Generates a unique type identifier
    fn gen_tid(&mut self) -> Ident {
        self.ident_idx += 1;
        self.make_unique(Ident {
            name: format!("_T{}", self.ident_idx),
            kind: ast::IdentKind::Type,
        })
    }

    /// Pushes an identifier suffix to the current context.
    fn push_suffix(&mut self, ident: Ident) {
        self.ident_suffix.push(ident.name);
    }

    /// Pops an identifier sufix from the current context, returning [true] if one was popped.
    fn pop_suffix(&mut self) -> bool {
        self.ident_suffix.pop().is_some()
    }

    /// Pushes a constant literal to the global context, returns an identifier.
    fn push_const(&mut self, lit: Lit) -> Ident {
        let ident = self.gen_vid();
        self.prog.globals.insert(ident.clone(), Global::Lit(lit));
        ident
    }

    /// Pushes a declaration statement to the current function.
    fn push_decl(&mut self, lhs: Ident, rhs: Ident) {
        self.func
            .as_mut()
            .expect("called push_decl without active func")
            .stmts
            .push(Stmt::Decl(Decl { lhs, rhs }))
    }

    /// Pushes a return statement to the current function.
    fn push_return(&mut self, id: Ident) {
        self.func
            .as_mut()
            .expect("called push_return without active func")
            .stmts
            .push(Stmt::Return(id))
    }

    /// Pushes a function call statement to the currently active function, returns an identifier.
    fn push_call(&mut self, func: Ident, args: Vec<Ident>) -> Ident {
        let ident = self.gen_vid();
        self.func
            .as_mut()
            .expect("called push_call without active func")
            .stmts
            .push(Stmt::Call(Call {
                ident: ident.clone(),
                func,
                args,
            }));
        ident
    }

    // Begins a new function lowering.
    fn begin_func(&mut self) {
        if self.func.is_some() {
            panic!("called begin_func with active func");
        }
        self.func = Some(Func {
            params: Vec::new(),
            stmts: Vec::new(),
            ret: None,
        });
    }

    // Pushes the currently active function to the global context.
    fn end_func(&mut self) -> Ident {
        let vid = self.gen_vid();
        self.prog.globals.insert(
            vid.clone(),
            Global::Func(
                self.func
                    .take()
                    .expect("called end_func without active func"),
            ),
        );
        vid
    }
}

impl Default for Ctxt {
    fn default() -> Self {
        Self::new()
    }
}

/// An error that can occur during the AST to SSA lowering process.
#[derive(Debug)]
pub enum LoweringError {
    /// An illegal type was provided.
    MalformedType(ast::Expr),

    /// An illegal type identifier was provided.
    IllegalTypeIdentifier(ast::Ident),
}

/// Loewrs the AST into SSA form.
pub trait Lower {
    /// The SSA lowered type.
    type Ssa;

    /// Attempts to lower `self` into its SSA form.
    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError>;
}

impl Lower for ast::Expr {
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        match self {
            ast::Expr::Ident(ident) => Ok(ctxt.make_unique(ident)),
            ast::Expr::Lit(l) => l.lower(ctxt),
            ast::Expr::Call(c) => c.lower(ctxt),
            ast::Expr::Func(f) => f.lower(ctxt),
            // ast::Expr::Block(b) => b.lower(ctxt),
            // ast::Expr::Proj(p) => p.lower(ctxt),
            ast::Expr::Object(o) => o.lower(ctxt),
            // ast::Expr::Array(a) => a.lower(ctxt),
            // ast::Expr::Vector(v) => v.lower(ctxt),
            _ => unimplemented!(),
        }
    }
}

impl Lower for ast::Lit {
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        Ok(ctxt.push_const(self))
    }
}

impl Lower for ast::Call {
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        let func = self.func.lower(ctxt)?;
        let args: Result<Vec<Ident>, LoweringError> =
            self.args.into_iter().map(|arg| arg.lower(ctxt)).collect();
        let args = args?;
        Ok(ctxt.push_call(func, args))
    }
}

impl Lower for ast::Arg {
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        let vid = self.expr.lower(ctxt)?;
        Ok(vid)
    }
}

impl Lower for ast::Object {
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        let tid = ctxt.gen_tid();
        let fields: Result<Vec<(Ident, Ident)>, LoweringError> = self
            .fields
            .into_iter()
            .map(|field| field.lower(ctxt))
            .collect();
        let fields = fields?;
        ctxt.prog
            .globals
            .insert(tid.clone(), Global::Object(Object { fields }));
        Ok(tid)
    }
}

impl Lower for ast::Field {
    type Ssa = (Ident, Ident);

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        ctxt.push_suffix(self.ident.clone());
        let tid = self.ty.lower(ctxt)?;
        ctxt.pop_suffix();
        let fid = ctxt.make_unique(self.ident);
        Ok((fid, tid))
    }
}

impl Lower for ast::Func {
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        ctxt.begin_func();
        for param in self.params.fields {
            let vid = ctxt.make_unique(param.ident);
            let ty = param.ty.lower(ctxt)?;
            ctxt.func
                .as_mut()
                .unwrap()
                .params
                .push(Param { ident: vid, ty })
        }
        if let Some(body) = self.body {
            for stmt in body.stmts {
                stmt.lower(ctxt)?;
            }
        }
        Ok(ctxt.end_func())
    }
}

impl Lower for ast::Stmt {
    type Ssa = ();

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        match self {
            ast::Stmt::Decl(ast::Decl { ident, expr }) => {
                let rhs = expr.lower(ctxt)?;
                let lhs = ctxt.make_unique(ident);
                ctxt.push_decl(lhs.clone(), rhs);
                Ok(())
            }
            ast::Stmt::Assn(a) => {
                dbg!(a);
                todo!();
            }
            ast::Stmt::Return(expr) => {
                let ident = expr.lower(ctxt)?;
                ctxt.push_return(ident);
                Ok(())
            }
        }
    }
}
