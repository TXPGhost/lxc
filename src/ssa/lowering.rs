use indexmap::IndexMap;

use super::*;
use crate::ast;

/// Context for the lowering process.
#[derive(Debug)]
pub struct Ctxt {
    /// The program being built up.
    prog: Prog,

    /// Some identifier to prefix to all child identifiers (guarantees uniqueness).
    ident_prefix: String,

    /// The current function being lowered.
    func: Option<Func>,

    /// The current identifier generator index.
    ident_idx: usize,
}

impl Ctxt {
    /// Joins an identifier with the current prefix to generate a unique identifier.
    fn make_unique(&self, ident: Ident) -> Ident {
        Ident {
            name: format!("{}${}", self.ident_prefix, ident.name),
            kind: ident.kind,
        }
    }

    /// Generates a unique value identifier
    fn gen_vid(&mut self) -> Ident {
        self.ident_idx += 1;
        self.make_unique(Ident {
            name: format!("v{}", self.ident_idx),
            kind: ast::IdentKind::Value,
        })
    }

    /// Generates a unique type identifier
    fn gen_tid(&mut self) -> Ident {
        self.ident_idx += 1;
        self.make_unique(Ident {
            name: format!("T{}", self.ident_idx),
            kind: ast::IdentKind::Type,
        })
    }

    /// Pushes an identifier prefix to the current context.
    fn push_prefix(&mut self, ident: Ident) {
        self.ident_prefix = self.make_unique(ident).name
    }

    /// Pops an identifier prefix from the current context, returning [true] if one was popped.
    fn pop_prefix(&mut self) -> bool {
        match self.ident_prefix.match_indices('$').next_back() {
            Some(index) => {
                self.ident_prefix = self.ident_prefix[..index.0].to_owned();
                true
            }
            None => false,
        }
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

    /// Attempts to lower `self` into an SSA program.
    fn lower_prog(self) -> Result<(Prog, Self::Ssa), LoweringError>
    where
        Self: Sized,
    {
        let mut ctxt = Ctxt {
            prog: Prog {
                globals: IndexMap::new(),
            },
            ident_prefix: String::new(),
            func: None,
            ident_idx: 0,
        };
        let ident = self.lower(&mut ctxt)?;
        Ok((ctxt.prog, ident))
    }
}

impl Lower for ast::Expr {
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        match self {
            ast::Expr::Ident(ident) => Ok(ident),
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
        let fields: Result<Vec<Ident>, LoweringError> = self
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
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        ctxt.push_prefix(self.ident);
        let tid = self.ty.lower(ctxt)?;
        ctxt.pop_prefix();
        Ok(tid)
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
    type Ssa = Ident;

    fn lower(self, ctxt: &mut Ctxt) -> Result<Self::Ssa, LoweringError> {
        match self {
            ast::Stmt::Decl(ast::Decl { ident, expr }) => {
                let rhs = expr.lower(ctxt)?;
                let lhs = ctxt.make_unique(ident);
                ctxt.push_decl(lhs.clone(), rhs);
                Ok(lhs)
            }
            ast::Stmt::Assn(_) => todo!(),
            ast::Stmt::Return(_) => todo!(),
        }
    }
}
