//! Dead Code Elimination
//!
//! Construct a graph of dependencies (A depends on B if the value of A changes based on the value
//! of B). Importantly, if A's value is known at compile time, we say that it has no dependencies.
//! Then, we conduct a graph search algorithm to determine reachability, and eliminate all "dead"
//! identifiers.

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::{ast::IdentKind, ssa::*, style::comma_join};

/// Dead code elimination context.
pub struct DceCtxt<'a> {
    /// Adjacently list for a directed graph of identifiers. Identifier A points to identifier B if
    /// A references B. Then to perform dead code elimination we can simply conduct reachability
    /// analysis on this graph.
    pub refs: HashMap<Ident, Vec<Ident>>,

    /// The program itself being analyzed.
    pub prog: &'a Prog,
}

impl<'a> DceCtxt<'a> {
    /// Constructs a new dead code elimination context.
    pub fn new(prog: &'a Prog) -> Self {
        Self {
            refs: HashMap::new(),
            prog,
        }
    }

    /// Adds an edge to the graph.
    pub fn add_edge(&mut self, from: Ident, to: Ident) {
        self.refs.entry(from).or_default().push(to)
    }

    /// Adds a collection of edges to the graph.
    pub fn add_edges(&mut self, from: Ident, to: &[Ident]) {
        self.refs.entry(from).or_default().extend_from_slice(to)
    }

    fn dfs(&self, ident: &Ident, discovered: &mut HashSet<Ident>) {
        if discovered.contains(ident) {
            return;
        }
        discovered.insert(ident.clone());
        if let Some(neighbors) = self.refs.get(ident) {
            for neighbor in neighbors {
                self.dfs(neighbor, discovered);
            }
        }
    }

    /// Computes the set of reachable identifiers from the given starting set.
    pub fn compute_reachability(&self, starting_set: &[Ident]) -> HashSet<Ident> {
        let mut discovered = HashSet::new();
        for ident in starting_set {
            self.dfs(ident, &mut discovered);
        }
        discovered
    }
}

impl Prog {
    /// Eliminates dead code by removing all identifiers not listed in the provided set.
    pub fn eliminte_dead_code(&mut self, reachable_identifiers: &HashSet<Ident>) {
        let keys: Vec<Ident> = self.globals.keys().cloned().collect();

        // eliminate dead globals
        for ident in keys {
            if !reachable_identifiers.contains(&ident) {
                self.globals.swap_remove(&ident);
            }
        }

        // eliminate unreachable statements, fields, etc.
        for global in self.globals.values_mut() {
            match global {
                Global::Func(func) => {
                    func.stmts
                        .retain(|stmt| reachable_identifiers.contains(stmt));
                }
                Global::Object(object) => object
                    .fields
                    .retain(|field, _| reachable_identifiers.contains(field)),
                _ => {}
            }
        }
    }
}

/// Indicates that a type is capable of dead code elimination analysis.
pub trait DceAnalyze {
    /// Builds a reference graph for this type, writing into the provided context. Returns a list
    /// of identifiers referenced by this SSA node.
    fn build_dce_graph(&self, ctxt: &mut DceCtxt) -> Vec<Ident>;
}

impl DceAnalyze for Ident {
    fn build_dce_graph(&self, ctxt: &mut DceCtxt) -> Vec<Ident> {
        if ctxt.refs.contains_key(self) {
            println!("visiting twice: {}", self.name);
            // return ctxt.refs[self].clone();
        }
        let Ok(global) = ctxt.prog.lookup(self) else {
            panic!("unresolved identifier {}", self.name);
        };
        let neighbors = global.build_dce_graph(ctxt);
        ctxt.add_edges(self.clone(), &neighbors);
        vec![self.clone()]
    }
}

impl DceAnalyze for Global {
    fn build_dce_graph(&self, ctxt: &mut DceCtxt) -> Vec<Ident> {
        match self {
            Global::Lit(lit) => lit.build_dce_graph(ctxt),
            Global::Func(func) => func.build_dce_graph(ctxt),
            Global::Object(object) => object.build_dce_graph(ctxt),
            Global::Stmt(stmt) => stmt.build_dce_graph(ctxt),
            Global::Param(ident) => ident.build_dce_graph(ctxt),
        }
    }
}

impl DceAnalyze for Func {
    fn build_dce_graph(&self, ctxt: &mut DceCtxt) -> Vec<Ident> {
        // TODO: handle mutable arguments
        for stmt in &self.stmts {
            stmt.build_dce_graph(ctxt);
        }
        if let Some(ret) = &self.ret {
            ret.build_dce_graph(ctxt);
            vec![ret.clone()]
        } else {
            vec![]
        }
    }
}

impl DceAnalyze for Stmt {
    fn build_dce_graph(&self, ctxt: &mut DceCtxt) -> Vec<Ident> {
        match self {
            Stmt::Call(call) => call.build_dce_graph(ctxt),
            Stmt::Decl(ident) => ident.build_dce_graph(ctxt),
        }
    }
}

impl DceAnalyze for Call {
    fn build_dce_graph(&self, ctxt: &mut DceCtxt) -> Vec<Ident> {
        let mut res = vec![self.func.clone()];
        self.func.build_dce_graph(ctxt);
        for arg in &self.args {
            arg.build_dce_graph(ctxt);
            res.push(arg.clone());
        }
        res
    }
}

impl DceAnalyze for Lit {
    fn build_dce_graph(&self, _: &mut DceCtxt) -> Vec<Ident> {
        vec![]
    }
}

impl DceAnalyze for Object {
    fn build_dce_graph(&self, ctxt: &mut DceCtxt) -> Vec<Ident> {
        let mut res = vec![];
        for (ident, field) in &self.fields {
            field.build_dce_graph(ctxt);
            ctxt.add_edge(ident.clone(), field.clone());
            res.push(ident.clone());
        }
        res
    }
}

impl Display for DceCtxt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (ident, neighbors) in &self.refs {
            writeln!(
                f,
                "{} -> {{{}}}",
                ident.name,
                neighbors
                    .iter()
                    .map(|ident| ident.name.to_owned())
                    .reduce(comma_join)
                    .unwrap_or_default()
            )?;
        }
        Ok(())
    }
}
