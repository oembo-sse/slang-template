use slang::{ast::{Expr, Name, Type}, Span};
use slang_ui::prelude::*;

use crate::ivl0::{IVL0Stmt, IVL0StmtKind};

impl IVL0Stmt {
    pub(crate) fn assign(name: &Name, expr: &Expr) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::Assignment { name: name.clone(), expr: expr.clone() }
        }
    }
    pub(crate) fn seq(&self, other: &IVL0Stmt) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::Seq(Box::new(self.clone()), Box::new(other.clone()))
        }
    }
    pub(crate) fn seqs(stmts: &Vec<IVL0Stmt>) -> IVL0Stmt {
        stmts.iter().cloned()
        .reduce(|a, b| IVL0Stmt::seq(&a, &b))
        .unwrap_or(IVL0Stmt::nop())
    }
    pub(crate) fn nondet(&self, other: &IVL0Stmt) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::NonDet(Box::new(self.clone()), Box::new(other.clone()))
        }
    }
    pub(crate) fn nondets(stmts: &Vec<IVL0Stmt>) -> IVL0Stmt {
        stmts.iter().cloned()
            .reduce(|a, b| IVL0Stmt::nondet(&a, &b))
            .unwrap_or(IVL0Stmt::unreachable())
    }
    pub(crate) fn assume(condition: &Expr) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::Assume { condition: condition.clone() }
        }
    }
    pub(crate) fn assert(condition: &Expr, message: &str) -> IVL0Stmt {
        IVL0Stmt {
            span: Span::default(),
            kind: IVL0StmtKind::Assert { condition: condition.clone(), message: message.to_owned() },
        }
    }
    pub(crate) fn havoc(name: &Name, ty: &Type) -> IVL0Stmt {
        IVL0Stmt {
            kind: IVL0StmtKind::Havoc { name: name.clone(), ty: ty.clone() },
            span: Span::default()
        }
    }
    pub(crate) fn nop() -> IVL0Stmt {
        IVL0Stmt::assume(&Expr::bool(true))
    }
    pub(crate) fn unreachable() -> IVL0Stmt {
        IVL0Stmt::assume(&Expr::bool(false))
    }
}

impl std::fmt::Display for IVL0Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            IVL0StmtKind::Assignment { name, expr } => write!(f, "{name} := {expr}"),
            IVL0StmtKind::Havoc { name, .. } => write!(f, "havoc {name}"),
            IVL0StmtKind::Assume { condition } => write!(f, "assume {condition}"),
            IVL0StmtKind::Assert { condition, .. } => write!(f, "assert {condition}"),
            IVL0StmtKind::Seq(c1, c2) => write!(f, "{c1} ; {c2}"),
            IVL0StmtKind::NonDet(c1, c2) => write!(f, "{{ {c1} }} [] {{ {c2} }}"),
        }
    }
}
