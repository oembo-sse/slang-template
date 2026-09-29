pub mod ivl;
mod ivl_ext;

use ivl::{IVLCmd, IVLCmdKind};
use slang::ast::{Cmd, CmdKind, Expr};
use slang_ui::prelude::{slang::Span, *};

pub struct App;

impl slang_ui::Hook for App {
    fn analyze(&self, cx: &slang_ui::Context, file: &slang::SourceFile) -> Result<()> {
        // get reference to z3 solver
        let mut solver = cx.solver()?;

        // iterate all methods
        for m in file.methods() {
            // convert all the preconditions into smt
            let pres: Vec<smtlib::Bool<'_>> = m
                .requires()
                .map(|pre| Ok(pre.smt(cx.smt_st())?.as_bool()?))
                .collect::<Result<_>>()?;

            // get method body, convert to IVL
            let cmd = &m.body.clone().unwrap().cmd;
            let ivl = cmd_to_ivlcmd(cmd)?;

            // compute single proof obligation using postcondition true
            let (oblig, oblig_span, msg) = wp(&ivl, &Expr::bool(true))?;

            // put all obligations into a list of oblications with span for error localication
            let obligations = vec![(oblig.smt(cx.smt_st())?.as_bool()?, oblig_span, msg)];

            // open a solver scope for this method
            solver.scope(|solver| {
                // assert all preconditions once per method
                for pre in pres {
                    solver.assert(pre)?;
                }

                for (soblig, span, msg) in obligations {
                    // open a solver scope for this obligation
                    solver.scope(|solver| {
                        // assert the negation of theobligation
                        solver.assert(!soblig)?;
                        // check sat of !obligation
                        match solver.check_sat()? {
                            smtlib::SatResult::Sat => {
                                cx.error(span, msg.to_string());
                            }
                            smtlib::SatResult::Unknown => {
                                cx.warning(span, format!("{msg}: unknown sat result"));
                            }
                            smtlib::SatResult::Unsat => (),
                        }

                        Ok(())
                    })?;
                }

                Ok(())
            })?;
        }

        Ok(())
    }
}

/// Encode a Cmd into IVL
fn cmd_to_ivlcmd(cmd: &Cmd) -> Result<IVLCmd> {
    match &cmd.kind {
        CmdKind::Assert { condition, message } => Ok(IVLCmd::assert(condition, message)),
        c => bail!("not yet implemented: cmd_to_ivlcmd {c:?}"),
    }
}

/// Compute the weakest precondition of IVL program
fn wp(ivl: &IVLCmd, post: &Expr) -> Result<(Expr, Span, String)> {
    match &ivl.kind {
        IVLCmdKind::Assert { condition, message } => {
            Ok((condition.clone(), condition.span, message.clone()))
        }
        c => bail!("not yet implemented: wp of {:?}", c),
    }
}
