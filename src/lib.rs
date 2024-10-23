pub mod ivl;
mod ivl_ext;

use ivl::{IVLCmd, IVLCmdKind};
use slang::ast::{Cmd, CmdKind, Expr};
use slang_ui::prelude::*;

pub struct App;

impl slang_ui::Hook for App {
    fn analyze(&self, cx: &mut slang_ui::Context, file: &slang::SourceFile) -> Result<()> {
        // Get reference to Z3 solver
        let mut solver = cx.solver()?;

        // Iterate methods
        for m in file.methods() {
            // Get method's preconditions;
            let pres = m.requires();
            // Merge them into a single condition
            let pre = pres
                .cloned()
                .reduce(|a, b| a & b)
                .unwrap_or(Expr::bool(true));
            // Convert the expression into an SMT expression
            let spre = pre.smt()?;
            // Assert precondition
            solver.assert(spre.as_bool()?)?;

            // Get method's body
            let cmd = &m.body.clone().unwrap().cmd;
            // Encode it in IVL
            let ivl = cmd_to_ivlcmd(cmd)?;
            // Calculate obligation and error message (if obligation is not
            // verified)
            let (oblig, msg) = wp(&ivl, &Expr::bool(true))?;
            // Convert obligation to SMT expression
            let soblig = oblig.smt()?;

            // Run the following solver-related statements in a closed scope.
            // That is, after exiting the scope, all assertions are forgotten
            // from subsequent executions of the solver
            solver.scope(|solver| {
                // Check validity of obligation
                solver.assert(!soblig.as_bool()?)?;
                // Run SMT solver on all current assertions
                match solver.check_sat()? {
                    // If the obligations result not valid, report the error (on
                    // the span in which the error happens)
                    smtlib::SatResult::Sat => {
                        cx.error(oblig.span, format!("{msg}"));
                    }
                    smtlib::SatResult::Unknown => {
                        cx.warning(oblig.span, "{msg}: unknown sat result");
                    }
                    smtlib::SatResult::Unsat => (),
                }
                Ok(())
            })?;
        }

        Ok(())
    }
}

// Encoding of (assert-only) statements into IVL (for programs comprised of only
// a single assertion)
fn cmd_to_ivlcmd(cmd: &Cmd) -> Result<IVLCmd> {
    match &cmd.kind {
        CmdKind::Seq(cmd1, cmd2) => {
            let ivl1 = cmd_to_ivlcmd(cmd1)?;
            let ivl2 = cmd_to_ivlcmd(cmd2)?;
            Ok(IVLCmd {
                span: cmd.span.clone(),
                kind: IVLCmdKind::Seq(Box::new(ivl1), Box::new(ivl2)),
            })
        },
        CmdKind::Assert { condition, .. } => Ok(IVLCmd::assert(condition, "Assert might fail!")),
        CmdKind::Assume { condition } => {
            Ok(IVLCmd::assume(condition))
        },
        CmdKind::VarDefinition { name, ty, expr } => {
            if let Some(expr) = expr {
                Ok(IVLCmd::assign(name, expr))
            } else {
                Ok(IVLCmd::havoc(name, &ty.1))
            }
        },
        CmdKind::Assignment { name, expr } => Ok(IVLCmd::assign(name, expr)),
        CmdKind::Match { body } => {
            let mut cases: Vec<IVLCmd> = vec![];
            for case in &body.cases {
                let condition = IVLCmd::assume(&case.condition);
                let cmd = cmd_to_ivlcmd(&case.cmd)?;
                cases.push(IVLCmd::seq(&condition, &cmd));
            }
            Ok(IVLCmd::nondets(&cases))
        },
        _ => todo!("Not supported (yet). cmd_to_ivlcmd"),
    }
}

// Weakest precondition of (assert-only) IVL programs comprised of a single
// assertion
fn wp(ivl: &IVLCmd, post_condition: &Expr) -> Result<(Expr, String)> {
    match &ivl.kind {
        IVLCmdKind::Seq(ivl1, ivl2) => {
            let (wp2, msg2) = wp(ivl2, post_condition)?;
            let (wp1, msg1) = wp(ivl1, &wp2)?;
            Ok((wp1, format!("msg2: {}", msg2)))
        },
        IVLCmdKind::Assert { condition, message } => Ok((condition.clone() & post_condition.clone(), message.clone())),
        IVLCmdKind::Havoc { name, ty } => Ok((post_condition.clone(), "Havoc".to_string())),
        IVLCmdKind::Assignment { expr, name } => Ok((post_condition.subst_ident(&name.ident, expr), format!("{} := {}", name, expr))),
        IVLCmdKind::NonDet(ivl1, ivl2) => {
            let (wp1, msg1) = wp(ivl1, post_condition)?;
            let (wp2, msg2) = wp(ivl2, post_condition)?;
            Ok((wp1.clone().and(&wp2), format!("Msg1: {}, msg2: {}", msg1, msg2)))
        },
        IVLCmdKind::Assume { condition } => Ok((condition.clone() & post_condition.clone(), format!("{} & {}", condition, post_condition))),
        IVLCmdKind::Match { body } => {
            let mut wps: Vec<Expr> = vec![];
            let mut messages: Vec<String> = vec![];
            let mut match_conditions: Vec<Expr> = vec![];

            for case in &body.cases {
                let case_condition = case.condition.clone();

                // Calculate wp for the command inside the branch
                let (case_wp, msg) = wp(&cmd_to_ivlcmd(&case.cmd)?, post_condition)?;

                // Use implication: case_condition => wp(case_cmd)
                let case_wp_with_condition = case_condition.clone().imp(&case_wp);

                wps.push(case_wp_with_condition);
                match_conditions.push(case_condition); // Track conditions for use after match
                messages.push(msg);
            }

            // Combine all the case weakest preconditions using OR
            let combined_wp = wps.into_iter().reduce(|a, b| a | b).unwrap_or(Expr::bool(true));

            // Combine all the messages
            let combined_msg = messages.join(", ");

            // Combine all match conditions (disjunction of all possible match conditions)
            let match_condition = match_conditions.into_iter().reduce(|a, b| a | b).unwrap_or(Expr::bool(true));

            // Return combined wp and message, but also update post-condition
            // to include the match condition so it propagates to the next statement
            let new_post_condition = match_condition.imp(post_condition);
            Ok((combined_wp & new_post_condition, combined_msg))
        },
        _ => todo!("{}", format!("Not supported (yet). wp for {}", ivl)),
    }
}