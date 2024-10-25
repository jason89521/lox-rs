use crate::Span;

use super::Expr;

#[derive(Debug)]
pub enum Statement<'a> {
    PrintStmt { span: Span, expr: Expr<'a> },
    ExprStmt { span: Span, expr: Expr<'a> },
}
