use span::{GetSpan, Span};
use span_derive::Span;

use super::expression::Expression;

#[derive(Debug)]
pub enum Statement<'a> {
    PrintStatement(PrintStatement<'a>),
    ExpressionStatement(ExpressionStatement<'a>),
}

impl GetSpan for Statement<'_> {
    fn span(&self) -> Span {
        match self {
            Statement::PrintStatement(stmt) => stmt.span(),
            Statement::ExpressionStatement(stmt) => stmt.span(),
        }
    }
}

#[derive(Debug, Span)]
pub struct PrintStatement<'a> {
    span: Span,
    pub expr: Expression<'a>,
}

impl<'a> PrintStatement<'a> {
    pub fn new(expr: Expression<'a>, span: Span) -> Self {
        Self { span, expr }
    }
}

#[derive(Debug, Span)]
pub struct ExpressionStatement<'a> {
    span: Span,
    pub expr: Expression<'a>,
}

impl<'a> ExpressionStatement<'a> {
    pub fn new(expr: Expression<'a>, span: Span) -> Self {
        Self { span, expr }
    }
}
