use lox_derive::{New, Span};
use lox_span::Span;

use super::expression::Expression;

#[derive(Debug, Span)]
pub enum Statement<'a> {
    PrintStatement(PrintStatement<'a>),
    ExpressionStatement(ExpressionStatement<'a>),
}

#[derive(Debug, Span, New)]
pub struct PrintStatement<'a> {
    pub expr: Expression<'a>,
    span: Span,
}

#[derive(Debug, Span, New)]
pub struct ExpressionStatement<'a> {
    pub expr: Expression<'a>,
    span: Span,
}
