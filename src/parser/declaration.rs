use super::{
    expression::{Expression, IdentifierExpression},
    Statement,
};
use lox_derive::{New, Span};
use lox_span::Span;

#[derive(Debug, Span, Clone)]
pub enum Declaration<'a> {
    VarDeclaration(VarDeclaration<'a>),
    Statement(Statement<'a>),
}

#[derive(Debug, Span, New, Clone)]
pub struct VarDeclaration<'a> {
    pub ident: IdentifierExpression<'a>,
    pub init: Option<Expression<'a>>,
    span: Span,
}
