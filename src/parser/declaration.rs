use lox_derive::{New, Span};
use lox_span::Span;

use super::{expression::IdentifierExpression, Expression};

#[derive(Debug, Span)]
pub enum Declaration<'a> {
    VarDeclaration(VarDeclaration<'a>),
}

#[derive(Debug, Span, New)]
pub struct VarDeclaration<'a> {
    pub ident: IdentifierExpression<'a>,
    pub init: Option<Expression<'a>>,
    span: Span,
}
