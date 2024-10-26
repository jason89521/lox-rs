use span::Span;
use span_derive::Span;

use super::{expression::IdentifierExpression, Expression};

#[derive(Debug)]
pub enum Declaration<'a> {
    VarDeclaration(VarDeclaration<'a>),
}

#[derive(Debug, Span)]
pub struct VarDeclaration<'a> {
    span: Span,
    pub init: Option<Expression<'a>>,
    pub ident: IdentifierExpression<'a>,
}

impl<'a> VarDeclaration<'a> {
    pub fn new(ident: IdentifierExpression<'a>, init: Option<Expression<'a>>, span: Span) -> Self {
        Self { span, init, ident }
    }
}
