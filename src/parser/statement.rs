use lox_derive::{New, Span};
use lox_span::Span;

use super::expression::{Expression, IdentifierExpression};

#[derive(Debug, Span)]
pub enum Statement<'a> {
    PrintStatement(PrintStatement<'a>),
    ExpressionStatement(ExpressionStatement<'a>),
    VarDeclaration(VarDeclaration<'a>),
    BlockStatement(BlockStatement<'a>),
    IfStatement(Box<IfStatement<'a>>),
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

#[derive(Debug, Span, New)]
pub struct VarDeclaration<'a> {
    pub ident: IdentifierExpression<'a>,
    pub init: Option<Expression<'a>>,
    span: Span,
}

#[derive(Debug, Span, New)]
pub struct BlockStatement<'a> {
    pub stmts: Vec<Statement<'a>>,
    span: Span,
}

#[derive(Debug, Span, New)]
pub struct IfStatement<'a> {
    pub condition: Expression<'a>,
    pub then_branch: Statement<'a>,
    pub else_branch: Option<Statement<'a>>,
    span: Span,
}
