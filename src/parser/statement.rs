use lox_derive::{New, Span};
use lox_span::Span;

use super::{expression::Expression, Declaration};

#[derive(Debug, Span, Clone)]
pub enum Statement<'a> {
    PrintStatement(PrintStatement<'a>),
    ExpressionStatement(ExpressionStatement<'a>),
    BlockStatement(BlockStatement<'a>),
    IfStatement(Box<IfStatement<'a>>),
    WhileStatement(Box<WhileStatement<'a>>),
    ForLoopStatement(Box<ForLoopStatement<'a>>),
}

#[derive(Debug, Span, New, Clone)]
pub struct PrintStatement<'a> {
    pub expr: Expression<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct ExpressionStatement<'a> {
    pub expr: Expression<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct BlockStatement<'a> {
    pub decl: Vec<Declaration<'a>>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct IfStatement<'a> {
    pub condition: Expression<'a>,
    pub then_branch: Statement<'a>,
    pub else_branch: Option<Statement<'a>>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct WhileStatement<'a> {
    pub condition: Expression<'a>,
    pub block: Statement<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct ForLoopStatement<'a> {
    pub init: Option<Declaration<'a>>,
    pub condition: Option<Expression<'a>>,
    pub step: Option<Expression<'a>>,
    pub block: Statement<'a>,
    span: Span,
}
