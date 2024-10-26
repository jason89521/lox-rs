use span::Span;

use super::expression::Expression;

#[derive(Debug)]
pub enum Statement<'a> {
    PrintStmt { span: Span, expr: Expression<'a> },
    ExprStmt { span: Span, expr: Expression<'a> },
}
