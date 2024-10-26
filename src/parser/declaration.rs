use span::Span;

use super::Expression;

#[derive(Debug)]
pub enum Declaration<'a> {
    VarDeclaration {
        span: Span,
        init: Option<Expression<'a>>,
    },
}
