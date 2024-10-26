use span::Span;
use span_derive::Span;

use super::Operator;

#[derive(Debug)]
pub enum Expression<'a> {
    LiteralExpression(LiteralExpression<'a>),
    ParenExpression(ParenExpression<'a>),
    UnaryExpression(UnaryExpression<'a>),
    BinaryExpression(BinaryExpression<'a>),
}

impl std::fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::LiteralExpression(literal_expression) => {
                write!(f, "{}", literal_expression.kind)
            }
            Expression::ParenExpression(paren_expression) => {
                write!(f, "(group {})", paren_expression.expr)
            }
            Expression::UnaryExpression(unary_expression) => {
                write!(f, "({} {})", unary_expression.op, unary_expression.expr)
            }
            Expression::BinaryExpression(binary_expression) => write!(
                f,
                "({} {} {})",
                binary_expression.op, binary_expression.lhs_expr, binary_expression.rhs_expr
            ),
        }
    }
}

impl span::GetSpan for Expression<'_> {
    fn span(&self) -> Span {
        match self {
            Expression::LiteralExpression(expr) => expr.span(),
            Expression::ParenExpression(expr) => expr.span(),
            Expression::UnaryExpression(expr) => expr.span(),
            Expression::BinaryExpression(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Span)]
pub struct LiteralExpression<'a> {
    pub kind: LiteralKind<'a>,
    span: Span,
}

impl<'a> LiteralExpression<'a> {
    pub fn new(kind: LiteralKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Span)]
pub struct ParenExpression<'a> {
    pub expr: Box<Expression<'a>>,
    span: Span,
}

impl<'a> ParenExpression<'a> {
    pub fn new(expr: Box<Expression<'a>>, span: Span) -> Self {
        Self { expr, span }
    }
}

#[derive(Debug, Span)]
pub struct UnaryExpression<'a> {
    pub op: Operator,
    pub expr: Box<Expression<'a>>,
    span: Span,
}

impl<'a> UnaryExpression<'a> {
    pub fn new(op: Operator, expr: Box<Expression<'a>>, span: Span) -> Self {
        Self { op, expr, span }
    }
}

#[derive(Debug, Span)]
pub struct BinaryExpression<'a> {
    pub lhs_expr: Box<Expression<'a>>,
    pub op: Operator,
    pub rhs_expr: Box<Expression<'a>>,
    span: Span,
}

impl<'a> BinaryExpression<'a> {
    pub fn new(
        lhs_expr: Box<Expression<'a>>,
        op: Operator,
        rhs_expr: Box<Expression<'a>>,
        span: Span,
    ) -> Self {
        Self {
            lhs_expr,
            op,
            rhs_expr,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LiteralKind<'a> {
    Number(f64),
    String(&'a str),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for LiteralKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::Boolean(b) => write!(f, "{b}"),
            LiteralKind::Nil => write!(f, "nil"),
            LiteralKind::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            LiteralKind::String(s) => write!(f, "{s}"),
        }
    }
}
