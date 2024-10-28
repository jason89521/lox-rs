use lox_derive::{New, Span};
use lox_span::Span;

use super::Operator;

#[derive(Debug, Span, Clone)]
pub enum Expression<'a> {
    LiteralExpression(LiteralExpression<'a>),
    ParenExpression(Box<ParenExpression<'a>>),
    UnaryExpression(Box<UnaryExpression<'a>>),
    BinaryExpression(Box<BinaryExpression<'a>>),
    IdentifierExpression(IdentifierExpression<'a>),
    AssignmentExpression(Box<AssignmentExpression<'a>>),
    LogicalExpression(Box<LogicalExpression<'a>>),
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
            Expression::IdentifierExpression(identifier_expression) => {
                write!(f, "{}", identifier_expression.name)
            }
            Expression::AssignmentExpression(assignment_expression) => write!(
                f,
                "(= {} {})",
                assignment_expression.ident.name, assignment_expression.expr
            ),
            Expression::LogicalExpression(expr) => {
                write!(f, "({} {} {})", expr.op, expr.lhs_expr, expr.rhs_expr)
            }
        }
    }
}

#[derive(Debug, Span, New, Clone)]
pub struct LiteralExpression<'a> {
    pub kind: LiteralKind<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct ParenExpression<'a> {
    pub expr: Expression<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct UnaryExpression<'a> {
    pub op: Operator,
    pub expr: Expression<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct BinaryExpression<'a> {
    pub lhs_expr: Expression<'a>,
    pub op: Operator,
    pub rhs_expr: Expression<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct IdentifierExpression<'a> {
    pub name: &'a str,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct AssignmentExpression<'a> {
    pub ident: IdentifierExpression<'a>,
    pub expr: Expression<'a>,
    span: Span,
}

#[derive(Debug, Span, New, Clone)]
pub struct LogicalExpression<'a> {
    pub lhs_expr: Expression<'a>,
    pub op: Operator,
    pub rhs_expr: Expression<'a>,
    span: Span,
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
