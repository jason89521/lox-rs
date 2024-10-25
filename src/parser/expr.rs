use crate::Span;

use super::Operator;

#[derive(Debug)]
pub enum Expr<'a> {
    LiteralExpr {
        kind: LiteralExprKind<'a>,
        span: Span,
    },
    ParenExpr {
        expr: Box<Expr<'a>>,
        span: Span,
    },
    UnaryExpr {
        op: Operator,
        expr: Box<Expr<'a>>,
        span: Span,
    },
    BinaryExpr {
        lhs_expr: Box<Expr<'a>>,
        op: Operator,
        rhs_expr: Box<Expr<'a>>,
        span: Span,
    },
}

impl std::fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::LiteralExpr { kind, .. } => write!(f, "{kind}"),
            Expr::ParenExpr { expr, .. } => write!(f, "(group {expr})"),
            Expr::UnaryExpr { op, expr, .. } => write!(f, "({op} {expr})"),
            Expr::BinaryExpr {
                lhs_expr,
                op,
                rhs_expr,
                ..
            } => write!(f, "({op} {lhs_expr} {rhs_expr})"),
        }
    }
}

impl<'a> Expr<'a> {
    pub fn span(&self) -> Span {
        match self {
            Expr::LiteralExpr { span, .. } => *span,
            Expr::ParenExpr { span, .. } => *span,
            Expr::UnaryExpr { span, .. } => *span,
            Expr::BinaryExpr { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum LiteralExprKind<'a> {
    Number(f64),
    String(&'a str),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for LiteralExprKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralExprKind::Boolean(b) => write!(f, "{b}"),
            LiteralExprKind::Nil => write!(f, "nil"),
            LiteralExprKind::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            LiteralExprKind::String(s) => write!(f, "{s}"),
        }
    }
}
