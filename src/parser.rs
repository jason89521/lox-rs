use crate::{Lexer, TokenKind};

pub enum Expr<'a> {
    LiteralExpr(LiteralExpr<'a>),
    ParenExpr(Box<Expr<'a>>),
    // UnaryExpr {
    //     op: &'a str,
    //     expr: Box<Expr<'a>>,
    // },
    // BinaryExpr {
    //     left_expr: Box<Expr<'a>>,
    //     op: &'a str,
    //     right_expr: Box<Expr<'a>>,
    // },
}

impl std::fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::LiteralExpr(literal) => write!(f, "{literal}"),
            Expr::ParenExpr(expr) => write!(f, "(group {expr})"),
        }
    }
}

pub enum LiteralExpr<'a> {
    Number(f64),
    String(&'a str),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for LiteralExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralExpr::Boolean(b) => write!(f, "{b}"),
            LiteralExpr::Nil => write!(f, "nil"),
            LiteralExpr::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            LiteralExpr::String(s) => write!(f, "{}", s.trim_matches('"')),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            lexer: Lexer::new(&source_code),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr<'a>, Box<dyn std::error::Error>> {
        let lhs = match self.lexer.next() {
            Some(token) => token?,
            None => return Ok(Expr::LiteralExpr(LiteralExpr::Nil)),
        };

        let lhs_expr = match lhs.kind() {
            TokenKind::String => Expr::LiteralExpr(LiteralExpr::String(lhs.lexeme())),
            TokenKind::Number(n) => Expr::LiteralExpr(LiteralExpr::Number(n)),
            TokenKind::True => Expr::LiteralExpr(LiteralExpr::Boolean(true)),
            TokenKind::False => Expr::LiteralExpr(LiteralExpr::Boolean(false)),
            TokenKind::Nil => Expr::LiteralExpr(LiteralExpr::Nil),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                self.lexer.expect(TokenKind::RightParen)?;
                Expr::ParenExpr(Box::new(expr))
            }
            _ => {
                unimplemented!()
            }
        };

        Ok(lhs_expr)
    }
}
