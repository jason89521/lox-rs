use crate::{Lexer, TokenKind};

pub enum Expr<'a> {
    LiteralExpr(LiteralExpr<'a>),
    ParenExpr(Box<Expr<'a>>),
    UnaryExpr {
        op: &'a str,
        expr: Box<Expr<'a>>,
    },
    BinaryExpr {
        lhs_expr: Box<Expr<'a>>,
        op: &'a str,
        rhs_expr: Box<Expr<'a>>,
    },
}

impl std::fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::LiteralExpr(literal) => write!(f, "{literal}"),
            Expr::ParenExpr(expr) => write!(f, "(group {expr})"),
            Expr::UnaryExpr { op, expr } => write!(f, "({op} {expr})"),
            Expr::BinaryExpr {
                lhs_expr,
                op,
                rhs_expr,
            } => write!(f, "({op} {lhs_expr} {rhs_expr})"),
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

    pub fn parse_expr(&mut self, min_bp: u8) -> anyhow::Result<Expr<'a>> {
        let lhs = match self.lexer.next() {
            Some(token) => token?,
            None => return Ok(Expr::LiteralExpr(LiteralExpr::Nil)),
        };

        let mut lhs_expr = match lhs.kind() {
            TokenKind::String => Expr::LiteralExpr(LiteralExpr::String(lhs.lexeme())),
            TokenKind::Number(n) => Expr::LiteralExpr(LiteralExpr::Number(n)),
            TokenKind::True => Expr::LiteralExpr(LiteralExpr::Boolean(true)),
            TokenKind::False => Expr::LiteralExpr(LiteralExpr::Boolean(false)),
            TokenKind::Nil => Expr::LiteralExpr(LiteralExpr::Nil),
            TokenKind::LeftParen => {
                let expr = self.parse_expr(0)?;
                self.lexer.expect(TokenKind::RightParen)?;
                Expr::ParenExpr(Box::new(expr))
            }
            TokenKind::Minus | TokenKind::Bang => Expr::UnaryExpr {
                op: &lhs.lexeme(),
                expr: Box::new(self.parse_expr(min_bp)?),
            },
            _ => {
                unimplemented!()
            }
        };

        loop {
            let op = match self.lexer.peek() {
                Some(Ok(ref token)) => token,
                Some(_) => {
                    return Err(self
                        .lexer
                        .next()
                        .expect("Checked option")
                        .expect_err("Checked error")
                        .into())
                }
                None => break,
            };
            match op.kind() {
                TokenKind::RightParen => break,
                TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                    let lexeme = op.lexeme();
                    let (l_bp, r_bp) = infix_binding_power(lexeme);
                    if l_bp < min_bp {
                        break;
                    }
                    self.lexer.next();
                    let rhs_expr = self.parse_expr(r_bp)?;
                    lhs_expr = Expr::BinaryExpr {
                        lhs_expr: Box::new(lhs_expr),
                        op: lexeme,
                        rhs_expr: Box::new(rhs_expr),
                    }
                }
                TokenKind::Eof => break,
                token => {
                    eprintln!("kind: {token}");
                    unimplemented!()
                }
            }
        }

        Ok(lhs_expr)
    }
}

fn infix_binding_power(op: &str) -> (u8, u8) {
    match op {
        "+" | "-" => (1, 2),
        "*" | "/" => (3, 4),
        _ => unreachable!(),
    }
}
