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
        let token = match self.lexer.next() {
            Some(token) => token?,
            None => return Ok(Expr::LiteralExpr(LiteralExpr::Nil)),
        };

        let mut lhs_expr = match token.kind() {
            TokenKind::String => Expr::LiteralExpr(LiteralExpr::String(token.lexeme())),
            TokenKind::Number(n) => Expr::LiteralExpr(LiteralExpr::Number(n)),
            TokenKind::True => Expr::LiteralExpr(LiteralExpr::Boolean(true)),
            TokenKind::False => Expr::LiteralExpr(LiteralExpr::Boolean(false)),
            TokenKind::Nil => Expr::LiteralExpr(LiteralExpr::Nil),
            TokenKind::LeftParen => {
                let expr = self.parse_expr(0)?;
                self.lexer.expect(TokenKind::RightParen)?;
                Expr::ParenExpr(Box::new(expr))
            }
            TokenKind::Minus | TokenKind::Bang => {
                let (_, r_bp) = prefix_binding_power(token.kind());
                let rhs_expr = self.parse_expr(r_bp)?;
                Expr::UnaryExpr {
                    op: &token.lexeme(),
                    expr: Box::new(rhs_expr),
                }
            }
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
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::BangEqual
                | TokenKind::EqualEqual => {
                    let lexeme = op.lexeme();
                    let (l_bp, r_bp) = infix_binding_power(op.kind());
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

fn infix_binding_power(op: TokenKind) -> (u8, u8) {
    match op {
        TokenKind::Less
        | TokenKind::LessEqual
        | TokenKind::Greater
        | TokenKind::GreaterEqual
        | TokenKind::BangEqual
        | TokenKind::EqualEqual => (1, 2),
        TokenKind::Plus | TokenKind::Minus => (3, 4),
        TokenKind::Star | TokenKind::Slash => (5, 6),
        _ => unreachable!(),
    }
}

fn prefix_binding_power(op: TokenKind) -> ((), u8) {
    match op {
        TokenKind::Minus | TokenKind::Bang => ((), 7),
        _ => unreachable!(),
    }
}
