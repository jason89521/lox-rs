use crate::{Lexer, TokenKind};

pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    EqualEqual,
    BangEqual,
    Bang,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::EqualEqual => write!(f, "=="),
            Operator::BangEqual => write!(f, "!="),
            Operator::Bang => write!(f, "!"),
        }
    }
}

impl Into<Operator> for &str {
    fn into(self) -> Operator {
        match self {
            "+" => Operator::Plus,
            "-" => Operator::Minus,
            "*" => Operator::Star,
            "/" => Operator::Slash,
            "<" => Operator::Less,
            ">" => Operator::Greater,
            "<=" => Operator::LessEqual,
            ">=" => Operator::GreaterEqual,
            "!" => Operator::Bang,
            "!=" => Operator::BangEqual,
            "==" => Operator::EqualEqual,
            _ => panic!("Unknown operator {self}"),
        }
    }
}

impl Into<Operator> for TokenKind {
    fn into(self) -> Operator {
        match self {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Star => Operator::Star,
            TokenKind::Slash => Operator::Slash,
            TokenKind::Less => Operator::Less,
            TokenKind::Greater => Operator::Greater,
            TokenKind::LessEqual => Operator::LessEqual,
            TokenKind::GreaterEqual => Operator::GreaterEqual,
            TokenKind::Bang => Operator::Bang,
            TokenKind::BangEqual => Operator::BangEqual,
            TokenKind::EqualEqual => Operator::EqualEqual,
            _ => panic!("Unknown operator {self}"),
        }
    }
}

pub enum Expr<'a> {
    LiteralExpr(LiteralExpr<'a>),
    ParenExpr(Box<Expr<'a>>),
    UnaryExpr {
        op: Operator,
        expr: Box<Expr<'a>>,
    },
    BinaryExpr {
        lhs_expr: Box<Expr<'a>>,
        op: Operator,
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

impl Into<f64> for LiteralExpr<'_> {
    fn into(self) -> f64 {
        if let LiteralExpr::Number(n) = self {
            n
        } else {
            panic!("Cannot turn {} to f64.", self)
        }
    }
}

impl<'a> Into<&'a str> for LiteralExpr<'a> {
    fn into(self) -> &'a str {
        if let LiteralExpr::String(s) = self {
            s
        } else {
            panic!("Cannot turn {self} to &str.")
        }
    }
}

impl Into<bool> for LiteralExpr<'_> {
    fn into(self) -> bool {
        if let LiteralExpr::Boolean(b) = self {
            b
        } else {
            panic!("Cannot turn {self} to bool.")
        }
    }
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
                let (_, r_bp) = prefix_binding_power(token.kind().into());
                let rhs_expr = self.parse_expr(r_bp)?;
                Expr::UnaryExpr {
                    op: token.lexeme().into(),
                    expr: Box::new(rhs_expr),
                }
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "[line {}] Error at '{}': Expect expression.",
                    self.lexer.current_line(),
                    token.lexeme()
                ))
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
                    let (l_bp, r_bp) = infix_binding_power(op.kind().into());
                    if l_bp < min_bp {
                        break;
                    }
                    self.lexer.next();
                    let rhs_expr = self.parse_expr(r_bp)?;
                    lhs_expr = Expr::BinaryExpr {
                        lhs_expr: Box::new(lhs_expr),
                        op: lexeme.into(),
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

fn infix_binding_power(op: Operator) -> (u8, u8) {
    match op {
        Operator::Less
        | Operator::LessEqual
        | Operator::Greater
        | Operator::GreaterEqual
        | Operator::BangEqual
        | Operator::EqualEqual => (1, 2),
        Operator::Plus | Operator::Minus => (3, 4),
        Operator::Star | Operator::Slash => (5, 6),
        _ => unreachable!(),
    }
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::Minus | Operator::Bang => ((), 7),
        _ => unreachable!(),
    }
}
