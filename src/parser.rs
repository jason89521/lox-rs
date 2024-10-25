use crate::{Lexer, TokenKind};

mod expr;
mod operator;
mod statement;

pub use expr::{Expr, LiteralExprKind};
pub use operator::Operator;
pub use statement::Statement;

#[derive(Debug)]
pub enum AstKind<'a> {
    Program { body: Vec<AstKind<'a>> },
    Statement(Statement<'a>),
    Expr(Expr<'a>),
    Eof,
}

impl<'a> AstKind<'a> {
    pub fn expect_expr(self) -> Expr<'a> {
        if let AstKind::Expr(expr) = self {
            return expr;
        }
        panic!("Expect expr but found :{:?}", self)
    }
}
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    source_code: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            lexer: Lexer::new(&source_code),
            source_code,
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<AstKind<'a>> {
        let mut body = vec![];
        while self.lexer.peek().is_some() {
            body.push(self.parse_statement(0)?);
        }

        Ok(AstKind::Program { body })
    }

    pub fn parse_statement(&mut self, _min_bp: u8) -> anyhow::Result<AstKind<'a>> {
        let token = match self.lexer.peek() {
            Some(Ok(token)) => token,
            Some(Err(e)) => {
                let e = e.clone();
                self.lexer.next();
                return Err((e.clone()).into());
            }
            None => {
                return Ok(AstKind::Expr(Expr::LiteralExpr {
                    kind: LiteralExprKind::Nil,
                    span: (self.source_code.len(), 0),
                }))
            }
        };

        match token.kind() {
            TokenKind::Print => {
                let token = self.lexer.next().unwrap().unwrap();
                let expr = self.parse_expr(0)?;
                let semicolon = self
                    .lexer
                    .expect_if(|kind| matches!(kind, TokenKind::Semicolon | TokenKind::Eof))?;
                let span = (token.span().0, semicolon.span().1);
                return Ok(AstKind::Statement(Statement::PrintStmt { span, expr }));
            }
            TokenKind::Eof => {
                self.lexer.next();
                return Ok(AstKind::Eof);
            }
            _ => {}
        }
        unimplemented!()
    }

    pub fn parse_expr(&mut self, min_bp: u8) -> anyhow::Result<Expr<'a>> {
        let token = match self.lexer.next() {
            Some(token) => token?,
            None => {
                return Ok(Expr::LiteralExpr {
                    kind: LiteralExprKind::Nil,
                    span: (self.source_code.len(), self.source_code.len()),
                })
            }
        };

        let mut lhs_expr = match token.kind() {
            TokenKind::String => Expr::LiteralExpr {
                kind: LiteralExprKind::String(token.lexeme().trim_matches('"')),
                span: token.span(),
            },
            TokenKind::Number(n) => Expr::LiteralExpr {
                kind: LiteralExprKind::Number(n),
                span: token.span(),
            },
            TokenKind::True => Expr::LiteralExpr {
                kind: LiteralExprKind::Boolean(true),
                span: token.span(),
            },
            TokenKind::False => Expr::LiteralExpr {
                kind: LiteralExprKind::Boolean(false),
                span: token.span(),
            },
            TokenKind::Nil => Expr::LiteralExpr {
                kind: LiteralExprKind::Nil,
                span: token.span(),
            },
            TokenKind::LeftParen => {
                let expr = self.parse_expr(0)?;
                let right_paren_span = self.lexer.expect(TokenKind::RightParen)?.span();
                let span = (token.span().0, right_paren_span.1);
                Expr::ParenExpr {
                    expr: Box::new(expr),
                    span,
                }
            }
            TokenKind::Minus | TokenKind::Bang => {
                let (_, r_bp) = prefix_binding_power(token.kind().into());
                let rhs_expr = self.parse_expr(r_bp)?;
                let span = (token.span().0, rhs_expr.span().1);
                Expr::UnaryExpr {
                    op: token.lexeme().into(),
                    expr: Box::new(rhs_expr),
                    span,
                }
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "[line {}] Error at '{}': Expect expression.",
                    token.line(self.source_code),
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
                    let span = (lhs_expr.span().0, rhs_expr.span().1);
                    lhs_expr = Expr::BinaryExpr {
                        op: lexeme.into(),
                        lhs_expr: Box::new(lhs_expr),
                        rhs_expr: Box::new(rhs_expr),
                        span,
                    }
                }
                TokenKind::Eof | TokenKind::Semicolon => break,
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
        | Operator::EqualEqual => (11, 12),
        Operator::Plus | Operator::Minus => (13, 14),
        Operator::Star | Operator::Slash => (15, 16),
        _ => unreachable!(),
    }
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::Print => ((), 1),
        Operator::Minus | Operator::Bang => ((), 17),
        _ => unreachable!(),
    }
}
