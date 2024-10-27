use crate::{Lexer, TokenKind};

mod declaration;
mod expression;
mod operator;
mod statement;

pub use declaration::Declaration;
use declaration::VarDeclaration;
use expression::{AssignmentExpression, IdentifierExpression};
pub use expression::{
    BinaryExpression, Expression, LiteralExpression, LiteralKind, ParenExpression, UnaryExpression,
};
use lox_span::GetSpan;
pub use operator::Operator;
pub use statement::Statement;
use statement::{ExpressionStatement, PrintStatement};

#[derive(Debug)]
pub enum AstKind<'a> {
    Program { body: Vec<AstKind<'a>> },
    Statement(Statement<'a>),
    Expression(Expression<'a>),
    Declaration(Declaration<'a>),
    Eof,
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
                return Err(e.into());
            }
            None => return Ok(AstKind::Eof),
        };

        match token.kind() {
            TokenKind::Print => {
                let token = self.lexer.next().unwrap().unwrap();
                let expr = self.parse_expr(0)?;
                let semicolon = self
                    .lexer
                    .expect_if(|kind| matches!(kind, TokenKind::Semicolon | TokenKind::Eof))?;
                let span = (token.span().start, semicolon.span().end).into();
                return Ok(AstKind::Statement(Statement::PrintStatement(
                    PrintStatement::new(expr, span),
                )));
            }
            TokenKind::Eof => {
                self.lexer.next();
                return Ok(AstKind::Eof);
            }
            TokenKind::Var => {
                let var = self.lexer.next().unwrap().unwrap();
                let ident = self.lexer.expect(TokenKind::Identifier)?;
                let kind = match self.lexer.peek() {
                    Some(Ok(token)) => token.kind(),
                    Some(Err(e)) => {
                        let e = e.clone();
                        self.lexer.next();
                        return Err(e.into());
                    }
                    None => {
                        return Err(anyhow::anyhow!(
                            "Expect equal or semicolon, but no token found"
                        ));
                    }
                };
                match kind {
                    TokenKind::Equal => {
                        let _equal = self.lexer.next().unwrap()?;
                        let expr = self.parse_expr(0)?;
                        let semicolon = self.lexer.expect(TokenKind::Semicolon)?;
                        let span = (var.span().start, semicolon.span().end).into();

                        return Ok(AstKind::Declaration(Declaration::VarDeclaration(
                            VarDeclaration::new(
                                IdentifierExpression::new(ident.lexeme(), ident.span()),
                                Some(expr),
                                span,
                            ),
                        )));
                    }
                    TokenKind::Semicolon => {
                        let semicolon = self.lexer.next().unwrap()?;
                        let span = (var.span().start, semicolon.span().end).into();

                        return Ok(AstKind::Declaration(Declaration::VarDeclaration(
                            VarDeclaration::new(
                                IdentifierExpression::new(ident.lexeme(), ident.span()),
                                None,
                                span,
                            ),
                        )));
                    }
                    kind => {
                        return Err(anyhow::anyhow!(
                            "Expect equal or semicolon, but found {kind}"
                        ))
                    }
                }
            }
            _ => {
                let expr = self.parse_expr(0)?;
                let semicolon = self.lexer.expect(TokenKind::Semicolon)?;
                let span = (expr.span().start, semicolon.span().end).into();
                let stat = Statement::ExpressionStatement(ExpressionStatement::new(expr, span));

                return Ok(AstKind::Statement(stat));
            }
        }
    }

    pub fn parse_expr(&mut self, min_bp: u8) -> anyhow::Result<Expression<'a>> {
        let lhs_token = match self.lexer.next() {
            Some(token) => token?,
            None => {
                return Ok(Expression::LiteralExpression(LiteralExpression::new(
                    LiteralKind::Nil,
                    (self.source_code.len(), self.source_code.len()).into(),
                )))
            }
        };

        let mut lhs_expr = match lhs_token.kind() {
            TokenKind::Identifier => Expression::IdentifierExpression(IdentifierExpression::new(
                lhs_token.lexeme(),
                lhs_token.span(),
            )),
            TokenKind::String => Expression::LiteralExpression(LiteralExpression::new(
                LiteralKind::String(lhs_token.lexeme().trim_matches('"')),
                lhs_token.span(),
            )),
            TokenKind::Number(n) => Expression::LiteralExpression(LiteralExpression::new(
                LiteralKind::Number(n),
                lhs_token.span(),
            )),
            TokenKind::True => Expression::LiteralExpression(LiteralExpression::new(
                LiteralKind::Boolean(true),
                lhs_token.span(),
            )),
            TokenKind::False => Expression::LiteralExpression(LiteralExpression::new(
                LiteralKind::Boolean(false),
                lhs_token.span(),
            )),
            TokenKind::Nil => Expression::LiteralExpression(LiteralExpression::new(
                LiteralKind::Nil,
                lhs_token.span(),
            )),
            TokenKind::LeftParen => {
                let expr = self.parse_expr(0)?;
                let right_paren_span = self.lexer.expect(TokenKind::RightParen)?.span();
                let span = (lhs_token.span().start, right_paren_span.end).into();
                Expression::ParenExpression(Box::new(ParenExpression::new(expr, span)))
            }
            TokenKind::Minus | TokenKind::Bang => {
                let (_, r_bp) = prefix_binding_power(lhs_token.kind().into());
                let rhs_expr = self.parse_expr(r_bp)?;
                let span = (lhs_token.span().start, rhs_expr.span().end).into();
                Expression::UnaryExpression(Box::new(UnaryExpression::new(
                    lhs_token.lexeme().into(),
                    rhs_expr,
                    span,
                )))
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "[line {}] Error at '{}': Expect expression.",
                    lhs_token.line(self.source_code),
                    lhs_token.lexeme()
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
                | TokenKind::EqualEqual
                | TokenKind::Equal => {
                    let (l_bp, r_bp) = infix_binding_power(op.kind().into());
                    let op = op.lexeme().into();
                    if l_bp < min_bp {
                        break;
                    }
                    self.lexer.next();
                    let rhs_expr = self.parse_expr(r_bp)?;
                    let span = (lhs_expr.span().start, rhs_expr.span().end).into();
                    lhs_expr = if op == Operator::Equal {
                        Expression::AssignmentExpression(Box::new(AssignmentExpression::new(
                            IdentifierExpression::new(&lhs_token.lexeme(), lhs_token.span()),
                            rhs_expr,
                            span,
                        )))
                    } else {
                        Expression::BinaryExpression(Box::new(BinaryExpression::new(
                            lhs_expr, op, rhs_expr, span,
                        )))
                    };
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
        Operator::Equal => (2, 1),
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
