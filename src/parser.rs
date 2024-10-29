use crate::{Lexer, TokenKind};

mod declaration;
mod expression;
mod operator;
mod statement;

use expression::{AssignmentExpression, IdentifierExpression, LogicalExpression};
pub use expression::{
    BinaryExpression, Expression, LiteralExpression, LiteralKind, ParenExpression, UnaryExpression,
};
use lox_span::GetSpan;
pub use operator::Operator;
pub use statement::Statement;
use statement::{
    BlockStatement, ExpressionStatement, ForLoopStatement, IfStatement, PrintStatement,
    WhileStatement,
};

pub use declaration::{Declaration, VarDeclaration};

#[derive(Debug)]
pub struct Program<'a> {
    pub body: Vec<Declaration<'a>>,
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

    pub fn parse(&mut self) -> anyhow::Result<Program<'a>> {
        let mut body = vec![];
        while let Some(token) = self.lexer.peek() {
            if token
                .as_ref()
                .map(|token| token.kind() == TokenKind::Eof)
                .unwrap_or(false)
            {
                break;
            }
            body.push(self.parse_declaration(0)?);
        }

        Ok(Program { body })
    }

    pub fn parse_declaration(&mut self, _min_bp: u8) -> anyhow::Result<Declaration<'a>> {
        let token = self.lexer.peek_token()?;
        match token.kind() {
            TokenKind::Var => {
                let var = self.lexer.next().unwrap().unwrap();
                let ident = self.lexer.expect(TokenKind::Identifier)?;
                let kind = self.lexer.peek_token()?.kind();

                match kind {
                    TokenKind::Equal => {
                        let _equal = self.lexer.next().unwrap()?;
                        let expr = self.parse_expr(0)?;
                        let semicolon = self.lexer.expect(TokenKind::Semicolon)?;
                        let span = (var.span().start, semicolon.span().end).into();

                        return Ok(Declaration::VarDeclaration(VarDeclaration::new(
                            IdentifierExpression::new(ident.lexeme(), ident.span()),
                            Some(expr),
                            span,
                        )));
                    }
                    TokenKind::Semicolon => {
                        let semicolon = self.lexer.next().unwrap()?;
                        let span = (var.span().start, semicolon.span().end).into();

                        return Ok(Declaration::VarDeclaration(VarDeclaration::new(
                            IdentifierExpression::new(ident.lexeme(), ident.span()),
                            None,
                            span,
                        )));
                    }
                    kind => {
                        return Err(anyhow::anyhow!(
                            "Expect equal or semicolon, but found {kind}"
                        ))
                    }
                }
            }
            _ => Ok(Declaration::Statement(self.parse_statement(0)?)),
        }
    }

    pub fn parse_statement(&mut self, _min_bp: u8) -> anyhow::Result<Statement<'a>> {
        let token = match self.lexer.peek() {
            Some(Ok(token)) => token,
            Some(Err(e)) => {
                let e = e.clone();
                self.lexer.next();
                return Err(e.into());
            }
            None => unreachable!("Ensure token exist in parse()"),
        };

        match token.kind() {
            TokenKind::Print => {
                let token = self.lexer.next().unwrap().unwrap();
                let expr = self.parse_expr(0)?;
                let semicolon = self
                    .lexer
                    .expect_if(|kind| matches!(kind, TokenKind::Semicolon | TokenKind::Eof))?;
                let span = (token.span().start, semicolon.span().end).into();
                return Ok(Statement::PrintStatement(PrintStatement::new(expr, span)));
            }
            TokenKind::LeftBrace => {
                let left_brace = self.lexer.next().unwrap()?;
                let mut decls = vec![];
                while let Some(Ok(token)) = self.lexer.peek().as_ref() {
                    if token.kind() == TokenKind::RightBrace {
                        break;
                    }
                    decls.push(self.parse_declaration(0)?);
                }
                let right_brace = self.lexer.expect(TokenKind::RightBrace)?;
                let span = (left_brace.span().start, right_brace.span().end).into();

                return Ok(Statement::BlockStatement(BlockStatement::new(decls, span)));
            }
            TokenKind::If => {
                let if_token = self.lexer.expect(TokenKind::If)?;
                let condition = self.parse_expr(0)?;
                let then_branch = self.parse_statement(0)?;
                let else_branch = match self.lexer.peek_token()? {
                    token if token.kind() == TokenKind::Else => {
                        self.lexer.next();
                        Some(self.parse_statement(0)?)
                    }
                    _ => None,
                };
                let span = (
                    if_token.span().start,
                    if let Some(stmt) = &else_branch {
                        stmt.span().end
                    } else {
                        then_branch.span().end
                    },
                )
                    .into();
                return Ok(Statement::IfStatement(Box::new(IfStatement::new(
                    condition,
                    then_branch,
                    else_branch,
                    span,
                ))));
            }
            TokenKind::While => {
                let while_token = self.lexer.expect(TokenKind::While)?;
                let condition = self.parse_expr(0)?;
                let block = self.parse_statement(0)?;
                let span = (while_token.span().start, block.span().end).into();

                return Ok(Statement::WhileStatement(Box::new(WhileStatement::new(
                    condition, block, span,
                ))));
            }
            TokenKind::For => {
                let _for_token = self.lexer.expect(TokenKind::For)?;
                let left_paren = self.lexer.expect(TokenKind::LeftParen)?;
                let init = if self.lexer.peek_expect(TokenKind::Semicolon)?.is_none() {
                    Some(self.parse_declaration(0)?)
                } else {
                    self.lexer.next();
                    None
                };
                let condition = if self.lexer.peek_expect(TokenKind::Semicolon)?.is_none() {
                    let condition = Some(self.parse_expr(0)?);
                    self.lexer.expect(TokenKind::Semicolon)?;
                    condition
                } else {
                    self.lexer.next();
                    None
                };
                let increment = if self.lexer.peek_expect(TokenKind::RightParen)?.is_none() {
                    let increment = Some(self.parse_expr(0)?);
                    self.lexer.expect(TokenKind::RightParen)?;
                    increment
                } else {
                    self.lexer.next();
                    None
                };
                let block = self.parse_statement(0)?;
                let span = (left_paren.span().start, block.span().end).into();

                return Ok(Statement::ForLoopStatement(Box::new(
                    ForLoopStatement::new(init, condition, increment, block, span),
                )));
            }
            _ => {
                let expr = self.parse_expr(0)?;
                let semicolon = self.lexer.expect(TokenKind::Semicolon)?;
                let span = (expr.span().start, semicolon.span().end).into();
                let stmt = Statement::ExpressionStatement(ExpressionStatement::new(expr, span));

                return Ok(stmt);
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
                | TokenKind::Equal
                | TokenKind::And
                | TokenKind::Or => {
                    let (l_bp, r_bp) = infix_binding_power(op.kind().into());
                    let op = op.lexeme().into();
                    if l_bp < min_bp {
                        break;
                    }
                    self.lexer.next();
                    let rhs_expr = self.parse_expr(r_bp)?;
                    let span = (lhs_expr.span().start, rhs_expr.span().end).into();
                    lhs_expr = match op {
                        Operator::Equal => {
                            Expression::AssignmentExpression(Box::new(AssignmentExpression::new(
                                IdentifierExpression::new(&lhs_token.lexeme(), lhs_token.span()),
                                rhs_expr,
                                span,
                            )))
                        }
                        Operator::And | Operator::Or => Expression::LogicalExpression(Box::new(
                            LogicalExpression::new(lhs_expr, op, rhs_expr, span),
                        )),
                        _ => Expression::BinaryExpression(Box::new(BinaryExpression::new(
                            lhs_expr, op, rhs_expr, span,
                        ))),
                    };
                }
                _ => {
                    break;
                }
            }
        }

        Ok(lhs_expr)
    }
}

fn infix_binding_power(op: Operator) -> (u8, u8) {
    match op {
        Operator::Equal => (2, 1),
        Operator::Or => (3, 4),
        Operator::And => (5, 6),
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
