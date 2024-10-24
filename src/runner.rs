use anyhow::Result;

use crate::{Expr, LiteralExpr, Operator, Parser};

pub struct Runner<'a> {
    parser: Parser<'a>,
}

impl<'a> Runner<'a> {
    pub fn new(source_code: &'a str) -> Self {
        let parser = Parser::new(&source_code);
        Self { parser }
    }

    pub fn run(&mut self) -> Result<()> {
        let expr = self.parser.parse_expr(0)?;
        let expr = self.evaluate(expr)?;
        match expr {
            LiteralExpr::Number(n) => println!("{n}"),
            _ => {
                println!("{expr}")
            }
        }
        Ok(())
    }

    fn evaluate(&self, expr: Expr<'a>) -> Result<LiteralExpr<'a>> {
        match expr {
            Expr::ParenExpr(expr) => return self.evaluate(*expr),
            Expr::BinaryExpr {
                lhs_expr,
                op,
                rhs_expr,
            } => {
                let lhs = self.evaluate(*lhs_expr)?;
                let rhs = self.evaluate(*rhs_expr)?;

                match (&lhs, &rhs) {
                    (LiteralExpr::Number(a), LiteralExpr::Number(b)) => match op {
                        Operator::Plus => return Ok(LiteralExpr::Number(a + b)),
                        Operator::Minus => return Ok(LiteralExpr::Number(a - b)),
                        Operator::Star => return Ok(LiteralExpr::Number(a * b)),
                        Operator::Slash => return Ok(LiteralExpr::Number(a / b)),
                        Operator::EqualEqual => return Ok(LiteralExpr::Boolean(a == b)),
                        Operator::BangEqual => return Ok(LiteralExpr::Boolean(a != b)),
                        _ => unimplemented!(),
                    },
                    (LiteralExpr::String(a), LiteralExpr::String(b)) => match op {
                        Operator::EqualEqual => return Ok(LiteralExpr::Boolean(a == b)),
                        Operator::BangEqual => return Ok(LiteralExpr::Boolean(a != b)),
                        _ => unimplemented!(),
                    },
                    (LiteralExpr::Boolean(a), LiteralExpr::Boolean(b)) => match op {
                        Operator::EqualEqual => return Ok(LiteralExpr::Boolean(a == b)),
                        Operator::BangEqual => return Ok(LiteralExpr::Boolean(a != b)),
                        _ => unimplemented!(),
                    },
                    (LiteralExpr::Nil, LiteralExpr::Nil) => todo!(),
                    _ => return Err(anyhow::anyhow!("Mismatched type: {} and {}", lhs, rhs)),
                }
            }
            Expr::LiteralExpr(expr) => return Ok(expr),
            _ => unimplemented!(),
        };
    }
}
