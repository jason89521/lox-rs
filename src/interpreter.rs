use std::borrow::Cow;

use anyhow::Result;

use crate::{AstKind, Expr, LiteralExprKind, Operator, Parser, Span, Statement};

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Operand{0} must be a {expected_operand}{0}.\n", if *binary {"s"} else {""})]
    InvalidOperand {
        expected_operand: &'static str,
        binary: bool,
        span: Span,
    },
}

enum Value<'a> {
    Number(f64),
    String(Cow<'a, str>),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

pub struct Interpreter<'a> {
    parser: Parser<'a>,
    source_code: &'a str,
}

impl<'a> Interpreter<'a> {
    pub fn new(source_code: &'a str) -> Self {
        let parser = Parser::new(&source_code);
        Self {
            parser,
            source_code,
        }
    }

    pub fn line(&self, span: Span) -> usize {
        self.source_code[..=span.0].lines().count()
    }

    pub fn eval(ast: AstKind) -> Result<()> {
        match ast {
            AstKind::Program { body } => {
                for ast in body {
                    Self::eval(ast)?;
                }
                return Ok(());
            }
            AstKind::Statement(statement) => match statement {
                Statement::PrintStmt { span, expr } => {
                    let value = Self::evaluate_expr(expr)?;
                    println!("{value}");
                }
                Statement::ExprStmt { span, expr } => {
                    Self::evaluate_expr(expr)?;
                }
            },
            AstKind::Expr(expr) => return Ok(()),
            AstKind::Eof => return Ok(()),
        }

        return Ok(());
    }

    pub fn evaluate(&mut self) -> Result<()> {
        let expr = self.parser.parse_expr(0)?;
        let value = Self::evaluate_expr(expr)?;

        println!("{value}");
        Ok(())
    }

    fn evaluate_expr(expr: Expr) -> Result<Value> {
        let span = expr.span();
        match expr {
            Expr::UnaryExpr { expr, op, span } => {
                let value = Self::evaluate_expr(*expr)?;
                return Ok(match op {
                    Operator::Bang => match value {
                        Value::Boolean(b) => Value::Boolean(!b),
                        Value::Nil => Value::Boolean(true),
                        _ => Value::Boolean(false),
                    },
                    Operator::Minus => match value {
                        Value::Number(n) => Value::Number(-n),
                        _ => {
                            return Err(RuntimeError::InvalidOperand {
                                span,
                                expected_operand: "number",
                                binary: false,
                            }
                            .into())
                        }
                    },
                    _ => unreachable!(""),
                });
            }
            Expr::ParenExpr { expr, .. } => return Self::evaluate_expr(*expr),
            Expr::BinaryExpr {
                op,
                lhs_expr,
                rhs_expr,
                ..
            } => {
                let lhs = Self::evaluate_expr(*lhs_expr)?;
                let rhs = Self::evaluate_expr(*rhs_expr)?;

                match (&lhs, &rhs) {
                    (Value::Number(a), Value::Number(b)) => match op {
                        Operator::Plus => return Ok(Value::Number(a + b)),
                        Operator::Minus => return Ok(Value::Number(a - b)),
                        Operator::Star => return Ok(Value::Number(a * b)),
                        Operator::Slash => return Ok(Value::Number(a / b)),
                        Operator::Greater => return Ok(Value::Boolean(a > b)),
                        Operator::GreaterEqual => return Ok(Value::Boolean(a >= b)),
                        Operator::Less => return Ok(Value::Boolean(a < b)),
                        Operator::LessEqual => return Ok(Value::Boolean(a <= b)),
                        Operator::EqualEqual => return Ok(Value::Boolean(a == b)),
                        Operator::BangEqual => return Ok(Value::Boolean(a != b)),
                        _ => unimplemented!(),
                    },
                    (Value::String(a), Value::String(b))
                        if matches!(
                            op,
                            Operator::Plus | Operator::EqualEqual | Operator::BangEqual
                        ) =>
                    {
                        match op {
                            Operator::Plus => {
                                return Ok(Value::String(Cow::Owned(format!(
                                    "{}{}",
                                    a.trim_matches('"'),
                                    b.trim_matches('"'),
                                ))));
                            }
                            Operator::EqualEqual => return Ok(Value::Boolean(a == b)),
                            Operator::BangEqual => return Ok(Value::Boolean(a != b)),
                            _ => unimplemented!(),
                        }
                    }
                    (Value::Boolean(a), Value::Boolean(b))
                        if matches!(op, Operator::EqualEqual | Operator::BangEqual) =>
                    {
                        match op {
                            Operator::EqualEqual => return Ok(Value::Boolean(a == b)),
                            Operator::BangEqual => return Ok(Value::Boolean(a != b)),
                            _ => unimplemented!(),
                        }
                    }
                    (Value::Nil, Value::Nil)
                        if matches!(op, Operator::BangEqual | Operator::EqualEqual) =>
                    {
                        return Ok(Value::Boolean(if op == Operator::BangEqual {
                            false
                        } else {
                            true
                        }))
                    }
                    _ if matches!(op, Operator::BangEqual | Operator::EqualEqual) => {
                        return Ok(Value::Boolean(if op == Operator::BangEqual {
                            true
                        } else {
                            false
                        }))
                    }
                    _ if matches!(
                        op,
                        Operator::Plus
                            | Operator::Minus
                            | Operator::Star
                            | Operator::Slash
                            | Operator::Less
                            | Operator::LessEqual
                            | Operator::Greater
                            | Operator::GreaterEqual
                    ) =>
                    {
                        return Err(RuntimeError::InvalidOperand {
                            span,
                            expected_operand: "number",
                            binary: true,
                        }
                        .into())
                    }
                    _ => return Err(anyhow::anyhow!("Mismatched type: {} and {}", lhs, rhs)),
                }
            }
            Expr::LiteralExpr { kind, .. } => {
                return Ok(match kind {
                    LiteralExprKind::Number(v) => Value::Number(v),
                    LiteralExprKind::String(v) => Value::String(Cow::Borrowed(v)),
                    LiteralExprKind::Boolean(v) => Value::Boolean(v),
                    LiteralExprKind::Nil => Value::Nil,
                })
            }
        };
    }
}
