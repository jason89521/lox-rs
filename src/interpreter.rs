use std::{borrow::Cow, collections::HashMap};

use anyhow::Result;

use crate::{
    AstKind, BinaryExpression, Declaration, Expression, LiteralKind, Operator, Parser, Statement,
    UnaryExpression,
};
use span::{GetSpan, Span};

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Operand{0} must be a {expected_operand}{0}.\n", if *binary {"s"} else {""})]
    InvalidOperand {
        expected_operand: &'static str,
        binary: bool,
        span: Span,
    },
}

#[derive(Debug, Clone)]
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
    var_value: HashMap<&'a str, Value<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new(source_code: &'a str) -> Self {
        let parser = Parser::new(&source_code);
        Self {
            parser,
            source_code,
            var_value: HashMap::new(),
        }
    }

    pub fn line(&self, span: Span) -> usize {
        self.source_code[..=span.start].lines().count()
    }

    pub fn run(&mut self) -> Result<()> {
        let ast = self.parser.parse()?;
        self.eval(ast)?;

        Ok(())
    }

    pub fn eval(&mut self, ast: AstKind<'a>) -> Result<()> {
        match ast {
            AstKind::Program { body } => {
                for ast in body {
                    self.eval(ast)?;
                }
                return Ok(());
            }
            AstKind::Statement(statement) => match statement {
                Statement::PrintStatement(stmt) => {
                    let expr = stmt.expr;
                    let value = self.evaluate_expr(expr)?;
                    println!("{value}");
                }
                Statement::ExpressionStatement(stmt) => {
                    let expr = stmt.expr;
                    self.evaluate_expr(expr)?;
                }
            },
            AstKind::Eof => {}
            AstKind::Expression(expression) => {
                self.evaluate_expr(expression)?;
            }
            AstKind::Declaration(declaration) => match declaration {
                Declaration::VarDeclaration(var_declaration) => {
                    let value = match var_declaration.init {
                        Some(expr) => self.evaluate_expr(expr)?,
                        None => Value::Nil,
                    };
                    self.var_value.insert(var_declaration.ident.name, value);
                }
            },
        }

        return Ok(());
    }

    pub fn evaluate(&mut self) -> Result<()> {
        let expr = self.parser.parse_expr(0)?;
        let value = self.evaluate_expr(expr)?;

        println!("{value}");
        Ok(())
    }

    fn evaluate_expr(&mut self, expr: Expression<'a>) -> Result<Value<'a>> {
        let span = expr.span();
        match expr {
            Expression::UnaryExpression(unary_expr) => {
                let span = unary_expr.span();
                let UnaryExpression { op, expr, .. } = unary_expr;
                let value = self.evaluate_expr(*expr)?;
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
            Expression::ParenExpression(paren_expr) => return self.evaluate_expr(*paren_expr.expr),
            Expression::BinaryExpression(binary_expr) => {
                let BinaryExpression {
                    op,
                    lhs_expr,
                    rhs_expr,
                    ..
                } = binary_expr;
                let lhs = self.evaluate_expr(*lhs_expr)?;
                let rhs = self.evaluate_expr(*rhs_expr)?;

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
            Expression::LiteralExpression(literal_expr) => {
                let kind = literal_expr.kind;
                return Ok(match kind {
                    LiteralKind::Number(v) => Value::Number(v),
                    LiteralKind::String(v) => Value::String(Cow::Borrowed(v)),
                    LiteralKind::Boolean(v) => Value::Boolean(v),
                    LiteralKind::Nil => Value::Nil,
                });
            }
            Expression::IdentifierExpression(identifier_expression) => {
                if let Some(value) = self.var_value.get(identifier_expression.name) {
                    return Ok(value.clone());
                } else {
                    return Err(anyhow::anyhow!("Cannot access undeclared variable."));
                }
            }
        };
    }
}
