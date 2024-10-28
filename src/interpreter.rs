use std::borrow::Cow;
use std::io::Write;

use anyhow::Result;

use crate::{
    AstKind, BinaryExpression, Expression, LiteralKind, Operator, Parser, Statement,
    UnaryExpression,
};
use lox_span::{GetSpan, Span};

mod context;

use context::Context;

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Operand{0} must be a {expected_operand}{0}.\n", if *binary {"s"} else {""})]
    InvalidOperand {
        expected_operand: &'static str,
        binary: bool,
        span: Span,
    },
    #[error("Undefined variable '{}'.", ident)]
    UndefinedVariable { ident: String },
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

impl Value<'_> {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }
}

pub struct Interpreter<'a> {
    parser: Parser<'a>,
    source_code: &'a str,
    context: Context<'a>,
    buffer: Vec<u8>,
}

impl<'a> Interpreter<'a> {
    pub fn new(source_code: &'a str) -> Self {
        let parser = Parser::new(&source_code);
        Self {
            parser,
            source_code,
            context: Context::new(),
            buffer: vec![],
        }
    }

    pub fn flush(&mut self) -> Result<()> {
        self.buffer.flush()?;
        Ok(())
    }

    pub fn print_buffer(&self) -> Result<()> {
        let output = String::from_utf8(self.buffer.clone())?;
        print!("{output}");
        Ok(())
    }

    pub fn output(&self) -> Result<String> {
        let output = String::from_utf8(self.buffer.clone())?;
        Ok(output)
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
            AstKind::Statement(statement) => return self.visit_stmt(statement),
            AstKind::Eof => {}
            AstKind::Expression(expression) => {
                self.evaluate_expr(expression)?;
            }
        }

        return Ok(());
    }

    fn visit_stmt(&mut self, stmt: Statement<'a>) -> Result<()> {
        match stmt {
            Statement::PrintStatement(stmt) => {
                let expr = stmt.expr;
                let value = self.evaluate_expr(expr)?;
                writeln!(self.buffer, "{value}")?;
            }
            Statement::ExpressionStatement(stmt) => {
                let expr = stmt.expr;
                self.evaluate_expr(expr)?;
            }
            Statement::VarDeclaration(declaration) => {
                let value = match declaration.init {
                    Some(expr) => self.evaluate_expr(expr)?,
                    None => Value::Nil,
                };
                self.context.declare(declaration.ident.name, value);
            }
            Statement::BlockStatement(block) => {
                self.context.enter_block();
                for stmt in block.stmts.into_iter() {
                    self.visit_stmt(stmt)?;
                }
                self.context.exit_block();
            }
            Statement::IfStatement(stmt) => {
                let condition = self.evaluate_expr(stmt.condition)?.is_truthy();
                if condition {
                    self.visit_stmt(stmt.then_branch)?;
                } else if stmt.else_branch.is_some() {
                    self.visit_stmt(stmt.else_branch.expect("Else checked"))?
                }
            }
        }

        return Ok(());
    }

    pub fn evaluate(&mut self) -> Result<()> {
        let expr = self.parser.parse_expr(0)?;
        let value = self.evaluate_expr(expr)?;
        writeln!(self.buffer, "{value}")?;
        Ok(())
    }

    fn evaluate_expr(&mut self, expr: Expression<'a>) -> Result<Value<'a>> {
        let span = expr.span();
        match expr {
            Expression::UnaryExpression(unary_expr) => {
                let span = unary_expr.span();
                let UnaryExpression { op, expr, .. } = *unary_expr;
                let value = self.evaluate_expr(expr)?;
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
            Expression::ParenExpression(paren_expr) => return self.evaluate_expr(paren_expr.expr),
            Expression::BinaryExpression(binary_expr) => {
                let BinaryExpression {
                    op,
                    lhs_expr,
                    rhs_expr,
                    ..
                } = *binary_expr;
                let lhs = self.evaluate_expr(lhs_expr)?;
                let rhs = self.evaluate_expr(rhs_expr)?;

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
                if let Some(value) = self.context.get(identifier_expression.name) {
                    return Ok(value.clone());
                } else {
                    return Err(RuntimeError::UndefinedVariable {
                        ident: identifier_expression.name.to_string(),
                    }
                    .into());
                }
            }
            Expression::AssignmentExpression(assignment_expression) => {
                let val = self.evaluate_expr(assignment_expression.expr)?;
                self.context
                    .assign(assignment_expression.ident.name, val.clone())
                    .map_err(|_| RuntimeError::UndefinedVariable {
                        ident: assignment_expression.ident.name.to_string(),
                    })?;
                return Ok(val);
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn expect(code: &str, expected: &str) {
        let mut interpreter = Interpreter::new(code);
        interpreter.run().expect("Failed to run interpreter");
        interpreter.flush().expect("Failed to flush the buffer");
        let output = interpreter.output().expect("Failed to get the output");
        assert_eq!(output.trim(), expected.trim());
    }

    #[test]
    fn string() {
        let code = r#"print "(" + "" + ")";
print "a string";
print "a
b
c";
print "A~¶Þॐஃ";"#;
        let expected = r#"()
a string
a
b
c
A~¶Þॐஃ
"#;
        expect(code, expected);
    }

    #[test]
    fn number() {
        let code = r#"
print 123;     // expect: 123
print 987654;  // expect: 987654
print 0;       // expect: 0
print -0;      // expect: -0

print 123.456; // expect: 123.456
print -0.001;  // expect: -0.001"#;
        let expected = r#"
123
987654
0
-0
123.456
-0.001
"#;
        expect(code, expected);
    }

    #[test]
    fn nil() {
        let code = "print nil; // expect: nil";
        let expected = "nil";
        expect(code, &expected);
    }

    #[test]
    fn bool() {
        let code = r#"print true == true;    // expect: true
print true == false;   // expect: false
print false == true;   // expect: false
print false == false;  // expect: true

// Not equal to other types.
print true == 1;        // expect: false
print false == 0;       // expect: false
print true == "true";   // expect: false
print false == "false"; // expect: false
print false == "";      // expect: false

print true != true;    // expect: false
print true != false;   // expect: true
print false != true;   // expect: true
print false != false;  // expect: false

// Not equal to other types.
print true != 1;        // expect: true
print false != 0;       // expect: true
print true != "true";   // expect: true
print false != "false"; // expect: true
print false != "";      // expect: true
print !true;    // expect: false
print !false;   // expect: true
print !!true;   // expect: true"#;
        let expected = r#"
true
false
false
true
false
false
false
false
false
false
true
true
false
true
true
true
true
true
false
true
true
"#;
        expect(code, expected);
    }
}
