use crate::types::StructDefinition;

pub fn generate_expression(ds: Vec<StructDefinition>) {
    let mut result = String::new();

    format!(
        r#"
#[derive(Debug)]
pub enum LiteralExprKind<'a> {{
  Number(f64),
  String(&'a str),
  Boolean(bool),
  Nil,
}}

impl std::fmt::Display for LiteralExprKind<'_> {{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
      match self {{
          LiteralExprKind::Boolean(b) => write!(f, "{{b}}"),
          LiteralExprKind::Nil => write!(f, "nil"),
          LiteralExprKind::Number(n) => {{
              if *n == n.trunc() {{
                  write!(f, "{{n}}.0")
              }} else {{
                  write!(f, "{{n}}")
              }}
          }}
          LiteralExprKind::String(s) => write!(f, "{{s}}"),
      }}
  }}
}}

"#,
    );
}
