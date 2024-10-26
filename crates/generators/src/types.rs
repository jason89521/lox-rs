use std::fmt::Display;

pub struct FieldDefinition {
    name: &'static str,
    file_type: AllType,
}

pub struct StructDefinition {
    pub name: &'static str,
    pub fields: Vec<AllType>,
    pub lifetime: bool,
}

pub fn generate_fields(fields: Vec<AllType>) -> String {
    let mut result = String::new();
    fields.into_iter().for_each(|field| {});

    result
}

pub fn generate_struct(definitions: Vec<StructDefinition>) -> String {
    let mut result = String::new();

    definitions.into_iter().for_each(
        |StructDefinition {
             lifetime,
             name,
             fields,
         }| {
            let lifetime = if lifetime { "<'a>" } else { "" };
            let s = format!(
                r#"
pub struct {name}{lifetime} {{
    
"#,
            );
        },
    );

    result
}

#[derive(Debug)]
pub enum CommonType {
    Span,
    Operator,
}

impl Display for CommonType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug)]
pub enum ExpressionType {
    LiteralExprKind,
    BoxExpr,
}

impl Display for ExpressionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionType::BoxExpr => write!(f, "{self:?}<'a>"),
            _ => write!(f, "{self:?}"),
        }
    }
}

macro_rules! display_enum {
    ($enum_name:ident { $( $variant:ident($inner:ty) ),* $(,)? }) => {
        pub enum $enum_name {
          $($variant($inner),)*
        }

        impl Display for $enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( $enum_name::$variant(value) => write!(f, "{}", value), )*
                }
            }
        }
    };
}

display_enum!(AllType {
    CommonType(CommonType),
    ExpressionType(ExpressionType),
});
