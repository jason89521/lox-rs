use crate::TokenKind;

#[derive(PartialEq, Clone, Copy, Debug)]
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
    Print,
    Equal,
    And,
    Or,
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
            Operator::Print => write!(f, "print"),
            Operator::Equal => write!(f, "="),
            Operator::And => write!(f, "and"),
            Operator::Or => write!(f, "or"),
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
            "print" => Operator::Print,
            "=" => Operator::Equal,
            "and" => Operator::And,
            "or" => Operator::Or,
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
            TokenKind::Equal => Operator::Equal,
            TokenKind::And => Operator::And,
            TokenKind::Or => Operator::Or,
            _ => panic!("Unknown operator {self}"),
        }
    }
}
