use std::fmt::Display;

#[derive(Debug, strum::Display)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
    literal: Option<&'a str>,
    line: i32,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, lexeme: &'a str) -> Self {
        Self {
            token_type,
            lexeme,
            literal: None,
            line: 1,
        }
    }

    pub fn set_literal(mut self, literal: &'a str) -> Self {
        self.literal = Some(literal);
        self
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = self.literal.unwrap_or("null");
        write!(f, "{} {} {}", &self.token_type, self.lexeme, literal)
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    rest: &'a str,
    byte_offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            rest: source,
            byte_offset: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.chars();
        let ch = chars.next();
        if ch.is_none() {
            return if self.byte_offset == self.source.len() {
                self.byte_offset += 1;
                Some(Token::new(TokenType::Eof, ""))
            } else {
                None
            };
        }
        let ch = ch.unwrap();
        let byte_offset = self.byte_offset;
        self.rest = chars.as_str();
        self.byte_offset += ch.len_utf8();

        match ch {
            '(' => return Some(Token::new(TokenType::LeftParen, "(")),
            ')' => return Some(Token::new(TokenType::RightParen, ")")),
            '{' => return Some(Token::new(TokenType::LeftBrace, "{")),
            '}' => return Some(Token::new(TokenType::RightBrace, "}")),
            ch => {
                let a = miette::miette!(
                    labels = vec![miette::LabeledSpan::at_offset(byte_offset, "here")],
                    // Rest of the arguments are passed to `format!`
                    // to form diagnostic message
                    "Unimplemented token: {ch}"
                )
                .with_source_code(self.source.to_string());
                eprintln!("{:?}", a);
                todo!()
            }
        }
    }
}
