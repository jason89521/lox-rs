use miette::SourceSpan;
use std::fmt::Display;

#[derive(thiserror::Error, Debug, miette::Diagnostic)]
pub enum LexerError {
    #[error("Unexpected character: {token}")]
    UnexpectedCharacter {
        token: char,
        #[source_code]
        src: String,
        #[label("The unexpected character")]
        span: SourceSpan,
    },
}

impl LexerError {
    pub fn line(&self) -> usize {
        match self {
            Self::UnexpectedCharacter { src, span, .. } => {
                let line = src[..=span.offset()].lines().count();
                line
            }
        }
    }
}

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
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, lexeme: &'a str) -> Self {
        Self {
            token_type,
            lexeme,
            literal: None,
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
    has_error: bool,
    source: &'a str,
    rest: &'a str,
    byte_offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            has_error: false,
            source,
            rest: source,
            byte_offset: 0,
        }
    }

    pub fn has_error(&self) -> bool {
        self.has_error
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.chars();
        let ch = chars.next();
        if ch.is_none() {
            return if self.byte_offset == self.source.len() {
                self.byte_offset += 1;
                Some(Ok(Token::new(TokenType::Eof, "")))
            } else {
                None
            };
        }
        let ch = ch.unwrap();
        let byte_offset = self.byte_offset;
        self.rest = chars.as_str();
        self.byte_offset += ch.len_utf8();

        match ch {
            '(' => return Some(Ok(Token::new(TokenType::LeftParen, "("))),
            ')' => return Some(Ok(Token::new(TokenType::RightParen, ")"))),
            '{' => return Some(Ok(Token::new(TokenType::LeftBrace, "{"))),
            '}' => return Some(Ok(Token::new(TokenType::RightBrace, "}"))),
            ',' => return Some(Ok(Token::new(TokenType::Comma, ","))),
            '.' => return Some(Ok(Token::new(TokenType::Dot, "."))),
            '-' => return Some(Ok(Token::new(TokenType::Minus, "-"))),
            '+' => return Some(Ok(Token::new(TokenType::Plus, "+"))),
            '*' => return Some(Ok(Token::new(TokenType::Star, "*"))),
            ';' => return Some(Ok(Token::new(TokenType::Semicolon, ";"))),
            ch => {
                // let a = miette::miette!(
                //     labels = vec![miette::LabeledSpan::at_offset(byte_offset, "here")],
                //     // Rest of the arguments are passed to `format!`
                //     // to form diagnostic message
                //     "Unimplemented token: {ch}"
                // )
                // .with_source_code(self.source.to_string());
                // eprintln!("{:?}", a);
                self.has_error = true;

                return Some(Err(LexerError::UnexpectedCharacter {
                    token: ch,
                    src: self.source.to_string(),
                    span: SourceSpan::from(byte_offset..(byte_offset + ch.len_utf8())),
                }));
            }
        }
    }
}
