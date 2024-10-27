use anyhow::anyhow;
use std::fmt::Display;

use lox_span::Span;

#[derive(thiserror::Error, Debug, Clone)]
pub struct LexerError {
    source_code: String,
    span: Span,
    kind: LexerErrorKind,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(thiserror::Error, Debug, Clone, Copy)]
pub enum LexerErrorKind {
    #[error("Unexpected character: {token}")]
    UnexpectedCharacter { token: char },
    #[error("Unterminated string.")]
    UnterminatedString,
    #[error("Invalid number.")]
    InvalidNumber,
}

impl LexerError {
    pub fn line(&self) -> usize {
        self.source_code[..=self.span.start].lines().count()
    }

    pub fn new(kind: LexerErrorKind, span: Span, source_code: &str) -> Self {
        Self {
            source_code: source_code.to_string(),
            span,
            kind,
        }
    }
}

#[derive(Debug, strum::Display, Clone, Copy, PartialEq)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum TokenKind {
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
    Number(f64),

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

#[derive(Debug)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a str,
    span: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, span: Span) -> Self {
        Self { kind, lexeme, span }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn lexeme(&self) -> &'a str {
        self.lexeme
    }

    pub fn line(&self, source_code: &str) -> usize {
        source_code[..=self.span.start].lines().count()
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TokenKind::Number(number) => {
                if number.trunc() == *number {
                    write!(f, "{} {} {}.0", self.kind, self.lexeme, number)
                } else {
                    write!(f, "{} {} {}", self.kind, self.lexeme, number)
                }
            }
            TokenKind::String => write!(
                f,
                "{} {} {}",
                self.kind,
                self.lexeme,
                self.lexeme.trim_matches('"')
            ),
            _ => write!(f, "{} {} null", self.kind, self.lexeme),
        }
    }
}

pub struct Lexer<'a> {
    has_error: bool,
    source: &'a str,
    rest: &'a str,
    byte_offset: usize,
    reach_end: bool,
    peeked: Option<Result<Token<'a>, LexerError>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            has_error: false,
            source,
            rest: source,
            byte_offset: 0,
            reach_end: false,
            peeked: None,
        }
    }

    pub fn has_error(&self) -> bool {
        self.has_error
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'a>, LexerError>> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }

        self.peeked.as_ref()
    }

    pub fn expect_if<F>(&mut self, f: F) -> anyhow::Result<Token<'a>>
    where
        F: Fn(TokenKind) -> bool,
    {
        if let Some(Ok(token)) = self.next() {
            if f(token.kind) {
                return Ok(token);
            } else {
                return Err(anyhow!("Unexpected token kind.",));
            }
        }

        return Err(anyhow!("There is no token."));
    }

    pub fn expect(&mut self, kind: TokenKind) -> anyhow::Result<Token<'a>> {
        if let Some(Ok(token)) = self.next() {
            if token.kind != kind {
                return Err(anyhow!(
                    "Unexpected token kind, expected: {}, found: {}",
                    kind,
                    token.kind,
                ));
            } else {
                return Ok(token);
            }
        }

        return Err(anyhow!(
            "Expect token kind: {}, but there is no token",
            kind
        ));
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.peeked.take() {
            return Some(token);
        }

        loop {
            let mut chars = self.rest.chars();
            let ch = chars.next();
            if ch.is_none() {
                return if self.reach_end {
                    None
                } else {
                    self.reach_end = true;
                    Some(Ok(Token::new(
                        TokenKind::Eof,
                        "",
                        (self.source.len() - 1, self.source.len() - 1).into(),
                    )))
                };
            }
            let ch = ch.unwrap();
            let ch_str = &self.rest[..ch.len_utf8()];
            let byte_offset = self.byte_offset;
            let origin_rest = self.rest;
            self.rest = chars.as_str();
            self.byte_offset += ch.len_utf8();

            let pure_token = |kind: TokenKind| {
                Some(Ok(Token::new(
                    kind,
                    ch_str,
                    (byte_offset, byte_offset + ch.len_utf8()).into(),
                )))
            };
            let token_item = |token: Token<'a>| Some(Ok(token));

            enum MultiCharToken {
                WithEqual { yes: TokenKind, no: TokenKind },
                Slash,
                String,
                Number,
                Identifier,
            }

            let multi_char_token = match ch {
                '(' => return pure_token(TokenKind::LeftParen),
                ')' => return pure_token(TokenKind::RightParen),
                '{' => return pure_token(TokenKind::LeftBrace),
                '}' => return pure_token(TokenKind::RightBrace),
                ',' => return pure_token(TokenKind::Comma),
                '.' => return pure_token(TokenKind::Dot),
                '-' => return pure_token(TokenKind::Minus),
                '+' => return pure_token(TokenKind::Plus),
                '*' => return pure_token(TokenKind::Star),
                ';' => return pure_token(TokenKind::Semicolon),
                '=' => MultiCharToken::WithEqual {
                    yes: TokenKind::EqualEqual,
                    no: TokenKind::Equal,
                },
                '!' => MultiCharToken::WithEqual {
                    yes: TokenKind::BangEqual,
                    no: TokenKind::Bang,
                },
                '<' => MultiCharToken::WithEqual {
                    yes: TokenKind::LessEqual,
                    no: TokenKind::Less,
                },
                '>' => MultiCharToken::WithEqual {
                    yes: TokenKind::GreaterEqual,
                    no: TokenKind::Greater,
                },
                '/' => MultiCharToken::Slash,
                '"' => MultiCharToken::String,
                '0'..='9' => MultiCharToken::Number,
                '_' | 'a'..='z' | 'A'..='Z' => MultiCharToken::Identifier,
                ch if ch.is_whitespace() => continue,
                ch => {
                    self.has_error = true;

                    return Some(Err(LexerError::new(
                        LexerErrorKind::UnexpectedCharacter { token: ch },
                        (byte_offset, ch.len_utf8()).into(),
                        self.source,
                    )));
                }
            };

            match multi_char_token {
                MultiCharToken::WithEqual { yes, no } => {
                    if self.rest.starts_with('=') {
                        self.byte_offset += 1;
                        self.rest = &self.rest[1..];
                        return token_item(Token::new(
                            yes,
                            &origin_rest[..ch.len_utf8() + 1],
                            (byte_offset, byte_offset + 1 + ch.len_utf8()).into(),
                        ));
                    } else {
                        return token_item(Token::new(
                            no,
                            ch_str,
                            (byte_offset, byte_offset + ch.len_utf8()).into(),
                        ));
                    }
                }
                MultiCharToken::Slash => {
                    if self.rest.starts_with('/') {
                        let line_end = self.rest.find('\n').unwrap_or(self.rest.len());
                        self.byte_offset += line_end;
                        self.rest = &self.rest[line_end..];
                        continue;
                    } else {
                        return token_item(Token::new(
                            TokenKind::Slash,
                            ch_str,
                            (byte_offset, byte_offset + ch.len_utf8()).into(),
                        ));
                    }
                }
                MultiCharToken::String => {
                    if let Some(string_end) = self.rest.find('"') {
                        let lexeme = &origin_rest[..=string_end + 1];
                        self.rest = &self.rest[string_end + 1..];
                        self.byte_offset += string_end + 1;
                        return token_item(Token::new(
                            TokenKind::String,
                            lexeme,
                            (byte_offset, byte_offset + lexeme.len()).into(),
                        ));
                    } else {
                        self.has_error = true;
                        self.byte_offset = self.source.len();
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Err(LexerError::new(
                            LexerErrorKind::UnterminatedString,
                            (byte_offset, self.source.len() - byte_offset).into(),
                            self.source,
                        )));
                    }
                }
                MultiCharToken::Number => {
                    let non_digit_pos = origin_rest
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or(origin_rest.len());
                    let mut lexeme = &origin_rest[..non_digit_pos];
                    let mut splitted_by_dot = lexeme.splitn(3, '.');
                    match (
                        splitted_by_dot.next(),
                        splitted_by_dot.next(),
                        splitted_by_dot.next(),
                    ) {
                        (Some(i), Some(f), _) => {
                            // 12. or 12..
                            if f.is_empty() {
                                lexeme = &lexeme[..i.len()]
                            } else {
                                lexeme = &lexeme[..i.len() + 1 + f.len()]
                            }
                        }
                        _ => {}
                    }
                    let extra_byte = lexeme.len() - ch.len_utf8();
                    self.byte_offset += extra_byte;
                    self.rest = &self.rest[extra_byte..];

                    let parsed_number = match lexeme.parse() {
                        Ok(n) => n,
                        Err(_) => {
                            return Some(Err(LexerError::new(
                                LexerErrorKind::InvalidNumber,
                                (byte_offset, lexeme.len()).into(),
                                &self.source,
                            )))
                        }
                    };

                    return token_item(Token::new(
                        TokenKind::Number(parsed_number),
                        lexeme,
                        (byte_offset, byte_offset + lexeme.len()).into(),
                    ));
                }
                MultiCharToken::Identifier => {
                    let non_ident_pos = origin_rest
                        .find(|ch| !matches!(ch, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'))
                        .unwrap_or(origin_rest.len());
                    let lexeme = &origin_rest[..non_ident_pos];
                    let extra_bytes = lexeme.len() - ch.len_utf8();
                    self.byte_offset += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match lexeme {
                        "and" => TokenKind::And,
                        "class" => TokenKind::Class,
                        "else" => TokenKind::Else,
                        "false" => TokenKind::False,
                        "for" => TokenKind::For,
                        "fun" => TokenKind::Fun,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "print" => TokenKind::Print,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "true" => TokenKind::True,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        _ => TokenKind::Identifier,
                    };

                    return token_item(Token::new(
                        kind,
                        lexeme,
                        (byte_offset, byte_offset + lexeme.len()).into(),
                    ));
                }
            }
        }
    }
}
