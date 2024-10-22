use miette::SourceSpan;
use std::fmt::Display;

#[derive(thiserror::Error, Debug)]
pub struct LexerError<'a> {
    source_code: &'a str,
    span: SourceSpan,
    kind: LexerErrorKind,
}

impl Display for LexerError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LexerErrorKind {
    #[error("Unexpected character: {token}")]
    UnexpectedCharacter { token: char },
    #[error("Unterminated string.")]
    UnterminatedString,
}

impl<'a> LexerError<'a> {
    pub fn line(&self) -> usize {
        self.source_code[..=self.span.offset()].lines().count()
    }

    pub fn new(kind: LexerErrorKind, span: SourceSpan, source_code: &'a str) -> Self {
        Self {
            source_code,
            span,
            kind,
        }
    }
}

#[derive(Debug, strum::Display)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum TokenKind<'a> {
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
    Number(&'a str),

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
    kind: TokenKind<'a>,
    lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, lexeme: &'a str) -> Self {
        Self { kind, lexeme }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match &self.kind {
            TokenKind::Number(number) => number,
            TokenKind::String => self.lexeme.trim_matches('"'),
            _ => "null",
        };
        write!(f, "{} {} {}", self.kind, self.lexeme, literal)
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
    type Item = Result<Token<'a>, LexerError<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.chars();
            let ch = chars.next();
            if ch.is_none() {
                return if self.byte_offset == self.source.len() {
                    self.byte_offset += 1;
                    Some(Ok(Token::new(TokenKind::Eof, "")))
                } else {
                    None
                };
            }
            let ch = ch.unwrap();
            let ch_str = &self.rest[..ch.len_utf8()];
            let byte_offset = self.byte_offset;
            let origin_rest = self.rest;
            self.rest = chars.as_str();
            self.byte_offset += ch.len_utf8();

            let pure_token = |kind: TokenKind<'a>| Some(Ok(Token::new(kind, ch_str)));
            let token_item = |token: Token<'a>| Some(Ok(token));

            enum MultiCharToken<'a> {
                WithEqual {
                    yes: TokenKind<'a>,
                    no: TokenKind<'a>,
                },
                Slash,
                String,
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
                ch if ch.is_whitespace() => continue,
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

                    return Some(Err(LexerError::new(
                        LexerErrorKind::UnexpectedCharacter { token: ch },
                        SourceSpan::from(byte_offset..(byte_offset + ch.len_utf8())),
                        self.source,
                    )));
                }
            };

            match multi_char_token {
                MultiCharToken::WithEqual { yes, no } => {
                    if self.rest.starts_with('=') {
                        self.byte_offset += 1;
                        self.rest = &self.rest[1..];
                        return token_item(Token::new(yes, &origin_rest[..ch.len_utf8() + 1]));
                    } else {
                        return token_item(Token::new(no, ch_str));
                    }
                }
                MultiCharToken::Slash => {
                    if self.rest.starts_with('/') {
                        let line_end = self.rest.find('\n').unwrap_or(self.rest.len());
                        self.byte_offset += line_end;
                        self.rest = &self.rest[line_end..];
                        continue;
                    } else {
                        return token_item(Token::new(TokenKind::Slash, ch_str));
                    }
                }
                MultiCharToken::String => {
                    if let Some(string_end) = self.rest.find('"') {
                        let lexeme = &origin_rest[..=string_end + 1];
                        self.rest = &self.rest[string_end + 1..];
                        self.byte_offset += string_end + 1;
                        return token_item(Token::new(TokenKind::String, lexeme));
                    } else {
                        self.has_error = true;
                        self.byte_offset = self.source.len();
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Err(LexerError::new(
                            LexerErrorKind::UnterminatedString,
                            SourceSpan::from(byte_offset..self.source.len()),
                            self.source,
                        )));
                    }
                }
            }
        }
    }
}
