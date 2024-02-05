#![allow(non_camel_case_types, dead_code)]
use std::{collections::HashMap, convert::TryFrom, fmt::Debug};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // identifiers + literals
    IDENT,
    INT,
    STRING,

    // operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,

    // delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl TryFrom<char> for TokenType {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            _ => Err(()),
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{:?},\"{}\"}}", self.token_type, self.literal)
    }
}

pub fn get_identifier_lookup() -> HashMap<String, TokenType> {
    use TokenType::*;
    let mut m = HashMap::new();
    let keywords = [
        ("fn", FUNCTION),
        ("let", LET),
        ("true", TRUE),
        ("false", FALSE),
        ("if", IF),
        ("else", ELSE),
        ("return", RETURN),
    ];
    for (s, t) in keywords {
        m.insert(s.into(), t);
    }
    return m;
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type: token_type,
            literal: literal,
        }
    }
}
