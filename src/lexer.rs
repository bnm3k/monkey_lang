use std::collections::HashMap;

use crate::token::{self, Token, TokenType};

pub struct Lexer {
    chars: Vec<char>,
    keywords: HashMap<String, TokenType>,
    i: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut chars = input.chars().collect::<Vec<_>>();
        chars.push(0 as char);
        Self {
            chars: chars,
            i: 0,
            keywords: token::get_identifier_lookup(),
        }
    }

    fn consume_whitespace(&mut self) {
        while self.chars[self.i].is_whitespace() {
            self.i += 1;
        }
    }

    fn peek(&self) -> char {
        let next = self.i + 1;
        if next >= self.chars.len() {
            0 as char // NUL as sentinel
        } else {
            self.chars[next]
        }
    }

    fn lookup_identifier(&self, s: &str) -> TokenType {
        if let Some(t) = self.keywords.get(s) {
            *t
        } else {
            TokenType::IDENT
        }
    }

    fn read_identifier(&mut self) -> String {
        assert!(self.chars[self.i].is_alphabetic());
        let mut identifier = String::new();
        let mut j = self.i;
        loop {
            let c = self.chars[j];
            if c.is_alphabetic() || c == '_' {
                identifier.push(c);
                j += 1;
            } else {
                break;
            }
        }
        self.i = j - 1;
        return identifier;
    }

    fn read_number(&mut self) -> String {
        assert!(self.chars[self.i].is_ascii_digit());
        let mut identifier = String::new();
        let mut j = self.i;
        loop {
            let c = self.chars[j];
            if c.is_ascii_digit() {
                identifier.push(c);
                j += 1;
            } else {
                break;
            }
        }
        self.i = j - 1;
        return identifier;
    }
}

impl Iterator for Lexer {
    type Item = Token;
    // next token
    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.chars.len() {
            return None;
        }
        self.consume_whitespace();
        let curr = self.chars[self.i];
        use TokenType::*;
        let token = match curr {
            // operators
            '=' => {
                if self.peek() == '=' {
                    self.i += 1;
                    Token::new(EQ, "==".into())
                } else {
                    Token::new(ASSIGN, curr.into())
                }
            }
            '!' => {
                if self.peek() == '=' {
                    self.i += 1;
                    Token::new(NOT_EQ, "!=".into())
                } else {
                    Token::new(BANG, curr.into())
                }
            }
            '+' => Token::new(PLUS, curr.into()),
            '-' => Token::new(MINUS, curr.into()),
            '*' => Token::new(ASTERISK, curr.into()),
            '/' => Token::new(SLASH, curr.into()),
            '<' => Token::new(LT, curr.into()),
            '>' => Token::new(GT, curr.into()),
            // delimiters
            ',' => Token::new(COMMA, curr.into()),
            ';' => Token::new(SEMICOLON, curr.into()),
            '(' => Token::new(LPAREN, curr.into()),
            ')' => Token::new(RPAREN, curr.into()),
            '{' => Token::new(LBRACE, curr.into()),
            '}' => Token::new(RBRACE, curr.into()),
            '\0' => Token::new(EOF, "".into()),
            c @ _ => {
                let is_letter = |c: char| c.is_alphabetic() || c == '_';
                if is_letter(c) {
                    let literal = self.read_identifier();
                    let token_type = self.lookup_identifier(&literal);
                    Token::new(token_type, literal)
                } else if c.is_ascii_digit() {
                    let num_str = self.read_number();
                    Token::new(INT, num_str)
                } else {
                    Token::new(ILLEGAL, "".into())
                }
            }
        };
        self.i += 1;
        return Some(token);
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    #[test]
    fn test_empty_input() {
        let input = "";
        let lexer = Lexer::new(input);
        let tokens = lexer.into_iter().collect::<Vec<_>>();
        assert_eq!(tokens[0].token_type, TokenType::EOF);
        assert!(tokens.len() == 1);
    }

    #[test]
    fn test_input_whitespace_only() {
        let input = "             ";
        let lexer = Lexer::new(input);
        let tokens = lexer.into_iter().collect::<Vec<_>>();
        assert_eq!(tokens[0].token_type, TokenType::EOF);
        assert!(tokens.len() == 1);
    }

    #[test]
    fn test_let_statement() {
        let input = "let five = 5;";
        let lexer = Lexer::new(input);
        let got = lexer.into_iter().map(|t| t.token_type).collect::<Vec<_>>();
        use TokenType::*;
        assert_eq!(vec![LET, IDENT, ASSIGN, INT, SEMICOLON, EOF], got);
    }

    #[test]
    fn test_single_char_tokens() {
        use TokenType::*;
        let input = "=+(){},;";
        let input_without_whitespace = " =\t+\n()\r {}\r\n,;";
        // output should be in the following order
        let expected = vec![
            (ASSIGN, "="),
            (PLUS, "+"),
            (LPAREN, "("),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RBRACE, "}"),
            (COMMA, ","),
            (SEMICOLON, ";"),
            (EOF, ""),
        ];
        for input in [input, input_without_whitespace] {
            let lexer = Lexer::new(input);
            let mut tokens_received = 0;
            for (got, expect) in lexer.zip(&expected) {
                assert_eq!(got.token_type, expect.0);
                assert_eq!(got.literal, expect.1);
                tokens_received += 1;
            }
            assert_eq!(tokens_received, expected.len());
        }
    }

    #[test]
    fn test_multi_char_tokens() {
        use TokenType::*;
        let input = r#"
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }


            10 == 10;
            10 != 9;
        "#;
        let expected = vec![
            (LET, "let"),
            (IDENT, "five"),
            (ASSIGN, "="),
            (INT, "5"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "ten"),
            (ASSIGN, "="),
            (INT, "10"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "add"),
            (ASSIGN, "="),
            (FUNCTION, "fn"),
            (LPAREN, "("),
            (IDENT, "x"),
            (COMMA, ","),
            (IDENT, "y"),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (IDENT, "x"),
            (PLUS, "+"),
            (IDENT, "y"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "result"),
            (ASSIGN, "="),
            (IDENT, "add"),
            (LPAREN, "("),
            (IDENT, "five"),
            (COMMA, ","),
            (IDENT, "ten"),
            (RPAREN, ")"),
            (SEMICOLON, ";"),
            (BANG, "!"),
            (MINUS, "-"),
            (SLASH, "/"),
            (ASTERISK, "*"),
            (INT, "5"),
            (SEMICOLON, ";"),
            (INT, "5"),
            (LT, "<"),
            (INT, "10"),
            (GT, ">"),
            (INT, "5"),
            (SEMICOLON, ";"),
            (IF, "if"),
            (LPAREN, "("),
            (INT, "5"),
            (LT, "<"),
            (INT, "10"),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RETURN, "return"),
            (TRUE, "true"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (ELSE, "else"),
            (LBRACE, "{"),
            (RETURN, "return"),
            (FALSE, "false"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (INT, "10"),
            (EQ, "=="),
            (INT, "10"),
            (SEMICOLON, ";"),
            (INT, "10"),
            (NOT_EQ, "!="),
            (INT, "9"),
            (SEMICOLON, ";"),
            (EOF, ""),
        ];
        let lexer = Lexer::new(input);
        let mut tokens_received = 0;
        for (i, (got, expect)) in lexer.zip(&expected).enumerate() {
            assert_eq!(got.token_type, expect.0, "At pos: {}", i);
            assert_eq!(got.literal, expect.1, "At pos: {}", i);
            tokens_received += 1;
        }
        assert_eq!(tokens_received, expected.len());
    }
}
