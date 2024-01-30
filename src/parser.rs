use std::collections::VecDeque;

use crate::ast::{Expression, Identifier, Node, PlaceholderExpression, Program, Statement};
use crate::{
    ast,
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFn = dyn Fn() -> Box<dyn ast::Expression>;
type InfixParseFn = dyn Fn(Box<dyn ast::Expression>) -> Box<dyn ast::Expression>;

pub struct Parser {
    tokens: VecDeque<Token>,
    error_msgs: Vec<String>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let lexer = Lexer::new(input);
        let tokens = lexer.collect::<VecDeque<_>>();
        return Parser {
            tokens: tokens,
            error_msgs: Vec::new(),
        };
    }

    pub fn parse_program(mut self) -> Result<ast::Program, Vec<String>> {
        let mut statements = Vec::<Statement>::new();
        loop {
            if self.peek_expect(TokenType::EOF) {
                let _ = self.tokens.pop_front();
                break;
            }
            if let Some(statement) = self.parse_statement() {
                statements.push(statement)
            } else {
                let _ = self.tokens.pop_front();
            }
        }
        let program = Program { statements };
        if self.error_msgs.len() > 0 {
            Err(self.error_msgs)
        } else {
            Ok(program)
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        use TokenType::*;
        match self.tokens.front().unwrap().token_type {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        // first token should be return
        let return_token = self.tokens.pop_front().unwrap();
        assert!(return_token.token_type == TokenType::RETURN);

        // TODO currently skipping expression until we encounter a semi-colon
        loop {
            let curr_token = self.tokens.pop_front().unwrap();
            match curr_token.token_type {
                TokenType::SEMICOLON => break,
                TokenType::EOF => {
                    self.error_msgs
                        .push("Unexpected end of let statement".into());
                    // place implicit semi-colon
                    self.tokens
                        .push_back(Token::new(TokenType::SEMICOLON, ";".into()));
                    self.tokens.push_back(curr_token); // push back eof
                    return None;
                }
                _ => continue,
            }
        }
        Some(Statement::ReturnStmt {
            token: return_token,
            value: PlaceholderExpression::TODO_Remove(),
        })
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        // should be a let statement
        let let_token = self.tokens.pop_front().unwrap();
        assert!(let_token.token_type == TokenType::LET);

        // after let statement, we should have an identifier
        if !self.peek_expect_or_set_err(TokenType::IDENT) {
            return None;
        }
        let ident_token = self.tokens.pop_front().unwrap();
        let ident_value = ident_token.literal.clone();
        let identifier = Identifier {
            token: ident_token,
            value: ident_value,
        };

        // after ident token we should get an assign token
        if !self.peek_expect_or_set_err(TokenType::ASSIGN) {
            self.tokens.pop_front();
            return None;
        }
        let _ = self.tokens.pop_front();

        // TODO currently skipping expression until we encounter a semi-colon
        loop {
            let curr_token = self.tokens.pop_front().unwrap();
            match curr_token.token_type {
                TokenType::SEMICOLON => break,
                TokenType::EOF => {
                    self.error_msgs
                        .push("Unexpected end of let statement".into());
                    // place implicit semi-colon
                    self.tokens
                        .push_back(Token::new(TokenType::SEMICOLON, ";".into()));
                    self.tokens.push_back(curr_token); // push back eof
                    return None;
                }
                _ => continue,
            }
        }

        Some(Statement::LetStmt {
            token: let_token,
            name: identifier,
            value: PlaceholderExpression::TODO_Remove(),
        })
    }

    fn peek_expect(&self, expected: TokenType) -> bool {
        match self.tokens.front() {
            Some(t) => t.token_type == expected,
            None => false,
        }
    }

    fn peek_expect_or_set_err(&mut self, expected: TokenType) -> bool {
        let is_match = self.peek_expect(expected);
        if !is_match {
            let got = self.tokens.front().unwrap().token_type;
            self.error_msgs.push(format!(
                "expected next token to be {:?}, got {:?} instead",
                expected, got
            ));
        }
        return is_match;
    }
}
#[cfg(test)]
mod parser_tests {
    use crate::ast::Statement;

    use super::*;
    #[test]
    fn test_let_statements() {
        let input = r#"
            let x =  5;
            let y = 10;
            let foobar = 124124;
        "#;
        let parser = Parser::new(input);
        let program = parser
            .parse_program()
            .expect("should be parsed successfully");
        assert_eq!(3, program.statements.len());
        let expected_identifiers = ["x", "y", "foobar"];
        for (i, expected_identifier) in expected_identifiers.into_iter().enumerate() {
            let stmt = &program.statements[i];
            use Statement::*;
            match stmt {
                ls @ LetStmt { name, .. } => {
                    assert!(ls.token_literal() == "let");
                    assert!(name.value == expected_identifier);
                    assert!(name.token_literal() == expected_identifier);
                }
                _ => panic!("Expect let statement only"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 9000;
        "#;
        let parser = Parser::new(input);
        let program = parser
            .parse_program()
            .expect("should be parsed successfully");
        assert_eq!(3, program.statements.len());
        for stmt in program.statements {
            use Statement::*;
            match stmt {
                rs @ ReturnStmt { .. } => {
                    assert!(rs.token_literal() == "return");
                }
                _ => panic!("Expected return statement only"),
            }
        }
    }
}
