use std::collections::VecDeque;
use std::fmt::Debug;

use crate::ast::{BlockStatement, Expression, Program, Statement};
use crate::{
    ast,
    lexer::Lexer,
    token::{Token, TokenType},
};

pub struct Parser {
    tokens: VecDeque<Token>,
    error_msgs: Vec<String>,
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // + or -
    Product,     // * or /
    Prefix,      // -x or !x
    Call,        // my_function(x)
    Index,
}

fn token_type_precedence(t: &Token) -> Option<Precedence> {
    use Precedence::*;
    use TokenType::*;
    let p = match t.token_type {
        EQ => Equals,
        NOT_EQ => Equals,
        LT => LessGreater,
        GT => LessGreater,
        PLUS => Sum,
        MINUS => Sum,
        SLASH => Product,
        ASTERISK => Product,
        LPAREN => Call,
        LBRACKET => Index,
        _ => return None,
    };
    Some(p)
}

#[derive(Debug)]
pub struct ParserError {
    errors: Vec<String>,
}

impl std::error::Error for ParserError {}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parser Error:\n")?;
        for e in &self.errors {
            write!(f, "- {}\n", e)?
        }
        Ok(())
    }
}

impl Parser {
    pub fn parse(input: &str) -> Result<ast::Program, ParserError> {
        // init parser
        let lexer = Lexer::new(input);
        let tokens = lexer.collect::<VecDeque<_>>();
        let mut parser = Parser {
            tokens,
            error_msgs: Vec::new(),
        };

        // do parsing
        let mut statements = Vec::<Statement>::new();
        loop {
            if parser.peek_token_is(TokenType::EOF) {
                let _ = parser.tokens.pop_front();
                break;
            }
            if let Some(statement) = parser.parse_statement() {
                statements.push(statement)
            } else {
                // error occured
                if !parser.peek_token_is(TokenType::EOF) {
                    let _ = parser.tokens.pop_front();
                }
            }
        }
        let program = Program { statements };
        if parser.error_msgs.len() > 0 {
            Err(ParserError {
                errors: parser.error_msgs,
            })
        } else {
            Ok(program)
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.tokens
            .front()
            .and_then(token_type_precedence)
            .unwrap_or(Precedence::Lowest)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        use TokenType::*;
        match self.tokens.front().unwrap().token_type {
            LET => self.parse_let_statement(),
            RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        let stmt = Statement::ExpressionStmt(expression);
        if self.peek_token_is(TokenType::SEMICOLON) {
            let _ = self.tokens.pop_front();
        }
        Some(stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let token_type = self.tokens.front().unwrap().token_type;

        use TokenType::*;
        // handle prefix
        let mut left_expression: Expression = match token_type {
            IDENT => self.parse_identifier(),
            INT => self.parse_integer_literal(),
            BANG | MINUS => self.parse_prefix_expression(),
            TRUE | FALSE => self.parse_boolean(),
            LPAREN => self.parse_grouped_expression(),
            FUNCTION => self.parse_function_literal(),
            STRING => self.parse_string_literal(),
            LBRACKET => self.parse_array_literal(),
            LBRACE => self.parse_hash_literal(),
            IF => {
                let res = self.parse_if_expression();
                res
            }
            _ => {
                self.error_msgs.push(format!(
                    "no prefix parse function for {:?} found",
                    token_type
                ));
                None
            }
        }?;

        // handle infix
        // if precedence >= self.peek_precedence
        while precedence < self.peek_precedence() && !self.peek_token_is(TokenType::SEMICOLON) {
            let got = match self.tokens.front().unwrap().token_type {
                PLUS | MINUS | SLASH | ASTERISK | EQ | NOT_EQ | LT | GT => {
                    self.parse_infix_expression(left_expression)
                }
                LPAREN => self.parse_call_expression(left_expression),
                LBRACKET => self.parse_index_expression(left_expression),
                _ => return Some(left_expression),
            };
            left_expression = got.unwrap()
        }
        Some(left_expression)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap();
        assert_eq!(TokenType::LPAREN, token.token_type);
        let arguments = self.parse_expression_list(TokenType::RPAREN)?;
        Some(Expression::CallExpression {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap();
        assert_eq!(TokenType::LBRACKET, token.token_type);
        let index = self.parse_expression(Precedence::Lowest)?;
        if !self.peek_expect_or_set_err(TokenType::RBRACKET) {
            return None;
        }
        let _ = self.tokens.pop_front();
        Some(Expression::IndexExpression {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap();
        assert_eq!(TokenType::LBRACKET, token.token_type);
        let elements = self.parse_expression_list(TokenType::RBRACKET)?;
        Some(Expression::ArrayLiteral(elements))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
        let mut items = Vec::new();
        // handle empty list
        if self.peek_token_is(end) {
            let _ = self.tokens.pop_front();
            return Some(items);
        }
        // handle first element
        let first = self.parse_expression(Precedence::Lowest)?;
        items.push(first);
        while self.peek_token_is(TokenType::COMMA) {
            let _ = self.tokens.pop_front();
            let item = self.parse_expression(Precedence::Lowest)?;
            items.push(item);
        }
        if !self.peek_expect_or_set_err(end) {
            return None;
        } else {
            let _ = self.tokens.pop_front();
        }
        Some(items)
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap();
        assert_eq!(TokenType::LBRACE, token.token_type);
        let mut entries = Vec::new();
        // handle empty list
        if self.peek_token_is(TokenType::RBRACE) {
            let _ = self.tokens.pop_front();
            return Some(Expression::HashLiteral(entries));
        }

        // handle rest
        loop {
            let key = self.parse_expression(Precedence::Lowest)?;
            if !self.peek_expect_or_set_err(TokenType::COLON) {
                return None;
            }
            let _ = self.tokens.pop_front(); // pop colon
            let value = self.parse_expression(Precedence::Lowest)?;
            entries.push((key, value));

            if self.peek_token_is(TokenType::RBRACE) {
                let _ = self.tokens.pop_front();
                break;
            } else if self.peek_token_is(TokenType::COMMA) {
                let _ = self.tokens.pop_front();
            } else {
                let got = self.tokens.front().unwrap().token_type;
                self.error_msgs.push(format!(
                    "expected next token to be {:?} or {:?}, got {:?} instead",
                    TokenType::COMMA,
                    TokenType::RBRACE,
                    got
                ));
                return None;
            }
        }
        Some(Expression::HashLiteral(entries))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap();
        let tt = token.token_type;
        assert!(tt == TokenType::BANG || tt == TokenType::MINUS);
        let right = Box::new(self.parse_expression(Precedence::Prefix)?);
        Some(Expression::PrefixExpression {
            operator: token.literal,
            right,
        })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        // fn token
        let token = self.tokens.pop_front().unwrap();
        assert!(token.token_type == TokenType::FUNCTION);
        let parameters = self.parse_function_parameters()?;
        let body = self.parse_block_statement()?;
        Some(Expression::FunctionLiteral { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<String>> {
        // lparen
        if !self.peek_expect_or_set_err(TokenType::LPAREN) {
            return None;
        }
        let _ = self.tokens.pop_front().unwrap();

        let mut identifiers = Vec::new();

        // handle empty function params
        if self.peek_token_is(TokenType::RPAREN) {
            let _ = self.tokens.pop_front().unwrap();
            return Some(identifiers);
        }

        // helper function
        let get_identifier = |p: &mut Parser| {
            if !p.peek_expect_or_set_err(TokenType::IDENT) {
                return None;
            }
            let token = p.tokens.pop_front().unwrap(); // ident token
            Some(token.literal)
        };

        // handle first arg
        identifiers.push(get_identifier(self)?);

        // handle rest of args
        loop {
            if !self.peek_token_is(TokenType::COMMA) {
                break;
            }
            let _ = self.tokens.pop_front();
            identifiers.push(get_identifier(self)?);
        }

        if self.peek_token_is(TokenType::RPAREN) {
            let _ = self.tokens.pop_front().unwrap();
            return Some(identifiers);
        };
        Some(identifiers)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        // if token
        let if_token = self.tokens.pop_front().unwrap();
        assert!(if_token.token_type == TokenType::IF);

        // lparen token
        if !self.peek_expect_or_set_err(TokenType::LPAREN) {
            return None;
        }
        let _ = self.tokens.pop_front();

        // condition
        let condition = self.parse_expression(Precedence::Lowest).unwrap();

        // rparen token
        if !self.peek_expect_or_set_err(TokenType::RPAREN) {
            return None;
        }
        let _ = self.tokens.pop_front();

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(TokenType::ELSE) {
            let _ = self.tokens.pop_front();
            let block = self.parse_block_statement()?;
            Some(block)
        } else {
            None
        };

        Some(Expression::IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        let l_paren_token = self.tokens.pop_front().unwrap();
        assert!(l_paren_token.token_type == TokenType::LPAREN);
        let exp = self.parse_expression(Precedence::Lowest);
        if self.peek_expect_or_set_err(TokenType::RPAREN) {
            let _ = self.tokens.pop_front();
        }
        exp
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap(); // operator token
        use TokenType::*;
        assert!([PLUS, MINUS, SLASH, ASTERISK, EQ, NOT_EQ, LT, GT].contains(&token.token_type));
        let curr_precedence = token_type_precedence(&token).unwrap_or(Precedence::Lowest);
        let right = Box::new(self.parse_expression(curr_precedence).unwrap());
        Some(Expression::InfixExpression {
            left: Box::new(left),
            operator: token.literal,
            right,
        })
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap(); // ident token
        assert!(token.token_type == TokenType::IDENT);
        Some(Expression::Identifier(token.literal))
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap(); // bool token
                                                      // assert handled implicitly below
        let value = match token.token_type {
            TokenType::TRUE => true,
            TokenType::FALSE => false,
            _ => panic!("parse_boolean should only be called on a bool token type"),
        };
        Some(Expression::Boolean(value))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap();
        assert!(token.token_type == TokenType::INT);
        let value = match token.literal.parse::<i64>() {
            Ok(v) => v,
            Err(e) => {
                self.error_msgs.push(format!(
                    "Could not parse {} as integer: {}",
                    token.literal, e
                ));
                return None;
            }
        };
        Some(Expression::IntegerLiteral(value))
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        let token = self.tokens.pop_front().unwrap();
        assert!(token.token_type == TokenType::STRING);
        Some(Expression::StringLiteral(token.literal))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        // first token should be return
        let token = self.tokens.pop_front().unwrap();
        assert!(token.token_type == TokenType::RETURN);

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            let _ = self.tokens.pop_front();
        }

        Some(Statement::ReturnStmt(value))
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
        let identifier = ident_token.literal;
        // after ident token we should get an assign token
        if !self.peek_expect_or_set_err(TokenType::ASSIGN) {
            return None;
        }
        let _ = self.tokens.pop_front();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            let _ = self.tokens.pop_front();
        }

        Some(Statement::LetStmt {
            name: identifier,
            value,
        })
    }
    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        // lbrace
        if !self.peek_expect_or_set_err(TokenType::LBRACE) {
            return None;
        }
        let _ = self.tokens.pop_front().unwrap(); // lbrace token

        let mut statements = Vec::new();
        use TokenType::{EOF, RBRACE};
        loop {
            if self.peek_token_is(EOF) {
                self.error_msgs
                    .push("Unexpected end of block statement while parsing".into());
                return None;
            }
            // rbrace
            if self.peek_token_is(RBRACE) {
                let _ = self.tokens.pop_front();
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Some(BlockStatement { statements })
    }

    fn peek_token_is(&self, expected: TokenType) -> bool {
        match self.tokens.front() {
            Some(t) => t.token_type == expected,
            None => false,
        }
    }

    fn peek_expect_or_set_err(&mut self, expected: TokenType) -> bool {
        let is_match = self.peek_token_is(expected);
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

    fn is_match_integer_literal(expect: i64, expr: &Expression) -> Result<(), &'static str> {
        if let Expression::IntegerLiteral(got) = expr {
            if expect != *got {
                return Err("values do not match");
            }
        } else {
            return Err("Expected expression to be Integer Literal");
        }
        Ok(())
    }

    fn is_match_bool_literal(expect: bool, expr: &Expression) -> Result<(), &'static str> {
        if let Expression::Boolean(got) = expr {
            if expect != *got {
                return Err("values do not match");
            }
        } else {
            return Err("Expected expression to be Boolean Literal");
        }
        Ok(())
    }

    fn is_match_identifier(expect: &str, expr: &Expression) -> Result<(), &'static str> {
        if let Expression::Identifier(identifier) = expr {
            if identifier != expect {
                return Err("values do not match");
            }
        } else {
            return Err("Expected expression to be Identifier");
        }
        Ok(())
    }

    enum Literal<'a> {
        IntVal(i64),
        StrVal(&'a str),
        BoolVal(bool),
    }

    fn is_match_literal_expression(expect: Literal, expr: &Expression) -> Result<(), &'static str> {
        use Literal::*;
        match expect {
            IntVal(expect) => is_match_integer_literal(expect, expr),
            StrVal(expect) => is_match_identifier(expect, expr),
            BoolVal(expect) => is_match_bool_literal(expect, expr),
        }
    }

    fn is_match_infix_expression(
        expr: &Expression,
        left: Literal,
        operator: &str,
        right: Literal,
    ) -> Result<(), &'static str> {
        let (got_left, got_operator, got_right) = match expr {
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => (left, operator, right),
            _ => panic!("Expect infix expression"),
        };
        is_match_literal_expression(left, got_left)?;
        assert_eq!(operator, got_operator);
        is_match_literal_expression(right, got_right)?;
        Ok(())
    }

    use super::*;
    #[test]
    fn test_let_statements() {
        let input = r#"
            let x =  5;
            let y = 10;
            let foobar = 124124;
        "#;
        let program = Parser::parse(input).expect("should be parsed successfully");
        assert_eq!(3, program.statements.len());
        let expected_identifiers = ["x", "y", "foobar"];
        for (i, expected_identifier) in expected_identifiers.into_iter().enumerate() {
            let stmt = &program.statements[i];
            use Statement::*;
            match stmt {
                LetStmt { name, .. } => {
                    assert!(name == expected_identifier);
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
        let program = Parser::parse(input).expect("should be parsed successfully");
        assert_eq!(3, program.statements.len());
        for stmt in program.statements {
            use Statement::*;
            match stmt {
                ReturnStmt { .. } => {}
                _ => panic!("Expected return statement only"),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let mut program = Parser::parse(input).unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements.pop().unwrap();

        match stmt {
            Statement::ExpressionStmt(value) => {
                if let Expression::Identifier(v) = value {
                    assert_eq!("foobar", v);
                } else {
                    panic!("Expected expression to be identifier");
                }
            }
            _ => panic!("Expected expression statement only"),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "55;";
        let mut program = Parser::parse(input).unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements.pop().unwrap();

        match stmt {
            Statement::ExpressionStmt(value) => {
                is_match_integer_literal(55, &value).unwrap();
            }
            _ => panic!("Expected expression statement only"),
        }
    }

    #[test]
    fn test_boolean_expression() {
        let test_cases = [("true;", true), ("false;", false)];
        for (input, expect) in test_cases {
            let mut program = Parser::parse(input).unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = program.statements.pop().unwrap();

            let expr = match stmt {
                Statement::ExpressionStmt(v) => v,
                _ => panic!("Expected expression statement only"),
            };
            match expr {
                Expression::Boolean(value) => {
                    assert_eq!(expect, value);
                }
                _ => panic!("Expected boolean literal"),
            }
        }
    }

    #[test]
    fn test_prefix_expressions() {
        use Literal::*;
        let test_cases = [
            ("!5;", "!", IntVal(5)),
            ("-15;", "-", IntVal(15)),
            ("!true;", "!", BoolVal(true)),
            ("!false;", "!", BoolVal(false)),
        ];
        for (input, expected_operator, expected_value) in test_cases {
            let mut program = Parser::parse(input).unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = program.statements.pop().unwrap();
            let expression = match stmt {
                Statement::ExpressionStmt(value) => value,
                _ => panic!("Expected expression statement only"),
            };
            let (operator, expr) = match expression {
                Expression::PrefixExpression { operator, right } => (operator, right),
                _ => panic!("Expected Prefix Expression"),
            };
            assert_eq!(expected_operator, operator);
            is_match_literal_expression(expected_value, &expr).unwrap();
        }
    }

    #[test]
    fn test_infix_expressions() {
        use Literal::*;
        let test_cases = [
            ("5 + 5;", IntVal(5), "+", IntVal(5)),
            ("5 - 5;", IntVal(5), "-", IntVal(5)),
            ("5 * 5;", IntVal(5), "*", IntVal(5)),
            ("5 / 5;", IntVal(5), "/", IntVal(5)),
            ("5 > 5;", IntVal(5), ">", IntVal(5)),
            ("5 < 5;", IntVal(5), "<", IntVal(5)),
            ("5 == 5;", IntVal(5), "==", IntVal(5)),
            ("5 != 5;", IntVal(5), "!=", IntVal(5)),
            ("true == true", BoolVal(true), "==", BoolVal(true)),
            ("true != false", BoolVal(true), "!=", BoolVal(false)),
            ("false == false", BoolVal(false), "==", BoolVal(false)),
        ];
        for (input, left, operator, right) in test_cases {
            let mut program = Parser::parse(input).unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = program.statements.pop().unwrap();
            let expression = match stmt {
                Statement::ExpressionStmt(value) => value,
                _ => panic!("Expected expression statement only"),
            };
            is_match_infix_expression(&expression, left, operator, right).unwrap();
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let test_cases = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in test_cases {
            let program = Parser::parse(input).unwrap();
            let actual = program.to_string();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        // outer statement should be an expression statement
        // retrieve expression from statement
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::FunctionLiteral {
                parameters,
                mut body,
            } => {
                use Literal::StrVal;
                assert!(parameters.len() == 2);
                let params = parameters.into_iter().map(|v| v).collect::<Vec<_>>();
                assert_eq!(params, ["x", "y"]);

                assert!(body.statements.len() == 1);
                match body.statements.pop().unwrap() {
                    Statement::ExpressionStmt(value) => {
                        is_match_infix_expression(&value, StrVal("x"), "+", StrVal("y")).unwrap();
                    }
                    _ => panic!("Expected statement to be ExpressionStmt"),
                };
            }
            _ => panic!("expression should be an FunctionLiteral"),
        };
    }

    #[test]
    fn test_function_parameter_parsing() {
        let test_cases = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x,y,z) {};", vec!["x", "y", "z"]),
        ];
        for (input, expected_params) in test_cases {
            let mut program = Parser::parse(input).unwrap();
            assert!(program.statements.len() == 1);
            // outer statement should be an expression statement
            // retrieve expression from statement
            let stmt = program.statements.pop().unwrap();
            let expr = match stmt {
                Statement::ExpressionStmt(value) => value,
                _ => panic!("statement is not an ExpressionStatement"),
            };

            // the expression should be an if expession
            match expr {
                Expression::FunctionLiteral { parameters, .. } => {
                    let params = parameters.into_iter().map(|v| v).collect::<Vec<_>>();
                    assert_eq!(expected_params, params);
                }
                _ => panic!("expression should be an FunctionLiteral"),
            };
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        // outer statement should be an expression statement
        // retrieve expression from statement
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::CallExpression {
                function,
                arguments,
            } => {
                match *function {
                    Expression::Identifier(ident) => {
                        assert_eq!(ident, "add");
                    }
                    _ => panic!("Expected function in call expression to be identifier"),
                };
                use Literal::IntVal;
                is_match_literal_expression(IntVal(1), &arguments[0]).unwrap();
                is_match_infix_expression(&arguments[1], IntVal(2), "*", IntVal(3)).unwrap();
                is_match_infix_expression(&arguments[2], IntVal(4), "+", IntVal(5)).unwrap();
            }
            _ => panic!("expression should be an CallExpression"),
        };
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        // outer statement should be an expression statement
        // retrieve expression from statement
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::IfExpression {
                condition,
                mut consequence,
                alternative,
            } => {
                use Literal::StrVal;
                is_match_infix_expression(&condition, StrVal("x"), "<", StrVal("y")).unwrap();
                assert!(consequence.statements.len() == 1);

                match consequence.statements.pop().unwrap() {
                    Statement::ExpressionStmt(expr) => {
                        is_match_identifier("x", &expr).unwrap();
                    }
                    _ => panic!("Expected statement to be ExpressionStmt"),
                };

                assert!(alternative.is_none());
            }
            _ => panic!("expression should be an IfExpression"),
        };
    }

    #[test]
    fn test_let_statement() {
        use Literal::*;
        let test_cases = [
            ("let x = 5;", "x", IntVal(5)),
            ("let y = true;", "y", BoolVal(true)),
            ("let foobar = y;", "foobar", StrVal("y")),
        ];
        for (input, expected_identifier, expected_value) in test_cases {
            let mut program = Parser::parse(input).unwrap();
            assert!(program.statements.len() == 1);
            let (got_identifier, got_value) = match program.statements.pop().unwrap() {
                Statement::LetStmt { name, value } => (name, value),
                _ => panic!("Expected LetStmt"),
            };
            assert_eq!(expected_identifier, got_identifier);
            is_match_literal_expression(expected_value, &got_value).unwrap();
        }
    }

    #[test]
    fn test_return_statement() -> eyre::Result<()> {
        use Literal::*;
        let test_cases = [
            ("return 5;", IntVal(5)),
            ("return true;", BoolVal(true)),
            ("return foobar;", StrVal("foobar")),
        ];
        for (input, expected_value) in test_cases {
            let mut program = Parser::parse(input).unwrap();
            assert!(program.statements.len() == 1);
            let got_value = match program.statements.pop().unwrap() {
                Statement::ReturnStmt(value) => value,
                _ => panic!("Expected ReturnStmt"),
            };
            is_match_literal_expression(expected_value, &got_value).unwrap();
        }
        Ok(())
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        // outer statement should be an expression statement
        // retrieve expression from statement
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::IfExpression {
                condition,
                mut consequence,
                alternative,
            } => {
                use Literal::StrVal;
                // condition
                is_match_infix_expression(&condition, StrVal("x"), "<", StrVal("y")).unwrap();

                // check if branch
                assert!(consequence.statements.len() == 1);

                match consequence.statements.pop().unwrap() {
                    Statement::ExpressionStmt(value) => {
                        is_match_identifier("x", &value).unwrap();
                    }
                    _ => panic!("Expected statement to be ExpressionStmt"),
                };

                // check else branch
                let mut alternative = alternative.unwrap();
                assert!(alternative.statements.len() == 1);
                match alternative.statements.pop().unwrap() {
                    Statement::ExpressionStmt(value) => {
                        is_match_identifier("y", &value).unwrap();
                    }
                    _ => panic!("Expected statement to be ExpressionStmt"),
                };
            }
            _ => panic!("expression should be an IfExpression"),
        };
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };
        match expr {
            Expression::StringLiteral(value) => {
                assert_eq!("hello world", value)
            }
            _ => panic!("expression should be a StringLiteral"),
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };
        let elements = match expr {
            Expression::ArrayLiteral(elements) => elements,
            _ => panic!("expression should be a StringLiteral"),
        };
        is_match_integer_literal(1, &elements[0]).unwrap();
        use Literal::IntVal;
        is_match_infix_expression(&elements[1], IntVal(2), "*", IntVal(2)).unwrap();
        is_match_infix_expression(&elements[2], IntVal(3), "+", IntVal(3)).unwrap();
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "my_array[1 + 1]";
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };
        let (left, index) = match expr {
            Expression::IndexExpression { left: l, index: i } => (l, i),
            _ => panic!("expression should be a StringLiteral"),
        };
        use Literal::IntVal;
        is_match_literal_expression(Literal::StrVal("my_array"), &left).unwrap();
        is_match_infix_expression(&index, IntVal(1), "+", IntVal(1)).unwrap();
    }

    #[test]
    fn test_parsing_hash_literal_string_keys() {
        let input = r#"{"one":1, "two": 2, "three":3}"#;
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };
        let entries = match expr {
            Expression::HashLiteral(e) => e,
            _ => panic!("expression should be a StringLiteral"),
        };
        use Literal::IntVal;
        let expected = [("one", 1), ("two", 2), ("three", 3)]
            .iter()
            .map(|(k, v)| (k, IntVal(*v)))
            .collect::<Vec<_>>();
        assert_eq!(expected.len(), entries.len());
        for (ks, vs) in expected
            .into_iter()
            .zip(entries)
            .map(|((ek, ev), (gk, gv))| ((ek, gk), (ev, gv)))
        {
            match ks.1 {
                Expression::StringLiteral(value) => {
                    assert_eq!(ks.0, &value)
                }
                _ => panic!("Expect string literal"),
            }
            is_match_literal_expression(vs.0, &vs.1).unwrap();
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = r#"{ }"#;
        let mut program = Parser::parse(input).unwrap();
        assert!(program.statements.len() == 1);
        let stmt = program.statements.pop().unwrap();
        let expr = match stmt {
            Statement::ExpressionStmt(value) => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };
        let entries = match expr {
            Expression::HashLiteral(e) => e,
            _ => panic!("expression should be a StringLiteral"),
        };
        assert_eq!(0, entries.len());
    }

    #[test]
    #[ignore]
    fn test_fixes_fuzz_crashes() {
        let input = [101, 50, 47];
        if let Ok(s) = std::str::from_utf8(&input) {
            let _ = Parser::parse(s);
        } else {
            panic!("invalid input");
        }
    }
}
