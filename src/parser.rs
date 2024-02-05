use std::collections::VecDeque;
use std::fmt::Debug;

use crate::ast::{BlockStatement, Expression, Identifier, Node, Program, Statement};
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
        // TODO: is this necessary? maybe implement clone on token
        let token_clone = Token::new(TokenType::IDENT, expression.token_literal().into());
        let stmt = Statement::ExpressionStmt {
            token: token_clone,
            value: expression,
        };

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
                _ => return Some(left_expression),
            };
            left_expression = got.unwrap()
        }
        Some(left_expression)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let lparen_token = self.tokens.pop_front().unwrap();
        assert_eq!(TokenType::LPAREN, lparen_token.token_type);
        let arguments = self.parse_call_arguments()?;
        Some(Expression::CallExpression {
            token: lparen_token,
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut arguments = Vec::new();
        // handle empty argument list
        if self.peek_token_is(TokenType::RPAREN) {
            let _ = self.tokens.pop_front();
            return Some(arguments);
        }
        // handle first arg
        let first_arg = self.parse_expression(Precedence::Lowest)?;
        arguments.push(first_arg);
        while self.peek_token_is(TokenType::COMMA) {
            let _ = self.tokens.pop_front();
            let arg = self.parse_expression(Precedence::Lowest)?;
            arguments.push(arg);
        }
        if !self.peek_expect_or_set_err(TokenType::RPAREN) {
            return None;
        } else {
            let _ = self.tokens.pop_front();
        }
        Some(arguments)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator_token = self.tokens.pop_front().unwrap();
        let tt = operator_token.token_type;
        assert!(tt == TokenType::BANG || tt == TokenType::MINUS);
        let right = Box::new(self.parse_expression(Precedence::Prefix)?);
        let operator = operator_token.literal.clone();
        Some(Expression::PrefixExpression {
            token: operator_token,
            operator,
            right,
        })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        // fn token
        let fn_token = self.tokens.pop_front().unwrap();
        assert!(fn_token.token_type == TokenType::FUNCTION);
        let parameters = self.parse_function_parameters()?;
        let body = self.parse_block_statement()?;
        Some(Expression::FunctionLiteral {
            token: fn_token,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
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
            let ident_token = p.tokens.pop_front().unwrap();
            let value = ident_token.literal.clone();
            Some(Identifier {
                token: ident_token,
                value,
            })
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
            token: if_token,
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
        let operator_token = self.tokens.pop_front().unwrap();
        use TokenType::*;
        assert!(
            [PLUS, MINUS, SLASH, ASTERISK, EQ, NOT_EQ, LT, GT].contains(&operator_token.token_type)
        );
        let operator = operator_token.literal.clone();
        let curr_precedence = token_type_precedence(&operator_token).unwrap_or(Precedence::Lowest);
        let right = Box::new(self.parse_expression(curr_precedence).unwrap());
        Some(Expression::InfixExpression {
            token: operator_token,
            left: Box::new(left),
            operator,
            right,
        })
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let ident_token = self.tokens.pop_front().unwrap();
        assert!(ident_token.token_type == TokenType::IDENT);
        let value = ident_token.literal.clone();
        let id = Identifier {
            token: ident_token,
            value,
        };
        Some(Expression::Identifier(id))
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        let bool_token = self.tokens.pop_front().unwrap();
        // assert handled implicitly below
        let value = match bool_token.token_type {
            TokenType::TRUE => true,
            TokenType::FALSE => false,
            _ => panic!("parse_boolean should only be called on a bool token type"),
        };
        Some(Expression::Boolean {
            token: bool_token,
            value,
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let int_token = self.tokens.pop_front().unwrap();
        assert!(int_token.token_type == TokenType::INT);
        let value = match int_token.literal.parse::<i64>() {
            Ok(v) => v,
            Err(e) => {
                self.error_msgs.push(format!(
                    "Could not parse {} as integer: {}",
                    int_token.literal, e
                ));
                return None;
            }
        };
        Some(Expression::IntegerLiteral {
            value,
            token: int_token,
        })
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        let str_token = self.tokens.pop_front().unwrap();
        assert!(str_token.token_type == TokenType::STRING);
        let value = str_token.literal.clone();
        Some(Expression::StringLiteral {
            token: str_token,
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        // first token should be return
        let return_token = self.tokens.pop_front().unwrap();
        assert!(return_token.token_type == TokenType::RETURN);

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            let _ = self.tokens.pop_front();
        }

        Some(Statement::ReturnStmt {
            token: return_token,
            value,
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
            return None;
        }
        let _ = self.tokens.pop_front();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            let _ = self.tokens.pop_front();
        }

        Some(Statement::LetStmt {
            token: let_token,
            name: identifier,
            value,
        })
    }
    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        // lbrace
        if !self.peek_expect_or_set_err(TokenType::LBRACE) {
            return None;
        }
        let lbrace_token = self.tokens.pop_front().unwrap();

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

        Some(BlockStatement {
            token: lbrace_token,
            statements,
        })
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
        let program = Parser::parse(input).expect("should be parsed successfully");
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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let mut program = Parser::parse(input).unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements.pop().unwrap();

        match stmt {
            Statement::ExpressionStmt { value, .. } => {
                if let Expression::Identifier(v) = value {
                    assert_eq!("foobar", v.value);
                    assert_eq!("foobar", v.token_literal());
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
            Statement::ExpressionStmt { ref value, .. } => {
                is_match_integer_literal(55, value).unwrap();
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
                Statement::ExpressionStmt { value, .. } => value,
                _ => panic!("Expected expression statement only"),
            };
            match expr {
                Expression::Boolean { value, .. } => {
                    assert_eq!(expect, value);
                }
                _ => panic!("Expected boolean literal"),
            }
        }
    }

    fn is_match_integer_literal(expect: i64, expr: &Expression) -> Result<(), &'static str> {
        if let Expression::IntegerLiteral { value: got, .. } = expr {
            if expect != *got {
                return Err("values do not match");
            }
            if got.to_string() != expr.token_literal() {
                return Err("token literal values do not match");
            }
        } else {
            return Err("Expected expression to be Integer Literal");
        }
        Ok(())
    }

    fn is_match_bool_literal(expect: bool, expr: &Expression) -> Result<(), &'static str> {
        if let Expression::Boolean { value: got, .. } = expr {
            if expect != *got {
                return Err("values do not match");
            }
            if got.to_string() != expr.token_literal() {
                return Err("token literal values do not match");
            }
        } else {
            return Err("Expected expression to be Boolean Literal");
        }
        Ok(())
    }

    fn is_match_identifier(expect: &str, expr: &Expression) -> Result<(), &'static str> {
        if let Expression::Identifier(identifier) = expr {
            if identifier.value != expect {
                return Err("values do not match");
            }
            if identifier.token_literal() != expect {
                return Err("token literal values do not match");
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
                ..
            } => (left, operator, right),
            _ => panic!("Expect infix expression"),
        };
        is_match_literal_expression(left, got_left)?;
        assert_eq!(operator, got_operator);
        is_match_literal_expression(right, got_right)?;
        Ok(())
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
                Statement::ExpressionStmt { value, .. } => value,
                _ => panic!("Expected expression statement only"),
            };
            let (operator, expr) = match expression {
                Expression::PrefixExpression {
                    operator, right, ..
                } => (operator, right),
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
                Statement::ExpressionStmt { value, .. } => value,
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
            Statement::ExpressionStmt { value, .. } => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::FunctionLiteral {
                parameters,
                mut body,
                ..
            } => {
                use Literal::StrVal;
                assert!(parameters.len() == 2);
                let params = parameters.into_iter().map(|v| v.value).collect::<Vec<_>>();
                assert_eq!(params, ["x", "y"]);

                assert!(body.statements.len() == 1);
                match body.statements.pop().unwrap() {
                    Statement::ExpressionStmt { value: expr, .. } => {
                        is_match_infix_expression(&expr, StrVal("x"), "+", StrVal("y")).unwrap();
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
                Statement::ExpressionStmt { value, .. } => value,
                _ => panic!("statement is not an ExpressionStatement"),
            };

            // the expression should be an if expession
            match expr {
                Expression::FunctionLiteral { parameters, .. } => {
                    let params = parameters.into_iter().map(|v| v.value).collect::<Vec<_>>();
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
            Statement::ExpressionStmt { value, .. } => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::CallExpression {
                function,
                arguments,
                ..
            } => {
                match *function {
                    Expression::Identifier(ident) => {
                        assert_eq!(ident.value, "add");
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
            Statement::ExpressionStmt { value, .. } => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::IfExpression {
                condition,
                mut consequence,
                alternative,
                ..
            } => {
                use Literal::StrVal;
                is_match_infix_expression(&condition, StrVal("x"), "<", StrVal("y")).unwrap();
                assert!(consequence.statements.len() == 1);

                match consequence.statements.pop().unwrap() {
                    Statement::ExpressionStmt {
                        value: ref expr, ..
                    } => {
                        is_match_identifier("x", expr).unwrap();
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
                Statement::LetStmt { name, value, .. } => (name.value, value),
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
                Statement::ReturnStmt { value, .. } => value,
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
            Statement::ExpressionStmt { value, .. } => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };

        // the expression should be an if expession
        match expr {
            Expression::IfExpression {
                condition,
                mut consequence,
                alternative,
                ..
            } => {
                use Literal::StrVal;
                // condition
                is_match_infix_expression(&condition, StrVal("x"), "<", StrVal("y")).unwrap();

                // check if branch
                assert!(consequence.statements.len() == 1);

                match consequence.statements.pop().unwrap() {
                    Statement::ExpressionStmt { value: expr, .. } => {
                        is_match_identifier("x", &expr).unwrap();
                    }
                    _ => panic!("Expected statement to be ExpressionStmt"),
                };

                // check else branch
                let mut alternative = alternative.unwrap();
                assert!(alternative.statements.len() == 1);
                match alternative.statements.pop().unwrap() {
                    Statement::ExpressionStmt { value: expr, .. } => {
                        is_match_identifier("y", &expr).unwrap();
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
            Statement::ExpressionStmt { value, .. } => value,
            _ => panic!("statement is not an ExpressionStatement"),
        };
        match expr {
            Expression::StringLiteral { value, .. } => {
                assert_eq!("hello world", value)
            }
            _ => panic!("expression should be a StringLiteral"),
        }
    }
}
