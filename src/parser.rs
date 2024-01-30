use std::collections::VecDeque;

use crate::ast::{Expression, Identifier, Node, Program, Statement};
use crate::{
    ast,
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFn = dyn Fn() -> Expression;
type InfixParseFn = dyn Fn(Expression) -> Expression;

pub struct Parser {
    tokens: VecDeque<Token>,
    error_msgs: Vec<String>,
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
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
        _ => return None,
    };
    Some(p)
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let lexer = Lexer::new(input);
        let tokens = lexer.collect::<VecDeque<_>>();
        return Parser {
            tokens,
            error_msgs: Vec::new(),
        };
    }

    fn peek_precedence(&self) -> Precedence {
        self.tokens
            .front()
            .and_then(token_type_precedence)
            .unwrap_or(Precedence::Lowest)
    }

    pub fn parse_program(mut self) -> Result<ast::Program, Vec<String>> {
        let mut statements = Vec::<Statement>::new();
        loop {
            if self.peek_token_is(TokenType::EOF) {
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
        // after expression we might or might not get a semi-colon
        // TODO: change for other statements too to handle peeked semicolon
        // without setting err
        // if self.peek_expect_or_set_err(TokenType::SEMICOLON) {
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.tokens.pop_front();
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
                _ => return Some(left_expression),
            };
            left_expression = got.unwrap()
        }
        Some(left_expression)
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
            right: right,
        })
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
            operator: operator,
            right: right,
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
            value: Expression::Empty,
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
            value: Expression::Empty,
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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let mut program = Parser::new(input).parse_program().unwrap();
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
        let mut program = Parser::new(input).parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements.pop().unwrap();

        match stmt {
            Statement::ExpressionStmt { value, .. } => {
                is_match_integer_literal(55, value).unwrap();
            }
            _ => panic!("Expected expression statement only"),
        }
    }

    #[test]
    fn test_boolean_expression() {
        let test_cases = [("true;", true), ("false;", false)];
        for (input, expect) in test_cases {
            let mut program = Parser::new(input).parse_program().unwrap();
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

    fn is_match_integer_literal(expect: i64, expr: Expression) -> Result<(), &'static str> {
        if let Expression::IntegerLiteral { value: got, .. } = expr {
            if expect != got {
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

    fn is_match_bool_literal(expect: bool, expr: Expression) -> Result<(), &'static str> {
        if let Expression::Boolean { value: got, .. } = expr {
            if expect != got {
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

    fn is_match_identifier(expect: &str, expr: Expression) -> Result<(), &'static str> {
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

    fn is_match_literal_expression(expect: Literal, expr: Expression) -> Result<(), &'static str> {
        use Literal::*;
        match expect {
            IntVal(expect) => is_match_integer_literal(expect, expr),
            StrVal(expect) => is_match_identifier(expect, expr),
            BoolVal(expect) => is_match_bool_literal(expect, expr),
        }
    }

    fn is_match_infix_expression(
        expr: Expression,
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
        is_match_literal_expression(left, *got_left)?;
        assert_eq!(operator, got_operator);
        is_match_literal_expression(right, *got_right)?;
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
            let mut program = Parser::new(input).parse_program().unwrap();
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
            is_match_literal_expression(expected_value, *expr).unwrap();
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
            let mut program = Parser::new(input).parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = program.statements.pop().unwrap();
            let expression = match stmt {
                Statement::ExpressionStmt { value, .. } => value,
                _ => panic!("Expected expression statement only"),
            };
            is_match_infix_expression(expression, left, operator, right).unwrap();
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
        ];

        for (input, expected) in test_cases {
            let program = Parser::new(input).parse_program().unwrap();
            let actual = program.to_string();
            assert_eq!(expected, actual);
        }
    }
}
