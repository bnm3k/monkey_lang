use crate::ast::{self, Expression, Statement};
use crate::object::Object;

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NULL: Object = Object::Null;

pub fn eval_program(program: ast::Program) -> Object {
    let mut res = NULL;
    for stmt in &program.statements {
        res = eval_statement(stmt);
        match res {
            Object::Return(v) => return *v,
            Object::Error(_) => return res,
            _ => continue,
        }
    }
    res
}

fn eval_block_statements(statements: &Vec<Statement>) -> Object {
    let mut res = NULL;
    for stmt in statements {
        res = eval_statement(stmt);
        match res {
            Object::Return(_) => return res,
            Object::Error(_) => return res,
            _ => continue,
        }
    }
    res
}

fn eval_statement(stmt: &Statement) -> Object {
    use Statement::*;
    match stmt {
        LetStmt { name, value, .. } => eval_expression(value),
        ReturnStmt { value, .. } => {
            let result = eval_expression(value);
            if result.is_err() {
                result
            } else {
                Object::Return(Box::new(result))
            }
        }
        ExpressionStmt { value, .. } => eval_expression(value),
    }
}

fn eval_expression(expr: &Expression) -> Object {
    use Expression::*;
    match expr {
        IntegerLiteral { value, .. } => Object::Int(*value),
        Boolean { value, .. } => native_bool_to_bool_obj(*value),
        PrefixExpression {
            operator, right, ..
        } => {
            let right = eval_expression(right);
            if right.is_err() {
                return right;
            }
            eval_prefix_expression(operator, &right)
        }
        InfixExpression {
            left,
            operator,
            right,
            ..
        } => {
            let left = eval_expression(left);
            if left.is_err() {
                return left;
            }
            let right = eval_expression(right);
            if right.is_err() {
                return right;
            }
            eval_infix_expression(operator, &left, &right)
        }
        IfExpression {
            condition,
            consequence,
            alternative,
            ..
        } => {
            let condition = eval_expression(condition);
            if condition.is_err() {
                return condition;
            }
            if is_truthy(&condition) {
                eval_block_statements(&consequence.statements)
            } else {
                if let Some(alt) = alternative {
                    eval_block_statements(&alt.statements)
                } else {
                    NULL
                }
            }
        }
        _ => NULL,
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null | Object::Bool(false) => false,
        _ => true,
    }
}

fn native_bool_to_bool_obj(b: bool) -> Object {
    if b {
        TRUE
    } else {
        FALSE
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    use Object::*;
    match (left, operator, right) {
        (Int(l), _, Int(r)) => eval_integer_infix_expression(operator, *l, *r),
        (Bool(l), "==", Bool(r)) => native_bool_to_bool_obj(l == r),
        (Bool(l), "!=", Bool(r)) => native_bool_to_bool_obj(l != r),
        (Bool(_), _, Bool(_)) => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.type_as_str(),
            operator,
            right.type_as_str()
        )),
        (_, _, _) => Object::Error(format!(
            "type mismatch: {} {} {}",
            left.type_as_str(),
            operator,
            right.type_as_str()
        )),
    }
}

fn eval_integer_infix_expression(operator: &str, l: i64, r: i64) -> Object {
    use Object::Int;
    match operator {
        "+" => Int(l + r),
        "-" => Int(l - r),
        "*" => Int(l * r),
        "/" => Int(l / r),
        "<" => native_bool_to_bool_obj(l < r),
        ">" => native_bool_to_bool_obj(l > r),
        "==" => native_bool_to_bool_obj(l == r),
        "!=" => native_bool_to_bool_obj(l != r),
        _ => Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_prefix_expression(operator: &String, right: &Object) -> Object {
    if operator == "!" {
        return eval_bang_operator_expression(right);
    } else if operator == "-" {
        return eval_minus_prefix_operator_expression(right);
    } else {
        Object::Error(format!(
            "unknown operator: {}{}",
            operator,
            right.type_as_str()
        ))
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    if let Object::Int(v) = right {
        Object::Int(-v)
    } else {
        Object::Error(format!("unknown operator: -{}", right.type_as_str()))
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    use Object::*;
    match right {
        Bool(v) => native_bool_to_bool_obj(!v),
        Null => TRUE,
        _ => FALSE,
    }
}

#[cfg(test)]
mod evaluator_tests {
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::*;

    fn is_match_integer_obj(expected: i64, obj: Object) -> Result<(), String> {
        let got = match obj {
            Object::Int(v) => v,
            _ => return Err(format!("Object is not an integer, got: {:?}", obj)),
        };
        if expected != got {
            return Err(format!("Expected {}, got {}", expected, got));
        }
        Ok(())
    }

    fn is_match_bool_obj(expected: bool, obj: Object) -> Result<(), String> {
        let got = match obj {
            Object::Bool(v) => v,
            _ => return Err(format!("Object is not a bool, got: {:?}", obj)),
        };
        if expected != got {
            return Err(format!("Expected {}, got {}", expected, got));
        }
        Ok(())
    }

    fn is_match_obj(expected: Object, got: Object) -> Result<(), String> {
        use Object::*;
        match (&expected, &got) {
            (Null, Null) => Ok(()),
            (Bool(v), _) => is_match_bool_obj(*v, got),
            (Int(v), _) => is_match_integer_obj(*v, got),
            (_, _) => Err(format!("Expected {:?}, got {:?}", expected, got)),
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        let test_cases = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in test_cases {
            let program = Parser::parse(input).unwrap();
            let got = eval_program(program);
            is_match_bool_obj(expected, got).unwrap();
        }
    }

    #[test]
    fn test_bang_operator() {
        let test_cases = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in test_cases {
            let program = Parser::parse(input).unwrap();
            let got = eval_program(program);
            is_match_bool_obj(expected, got).unwrap();
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let test_cases = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in test_cases {
            let program = Parser::parse(input).unwrap();
            let res = eval_program(program);
            is_match_integer_obj(expected, res).unwrap();
        }
    }

    #[test]
    fn test_if_else_expressions() {
        use Object::Int;
        let test_cases = [
            ("if (true) { 10 }", Int(10)),
            ("if (false) { 10 }", NULL),
            ("if (1) { 10 }", Int(10)),
            ("if (1 < 2) { 10 }", Int(10)),
            ("if (1 > 2) { 10 }", NULL),
            ("if (1 > 2) { 10 } else { 20 }", Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Int(10)),
        ];

        for (input, expected) in test_cases {
            let program = Parser::parse(input).unwrap();
            let got = eval_program(program);
            is_match_obj(expected, got).unwrap();
        }
    }

    #[test]
    fn test_return_statements() {
        let test_cases = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
        ];

        for (input, expected) in test_cases {
            let program = Parser::parse(input).unwrap();
            let got = eval_program(program);
            is_match_integer_obj(expected, got).unwrap();
        }
    }

    #[test]
    fn test_error_handling() {
        let test_cases = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                " if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];
        for (i, (input, expected)) in test_cases.into_iter().enumerate() {
            let program = Parser::parse(input).unwrap();
            let got = eval_program(program);
            if let Object::Error(msg) = got {
                assert_eq!(expected, msg)
            } else {
                panic!(
                    "[{}] Expected Error object, instead got: {}",
                    i,
                    got.inspect()
                )
            }
        }
    }
}
