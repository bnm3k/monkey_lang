use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{self, Expression, Statement};
use crate::object::{Environment, Function, Object};

type Env = Rc<RefCell<Environment>>;

pub fn eval_program(env: &Env, program: ast::Program) -> Rc<Object> {
    let mut res = Object::null();
    for stmt in &program.statements {
        res = eval_statement(env, stmt);
        match &*res {
            Object::Return(v) => return v.clone(),
            Object::Error(_) => return res,
            _ => continue,
        }
    }
    res
}

fn eval_block_statements(env: &Env, statements: &Vec<Statement>) -> Rc<Object> {
    let mut res = Object::null();
    for stmt in statements {
        res = eval_statement(env, stmt);
        match *res {
            Object::Return(_) => return res,
            Object::Error(_) => return res,
            _ => continue,
        }
    }
    res
}

fn eval_statement(env: &Env, stmt: &Statement) -> Rc<Object> {
    use Statement::*;
    match stmt {
        LetStmt { name, value, .. } => {
            let result = eval_expression(env, value);
            Environment::set(env, &name.value, &result);
            result
        }
        ReturnStmt { value, .. } => {
            let result = eval_expression(env, value);
            if result.is_err() {
                result
            } else {
                Rc::new(Object::Return(result))
            }
        }
        ExpressionStmt { value, .. } => eval_expression(env, value),
    }
}

fn eval_expression(env: &Env, expr: &Expression) -> Rc<Object> {
    use Expression::*;
    match expr {
        IntegerLiteral { value, .. } => Rc::new(Object::Int(*value)),
        Boolean { value, .. } => Object::bool(*value),
        PrefixExpression {
            operator, right, ..
        } => {
            let right = eval_expression(env, right);
            if right.is_err() {
                return right;
            }
            eval_prefix_expression(env, operator, &right)
        }
        InfixExpression {
            left,
            operator,
            right,
            ..
        } => {
            let left = eval_expression(env, left);
            if left.is_err() {
                return left;
            }
            let right = eval_expression(env, right);
            if right.is_err() {
                return right;
            }
            eval_infix_expression(env, operator, &left, &right)
        }
        IfExpression {
            condition,
            consequence,
            alternative,
            ..
        } => {
            let condition = eval_expression(env, condition);
            if condition.is_err() {
                return condition;
            }
            if is_truthy(&condition) {
                eval_block_statements(env, &consequence.statements)
            } else {
                if let Some(alt) = alternative {
                    eval_block_statements(env, &alt.statements)
                } else {
                    Object::null()
                }
            }
        }
        Identifier(identifier) => {
            if let Some(obj) = Environment::get(env, &identifier.value) {
                obj
            } else {
                to_err_obj(format!("identifier not found: {}", &identifier.value))
            }
        }
        FunctionLiteral {
            parameters, body, ..
        } => {
            // TODO: too much cloning, not a good idea you know
            let parameters = parameters
                .iter()
                .map(|v| v.value.clone())
                .collect::<Vec<_>>();
            let function = Function {
                parameters,
                body: body.clone(),
                env: env.clone(),
            };
            Rc::new(Object::Function(function))
        }
        CallExpression {
            function,
            arguments,
            ..
        } => {
            let res = eval_expression(env, function);
            if res.is_err() {
                return res;
            }
            let mut evaluated_args = eval_expressions(env, arguments);
            if evaluated_args.len() == 1 && evaluated_args[0].is_err() {
                return evaluated_args.remove(0); // TODO bad idea
            }
            // TODO: there should be a better way
            if let Object::Function(f) = &*res {
                if f.parameters.len() != evaluated_args.len() {
                    return to_err_obj(format!(
                        "Invalid number of arguments for function: expected {}, got {}",
                        f.parameters.len(),
                        evaluated_args.len(),
                    ));
                }
                let mut extended_env = Environment::with_outer(&f.env);

                for (obj, name) in evaluated_args.into_iter().zip(f.parameters.iter()) {
                    Environment::set(&extended_env, &name, &obj);
                }
                let result = eval_block_statements(&mut extended_env, &f.body.statements);
                // TODO: is there a better way?
                match &*result {
                    Object::Return(v) => v.clone(),
                    _ => result,
                }
            } else {
                to_err_obj(format!(
                    "Attempting call on object that is not a function. Got: {:?}",
                    res
                ))
            }
        }
        _ => panic!("Should not reach here"),
    }
}

fn eval_expressions(env: &Env, exprs: &Vec<Expression>) -> Vec<Rc<Object>> {
    let mut results = Vec::new();
    for expr in exprs {
        let result = eval_expression(env, expr);
        if result.is_err() {
            return vec![result];
        }
        results.push(result);
    }
    return results;
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null | Object::Bool(false) => false,
        _ => true,
    }
}

fn eval_infix_expression(_env: &Env, operator: &str, left: &Object, right: &Object) -> Rc<Object> {
    use Object::*;
    match (left, operator, right) {
        (Int(l), _, Int(r)) => eval_integer_infix_expression(operator, *l, *r),
        (Bool(l), "==", Bool(r)) => Object::bool(l == r),
        (Bool(l), "!=", Bool(r)) => Object::bool(l != r),
        (Bool(_), _, Bool(_)) => to_err_obj(format!(
            "unknown operator: {} {} {}",
            left.type_as_str(),
            operator,
            right.type_as_str()
        )),
        (_, _, _) => to_err_obj(format!(
            "type mismatch: {} {} {}",
            left.type_as_str(),
            operator,
            right.type_as_str()
        )),
    }
}

fn eval_integer_infix_expression(operator: &str, l: i64, r: i64) -> Rc<Object> {
    let int = |n| Rc::new(Object::Int(n));
    match operator {
        "+" => int(l + r),
        "-" => int(l - r),
        "*" => int(l * r),
        "/" => int(l / r),
        "<" => Object::bool(l < r),
        ">" => Object::bool(l > r),
        "==" => Object::bool(l == r),
        "!=" => Object::bool(l != r),
        _ => to_err_obj(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn to_err_obj(msg: String) -> Rc<Object> {
    Rc::new(Object::Error(msg))
}

fn eval_prefix_expression(_env: &Env, operator: &String, right: &Object) -> Rc<Object> {
    if operator == "!" {
        use Object::*;
        match right {
            Bool(v) => Object::bool(!v),
            Null => Object::bool(true),
            _ => Object::bool(false),
        }
    } else if operator == "-" {
        if let Object::Int(v) = right {
            Rc::new(Object::Int(-v))
        } else {
            to_err_obj(format!("unknown operator: -{}", right.type_as_str()))
        }
    } else {
        to_err_obj(format!(
            "unknown operator: {}{}",
            operator,
            right.type_as_str()
        ))
    }
}

#[cfg(test)]
mod evaluator_tests {
    use eyre::anyhow;

    use crate::{
        object::Object,
        parser::{Parser, ParserError},
    };

    use super::*;

    #[derive(Debug)]
    struct TestError(String);
    impl std::fmt::Display for TestError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }
    impl std::error::Error for TestError {}
    impl Into<TestError> for String {
        fn into(self) -> TestError {
            TestError(self)
        }
    }

    fn do_eval(input: &str) -> Result<Rc<Object>, ParserError> {
        let mut env = Environment::new();
        let program = Parser::parse(input)?;
        return Ok(eval_program(&mut env, program));
    }

    fn is_match_integer_obj(expected: &i64, obj: &Object) -> Result<(), TestError> {
        let got = match obj {
            Object::Int(v) => v,
            _ => return Err(format!("Object is not an integer, got: {:?}", obj).into()),
        };
        if expected != got {
            return Err(format!("Expected {}, got {}", expected, got).into());
        }
        Ok(())
    }

    fn is_match_bool_obj(expected: &bool, obj: &Object) -> Result<(), TestError> {
        let got = match obj {
            Object::Bool(v) => v,
            _ => return Err(format!("Object is not a bool, got: {:?}", obj).into()),
        };
        if expected != got {
            return Err(format!("Expected {}, got {}", expected, got).into());
        }
        Ok(())
    }

    fn is_match_obj(expected: &Object, got: &Object) -> Result<(), TestError> {
        use Object::*;
        match (expected, &got) {
            (Null, Null) => Ok(()),
            (Bool(v), _) => is_match_bool_obj(v, got),
            (Int(v), _) => is_match_integer_obj(v, got),
            (_, _) => Err(format!("Expected {:?}, got {:?}", expected, got).into()),
        }
    }

    #[test]
    fn test_eval_bool_expression() -> eyre::Result<()> {
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
            let got = do_eval(input)?;
            is_match_bool_obj(&expected, &got)?;
        }
        Ok(())
    }

    #[test]
    fn test_bang_operator() -> eyre::Result<()> {
        let test_cases = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in test_cases {
            let got = do_eval(input)?;
            is_match_bool_obj(&expected, &got)?;
        }
        Ok(())
    }

    #[test]
    fn test_eval_integer_expression() -> eyre::Result<()> {
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
            let got = do_eval(input)?;
            is_match_integer_obj(&expected, &got)?;
        }
        Ok(())
    }

    #[test]
    fn test_if_else_expressions() -> eyre::Result<()> {
        let int_val = |v| Rc::new(Object::Int(v));
        let test_cases = [
            ("if (true) { 10 }", int_val(10)),
            ("if (false) { 10 }", Object::null()),
            ("if (1) { 10 }", int_val(10)),
            ("if (1 < 2) { 10 }", int_val(10)),
            ("if (1 > 2) { 10 }", Object::null()),
            ("if (1 > 2) { 10 } else { 20 }", int_val(20)),
            ("if (1 < 2) { 10 } else { 20 }", int_val(10)),
        ];

        for (input, expected) in test_cases {
            let mut env = Environment::new();
            let program = Parser::parse(input)?;
            let got = eval_program(&mut env, program);
            is_match_obj(&expected, &got)?;
        }
        Ok(())
    }

    #[test]
    fn test_return_statements() -> eyre::Result<()> {
        let test_cases = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
        ];

        for (input, expected) in test_cases {
            let got = do_eval(input)?;
            is_match_integer_obj(&expected, &got)?;
        }
        Ok(())
    }

    #[test]
    fn test_error_handling() -> eyre::Result<()> {
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
            ("foobar", "identifier not found: foobar"),
        ];
        for (i, (input, expected)) in test_cases.into_iter().enumerate() {
            let got = do_eval(input)?;
            if let Object::Error(msg) = &*got {
                assert_eq!(expected, msg)
            } else {
                panic!(
                    "[{}] Expected Error object, instead got: {}",
                    i,
                    got.inspect()
                )
            }
        }
        Ok(())
    }

    #[test]
    fn test_let_statements() -> eyre::Result<()> {
        let test_cases = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for (input, expected) in test_cases {
            let got = do_eval(input)?;
            is_match_integer_obj(&expected, &got)?;
        }
        Ok(())
    }

    #[test]
    fn test_function_obj() -> eyre::Result<()> {
        let input = "fn(x) {x + 2; }";
        let evaluated = do_eval(input)?;
        match &*evaluated {
            Object::Function(f) => {
                assert_eq!(1, f.parameters.len());
                assert_eq!("x", f.parameters[0]);
                assert_eq!("(x + 2)", f.body.to_string());
            }
            _ => panic!("Object is not Function. got {:?}", evaluated),
        }
        Ok(())
    }

    #[test]
    fn test_function_application() -> eyre::Result<()> {
        let test_cases = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];
        for (input, expected) in test_cases {
            let got = do_eval(input)?;
            is_match_integer_obj(&expected, &got)?;
        }
        Ok(())
    }

    #[test]
    fn test_closures() -> eyre::Result<()> {
        let input = r#"
        let new_adder = fn(x) {
            fn(y) { x + y};
        };

        let add_two = new_adder(2);
        add_two(3);
        "#;
        let got = do_eval(input)?;
        let expected = 5;
        is_match_integer_obj(&expected, &got)?;
        Ok(())
    }
}
