#![feature(iter_intersperse)]

mod evaluator;
mod object;
mod parser;

mod ast;
mod builtins;
mod lexer;
mod token;

use std::{cell::RefCell, rc::Rc};

use crate::evaluator::eval_program;
use crate::object::Environment;
use crate::parser::Parser;

pub struct Monkey {
    env: Rc<RefCell<Environment>>,
}

impl Monkey {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn eval(&mut self, input: &str) {
        let program = match Parser::parse(input) {
            Ok(program) => program,
            Err(e) => {
                println!("{}", e);
                return;
            }
        };
        // eval
        let res = eval_program(&self.env, program);
        println!("{}", res.inspect());
    }
}

// for testing (fuzzying)
pub fn parse_test_input(input: &str) -> Option<()> {
    match Parser::parse(input) {
        Ok(_) => Some(()),
        Err(_) => None,
    }
}
