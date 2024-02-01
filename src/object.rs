use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BlockStatement, Identifier};

#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
    Function(Function),
    Return(Box<Object>),
    Error(String),
}

pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Function {
    fn inspect(&self) -> String {
        let parts = [
            "fn(",
            &self
                .parameters
                .iter()
                .map(|v| v.value.clone())
                .intersperse(String::from(", "))
                .collect::<String>(),
            ") {\n",
            &self.body.to_string(),
            "\n",
        ];
        parts.into_iter().collect::<String>()
    }
}

impl std::clone::Clone for Function {
    fn clone(&self) -> Self {
        Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: self.env.clone(),
        }
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

#[derive(Clone)]
pub struct Environment {
    outer: Option<Rc<Environment>>,
    curr: HashMap<String, Rc<Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            outer: None,
            curr: HashMap::new(),
        }
    }

    pub fn with_outer(outer: Rc<Environment>) -> Self {
        Environment {
            outer: Some(outer),
            curr: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        if let Some(v) = self.curr.get(name) {
            Some(v.clone())
        } else {
            if let Some(outer) = &self.outer {
                outer.get(name)
            } else {
                None
            }
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.curr.insert(name.into(), Rc::new(val));
    }
}

impl Object {
    pub fn is_err(&self) -> bool {
        if let Object::Error(_) = self {
            true
        } else {
            false
        }
    }
    pub fn inspect(&self) -> String {
        use Object::*;
        match self {
            Int(v) => v.to_string(),
            Bool(v) => v.to_string(),
            Null => "null".to_string(),
            Return(v) => v.inspect(),
            Error(msg) => format!("ERROR: {}", msg),
            Function(f) => f.inspect(),
        }
    }

    pub fn type_as_str(&self) -> &str {
        use Object::*;
        match self {
            Int(_) => "INTEGER",
            Bool(_) => "BOOLEAN",
            Null => "NULL",
            Return(_) => "RETURN_VALUE",
            Error(_) => "ERROR",
            Function { .. } => "FUNCTION_OBJ",
        }
    }
}
