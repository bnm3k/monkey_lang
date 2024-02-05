use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BlockStatement, Identifier};

#[derive(Debug)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
    Function(Function),
    Return(Rc<Object>),
    Error(String),
}

impl Object {
    pub fn null() -> Rc<Object> {
        Rc::new(Object::Null)
    }

    pub fn bool(b: bool) -> Rc<Object> {
        if b {
            Rc::new(Object::Bool(true))
        } else {
            Rc::new(Object::Bool(false))
        }
    }

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

pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
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

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

pub struct Environment {
    outer: Option<Rc<RefCell<Environment>>>,
    curr: HashMap<String, Rc<Object>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            outer: None,
            curr: HashMap::new(),
        }))
    }

    pub fn with_outer(outer: &Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            outer: Some(Rc::clone(outer)),
            curr: HashMap::new(),
        }))
    }

    fn do_set(&mut self, name: &str, val: &Rc<Object>) {
        self.curr.insert(name.into(), Rc::clone(&val));
    }

    fn do_get(&self, name: &str) -> Option<Rc<Object>> {
        if let Some(val) = self.curr.get(name) {
            Some(Rc::clone(val))
        } else {
            if let Some(outer) = &self.outer {
                outer.borrow().do_get(name)
            } else {
                None
            }
        }
    }

    pub fn set(env: &RefCell<Environment>, name: &str, val: &Rc<Object>) {
        let mut inner = env.borrow_mut();
        inner.do_set(name, val);
    }

    pub fn get(env: &RefCell<Environment>, name: &str) -> Option<Rc<Object>> {
        env.borrow().do_get(name)
    }
}
