#[derive(Debug)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
    Return(Box<Object>),
    Error(String),
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
        }
    }
}
