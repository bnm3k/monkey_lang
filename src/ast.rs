use crate::token::{self, Token};

pub trait Node: ToString {
    fn token_literal(&self) -> &str;
}

pub enum Statement {
    LetStmt {
        token: Token, // let token
        name: Identifier,
        value: Box<dyn Expression>,
    },
    ReturnStmt {
        token: Token, // return token
        value: Box<dyn Expression>,
    },
    ExpressionStmt {
        token: Token, // first token of the expression
        value: Box<dyn Expression>,
    },
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        use Statement::*;
        match self {
            LetStmt { token, .. } => &token.literal,
            ReturnStmt { token, .. } => &token.literal,
            ExpressionStmt { token, .. } => &token.literal,
        }
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        use Statement::*;
        match self {
            LetStmt { token, name, value } => {
                let mut out = String::new();
                out.push_str(self.token_literal()); // let
                out.push(' ');
                out.push_str(&name.to_string()); // identifier
                out.push_str(" = ");
                out.push_str(&value.to_string());
                out
            }
            ReturnStmt { token, value } => {
                let mut out = String::new();
                out.push_str(self.token_literal());
                out.push(' ');
                out.push_str(&value.to_string());
                out
            }
            ExpressionStmt { value, .. } => value.to_string(),
        }
    }
}

pub trait Expression: Node {}

pub struct PlaceholderExpression {}

impl PlaceholderExpression {
    pub fn TODO_Remove() -> Box<dyn Expression> {
        Box::new(Self {})
    }
}

impl Expression for PlaceholderExpression {}

impl Node for PlaceholderExpression {
    fn token_literal(&self) -> &str {
        return "plaholder token";
    }
}

impl ToString for PlaceholderExpression {
    fn to_string(&self) -> String {
        "PLACEHOLDER_VALUE".into()
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let mut out = String::new();
        for stmt in &self.statements {
            out.push_str(&stmt.to_string());
        }
        return out;
    }
}
// Expressions

pub struct Identifier {
    pub token: token::Token, // token.IDENT token
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl ToString for Identifier {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl Expression for Identifier {}
