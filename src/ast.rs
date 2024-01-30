use crate::token::{self, Token};

pub trait Node: ToString {
    fn token_literal(&self) -> &str;
}

pub enum Statement {
    LetStmt {
        token: Token, // let token
        name: Identifier,
        value: Expression,
    },
    ReturnStmt {
        token: Token, // return token
        value: Expression,
    },
    ExpressionStmt {
        token: Token, // first token of the expression
        value: Expression,
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

pub enum Expression {
    Empty,
    Identifier(Identifier),
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    PrefixExpression {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    InfixExpression {
        token: Token,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        use Expression::*;
        match self {
            Empty => "empty",
            Identifier(i) => i.token_literal(),
            IntegerLiteral { token, .. } => &token.literal,
            PrefixExpression { token, .. } => &token.literal,
            InfixExpression { token, .. } => &token.literal,
        }
    }
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        use Expression::*;
        match self {
            Empty => "empty".into(),
            Identifier(i) => i.to_string(),
            IntegerLiteral { token, .. } => token.literal.clone(),
            PrefixExpression {
                operator, right, ..
            } => ["(", operator, &right.to_string(), ")"]
                .into_iter()
                .collect::<String>(),
            InfixExpression {
                left,
                operator,
                right,
                ..
            } => [
                "(",
                &left.to_string(),
                " ",
                operator,
                " ",
                &right.to_string(),
                ")",
            ]
            .into_iter()
            .collect::<String>(),
        }
    }
}

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
