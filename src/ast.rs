use crate::token::{self, Token};

pub trait Node: ToString {
    fn token_literal(&self) -> &str;
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl ToString for BlockStatement {
    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|v| v.to_string())
            .collect::<String>()
    }
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
            LetStmt { name, value, .. } => {
                let mut out = String::from("let ");
                out.push_str(&name.to_string()); // identifier
                out.push_str(" = ");
                out.push_str(&value.to_string());
                out
            }
            ReturnStmt { value, .. } => {
                let mut out = String::from("return ");
                out.push_str(&value.to_string());
                out
            }
            ExpressionStmt { value, .. } => {
                println!("here bozo 3");
                value.to_string()
            }
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

#[derive(Clone)]
pub enum Expression {
    Empty,
    Identifier(Identifier),
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    Boolean {
        token: Token,
        value: bool,
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
    IfExpression {
        token: Token, // the 'if' token
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        token: Token,
        parameters: Vec<Identifier>, // TODO consider using Identifier directly
        body: BlockStatement,
    },
    CallExpression {
        token: Token,
        function: Box<Expression>,
        arguments: Vec<Expression>,
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
            Boolean { token, .. } => &token.literal,
            IfExpression { token, .. } => &token.literal,
            FunctionLiteral { token, .. } => &token.literal,
            CallExpression { token, .. } => &token.literal,
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
            Boolean { token, .. } => token.literal.clone(),
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
            } => {
                let parts = [
                    "(",
                    &left.to_string(),
                    " ",
                    operator,
                    " ",
                    &right.to_string(),
                    ")",
                ];
                parts.into_iter().collect::<String>()
            }
            IfExpression {
                condition,
                consequence,
                alternative,
                ..
            } => {
                let mut out = ["if", &condition.to_string(), " ", &consequence.to_string()]
                    .into_iter()
                    .collect::<String>();
                if let Some(alternative) = alternative {
                    out.push_str("else");
                    out.push_str(&alternative.to_string());
                }
                out
            }
            FunctionLiteral {
                parameters, body, ..
            } => {
                let parts = [
                    "fn",
                    "(",
                    &parameters
                        .into_iter()
                        .map(|v| v.to_string())
                        .intersperse(String::from(", "))
                        .collect::<String>(),
                    ")",
                    &body.to_string(),
                ];
                parts.into_iter().collect::<String>()
            }
            CallExpression {
                function,
                arguments,
                ..
            } => {
                let parts = [
                    &function.to_string(),
                    "(",
                    &arguments
                        .into_iter()
                        .map(|v| v.to_string())
                        .intersperse(String::from(", "))
                        .collect::<String>(),
                    ")",
                ];
                parts.into_iter().collect::<String>()
            }
        }
    }
}

#[derive(Clone)]
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
