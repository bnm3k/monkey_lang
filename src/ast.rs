#[derive(Clone, Debug)]
pub enum Statement {
    LetStmt { name: String, value: Expression },
    ReturnStmt(Expression),
    ExpressionStmt(Expression),
}

#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl ToString for BlockStatement {
    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|v| v.to_string())
            .collect::<String>()
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        use Statement::*;
        match self {
            LetStmt { name, value } => {
                let mut out = String::from("let ");
                out.push_str(&name.to_string()); // identifier
                out.push_str(" = ");
                out.push_str(&value.to_string());
                out
            }
            ReturnStmt(expr) => {
                let mut out = String::from("return ");
                out.push_str(&expr.to_string());
                out
            }
            ExpressionStmt(expr) => expr.to_string(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
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

#[derive(Clone, Debug)]
pub enum Expression {
    Boolean(bool),
    IntegerLiteral(i64),
    StringLiteral(String),
    Identifier(String),
    ArrayLiteral(Vec<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
    PrefixExpression {
        operator: String,
        right: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    IndexExpression {
        left: Box<Expression>,
        index: Box<Expression>,
    },
    IfExpression {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        parameters: Vec<String>,
        body: BlockStatement,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        use Expression::*;
        match self {
            Identifier(v) => v.clone(),
            IntegerLiteral(v) => v.to_string(),
            StringLiteral(v) => format!("\"{}\"", v),
            Boolean(v) => {
                let s = if *v { "true" } else { "false" };
                s.into()
            }
            PrefixExpression { operator, right } => ["(", operator, &right.to_string(), ")"]
                .into_iter()
                .collect::<String>(),
            InfixExpression {
                left,
                operator,
                right,
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
            IndexExpression { left, index } => {
                let parts = ["(", &left.to_string(), "[", &index.to_string(), "])"];
                parts.into_iter().collect::<String>()
            }
            IfExpression {
                condition,
                consequence,
                alternative,
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
            FunctionLiteral { parameters, body } => {
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
            ArrayLiteral(elements) => {
                let parts = [
                    "[",
                    &elements
                        .iter()
                        .map(|v| v.to_string())
                        .intersperse(String::from(", "))
                        .collect::<String>(),
                    "]",
                ];
                parts.into_iter().collect::<String>()
            }
            HashLiteral(entries) => {
                let parts = [
                    "{",
                    &entries
                        .iter()
                        .map(|(k, v)| k.to_string() + ":" + &v.to_string())
                        .intersperse(String::from(", "))
                        .collect::<String>(),
                    "}",
                ];
                parts.into_iter().collect::<String>()
            }
        }
    }
}
