use crate::lex::Token;

pub struct Statement {
    pub identifier: String,
    pub expression: Expression,
}

pub enum Expression {
    // Identifiers are usually variable names
    Identifier(Token),

    // Literals are always strings
    Literal(Token),

    // Binary operations are things like Assignment, Or
    BinaryOperation {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
}
