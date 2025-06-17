use std::iter::Peekable;

// Table of Symbols:
// Usage | Notation
// ---------------------------------
// definition    | =
// concatenation | ,
// termination   | ;
// alternation   | |
// exception     | -
// optional      | [ ... ] None or Once
// repitition    | { ... } None or More
// grouping      | ( ... )
// literal       | " ... "
// comment       | (* ... *)

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Literal(String),

    // Symbols
    Repetition, // *
    Except,     // -
    Assignment, // =
    Concat,     // ,
    Separator,  // |
    Terminator, // ;

    // Blocks
    OpeningBracket, // [
    ClosingBracket, // ]
    OpeningBrace,   // {
    ClosingBrace,   // }
    OpeningParen,   // (
    ClosingParen,   // )

    // Used to encapsulate errors in the lexer.
    // Wraps an informative message about the error.
    Error(String),
}

#[derive(Debug, Clone)]
pub struct Lexer<T: IntoIterator<Item = char>> {
    inner: Peekable<T::IntoIter>,
}

type LexError = String;
type LexResult<T> = Result<T, LexError>;
type NextTokenResult = Result<Token, LexError>;

fn is_valid_identifier_delimiter(c: char) -> bool {
    c.is_alphanumeric() || c == ' ' || c == '_'
}

fn is_valid_literal_delimiter(c: char) -> bool {
    c == '"' || c == '\''
}

impl<T: IntoIterator<Item = char>> Lexer<T> {
    pub fn new(iterable: T) -> Self {
        Lexer {
            inner: iterable.into_iter().peekable(),
        }
    }

    fn eat_whitespace(&mut self) {
        while let Some(&c) = self.inner.peek() {
            if c.is_whitespace() {
                self.inner.next();
            } else {
                break;
            }
        }
    }

    fn read_until(&mut self, delim: char) -> LexResult<String> {
        let mut buf = String::new();
        while let Some(&c) = self.inner.peek() {
            if c == delim {
                return Ok(buf);
            } else if is_valid_identifier_delimiter(c) {
                self.inner.next(); // Consume the character
                buf.push(c);
            }
        }

        Err(format!("Expected '{}' but reached end of input", delim))
    }

    fn read_next_identifier(&mut self) -> NextTokenResult {
        // We can assume the next character is alphanumeric or an underscore
        // Read valid characters until we hit an '='
        // Spaces are valid between parts of an identifier, but not at the start or end
        let ident = self.read_until('=')?.trim().to_string();

        // Identifiers can never be empty
        // If we get into this state, then that's really impressive
        if ident.is_empty() {
            Err("Expected identifier but found empty string".to_string())
        } else {
            Ok(Token::Identifier(ident))
        }
    }

    fn read_next_literal(&mut self, delim: char) -> NextTokenResult {
        // Read characters until we hit a closing quote
        // Unlike identifiers, literals may contain trailing or leading whitespace
        let literal = self.read_until(delim)?;

        // Empty is invalid for literals in EBNF
        // If we get into this state, then that's really impressive
        if literal.is_empty() {
            return Err("Expected string literal but found empty string".to_string());
        }
        self.inner.next();
        Ok(Token::Literal(literal))
    }
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // Eat any leading whitespace
        self.eat_whitespace();

        // Attempt to read the next token
        use Token::*;
        match self.inner.peek() {
            Some(&'=') => {
                self.inner.next();
                Some(Assignment)
            }
            Some(&'|') => {
                self.inner.next();
                Some(Separator)
            }
            Some(&',') => {
                self.inner.next();
                Some(Concat)
            }
            Some(&';') => {
                self.inner.next();
                Some(Terminator)
            }
            // String literals
            Some(&c) if is_valid_literal_delimiter(c) => {
                self.inner.next(); // Consume the opening quote
                let literal = self.read_next_literal(c).unwrap_or_else(Error);
                Some(literal)
            }
            // Identifiers
            Some(&c) if is_valid_identifier_delimiter(c) => {
                let ident = self.read_next_identifier().unwrap_or_else(Error);
                Some(ident)
            }
            None => None, // Reached end of input
            Some(c) => Some(Error(format!("Unexpected character: '{}'", c))), // Unhandled character
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! identifier {
        ($s:expr) => {
            crate::lex::Token::Identifier($s.to_string())
        };
    }

    macro_rules! literal {
        ($s:expr) => {
            crate::lex::Token::Literal($s.to_string())
        };
    }

    #[test]
    pub fn it_works() {
        let input = r#"
        boolean = "true" | "false";
        "#;

        let lexer = Lexer::new(input.chars());
        let tokens: Vec<Token> = lexer.collect();

        use Token::*;
        assert_eq!(tokens.len(), 6);
        assert_eq!(
            [
                identifier!("boolean"),
                Assignment,
                literal!("true"),
                Separator,
                literal!("false"),
                Terminator,
            ],
            tokens.as_slice()
        );
    }
}
