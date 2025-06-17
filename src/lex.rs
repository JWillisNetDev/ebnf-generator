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
// repetition    | { ... } None or More
// grouping      | ( ... )
// literal       | " ... "
// comment       | (* ... *)
// comment       | #

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

    fn eat_one_line_comment(&mut self) {
        while let Some(&c) = self.inner.peek() {
            if c == '\n' {
                self.inner.next(); // Consume the newline
                break;
            } else {
                self.inner.next();
            }
        }
        // Can sometimes eat until the end of the input. That's fine.
    }

    fn eat_delimited_comment(&mut self) -> LexResult<()> {
        // Eat until we find the closing delimiter
        while let Some(&c) = self.inner.peek() {
            if c == '*' {
                self.inner.next(); // Consume the '*'
                // Check the next character
                if self.inner.peek() == Some(&')') {
                    self.inner.next(); // Consume the ')'
                    return Ok(());
                }
            } else {
                self.inner.next();
            }
        }

        Err("Unterminated comment encountered at end of input".to_string())
    }

    fn read_until(&mut self, predicate: fn(char) -> bool) -> LexResult<String> {
        let mut buf = String::new();
        while let Some(&c) = self.inner.peek() {
            if predicate(c) {
                return Ok(buf);
            } else {
                self.inner.next(); // Consume the character
                buf.push(c);
            }
        }
        Err("Expected a predicate to be resolved, but reached end of input".to_string())
    }

    fn read_until_char(&mut self, delim: char) -> LexResult<String> {
        let mut buf = String::new();
        while let Some(&c) = self.inner.peek() {
            if c == delim {
                return Ok(buf);
            } else {
                self.inner.next(); // Consume the character
                buf.push(c);
            }
        }

        Err(format!("Expected '{}' but reached end of input", delim))
    }

    fn read_next_identifier(&mut self) -> NextTokenResult {
        // We can assume the next character is alphanumeric or an underscore
        // Read characters until we hit a character that is not valid in an identifier
        // Spaces are valid between parts of an identifier, but not at the start or end
        let ident = self
            .read_until(|c| !is_valid_identifier_delimiter(c))?
            .trim()
            .to_string();

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
        let literal = self.read_until_char(delim)?;

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
            Some(&'[') => {
                self.inner.next();
                Some(OpeningBracket)
            }
            Some(&']') => {
                self.inner.next();
                Some(ClosingBracket)
            }
            Some(&'{') => {
                self.inner.next();
                Some(OpeningBrace)
            }
            Some(&'}') => {
                self.inner.next();
                Some(ClosingBrace)
            }
            Some(&'(') => {
                // Can either be a grouping or a comment
                self.inner.next(); // Consume the '('
                if self.inner.peek() == Some(&'*') {
                    // This is a delimited comment
                    self.inner.next(); // Consume the '*'
                    if let Err(err) = self.eat_delimited_comment() {
                        Some(Error(err))
                    } else {
                        self.next() // Recurse to the next token after the comment
                    }
                } else {
                    Some(OpeningParen)
                }
            }
            Some(&')') => {
                self.inner.next();
                Some(ClosingParen)
            }
            Some(&'#') => {
                // This is a one-line comment
                self.inner.next(); // Consume the '#'
                self.eat_one_line_comment();
                self.next() // Recurse to the next token after the comment
            }
            Some(&'*') => {
                self.inner.next();
                Some(Repetition)
            }
            Some(&'-') => {
                self.inner.next();
                Some(Except)
            }
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

    #[test]
    pub fn it_eats_comments() {
        let input = r#"
        (* This is a comment *)
        boolean = "true" #bools can be true
            | "false" (* bools can be false
            | "lorem ipsum" # Bools cannot be lorem ipsum. *)
            | "maybe" # bools can be either
            | "both" # or sometimes even both
            ;"#;

        let lexer = Lexer::new(input.chars());
        let tokens: Vec<Token> = lexer.collect();

        use Token::*;
        assert_eq!(tokens.len(), 10);
        assert_eq!(
            [
                identifier!("boolean"),
                Assignment,
                literal!("true"),
                Separator,
                literal!("false"),
                Separator,
                literal!("maybe"),
                Separator,
                literal!("both"),
                Terminator,
            ],
            tokens.as_slice()
        );
    }

    #[test]
    pub fn it_lexes_all_symbols() {
        // Give it letter vomit.
        let input = r#"
        identifier "literal" 'another literal' * - = , | ; [ ] { } ( )
        "#;

        let lexer = Lexer::new(input.chars());
        let tokens: Vec<Token> = lexer.collect();

        use Token::*;
        assert_eq!(tokens.len(), 15);
        assert_eq!(
            [
                identifier!("identifier"),
                literal!("literal"),
                literal!("another literal"),
                Repetition,
                Except,
                Assignment,
                Concat,
                Separator,
                Terminator,
                OpeningBracket,
                ClosingBracket,
                OpeningBrace,
                ClosingBrace,
                OpeningParen,
                ClosingParen,
            ],
            tokens.as_slice()
        );
    }

    #[test]
    pub fn it_lexes_identifiers_with_spaces() {
        let input = "identifiers with spaces are valid   ;";
        let lexer = Lexer::new(input.chars());
        let tokens: Vec<Token> = lexer.collect();
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            [
                identifier!("identifiers with spaces are valid"),
                Token::Terminator
            ],
            tokens.as_slice()
        );
    }

    #[test]
    pub fn it_lexes_literals_with_troublesome_characters() {
        let input = r#"
            "here's a string with a single quote in it"
            'and here is a string with "a double quote" as they say'
        "#;
        let lexer = Lexer::new(input.chars());
        let tokens: Vec<Token> = lexer.collect();
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            [
                literal!("here's a string with a single quote in it"),
                literal!("and here is a string with \"a double quote\" as they say"),
            ],
            tokens.as_slice()
        );
    }
}
