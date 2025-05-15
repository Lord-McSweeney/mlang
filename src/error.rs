use crate::tokenize::Token;

#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedCharacter(char),
    UnexpectedToken(Token<'a>),
    UnexpectedEOI,
}
