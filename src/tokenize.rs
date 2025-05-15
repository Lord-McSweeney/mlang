use crate::error::Error;

#[derive(Clone, Copy, Debug)]
pub enum Token<'a> {
    Word(&'a str),
    WordAfterNumeric(&'a str),
    Integer(i32),
    Number(f64),

    Newline,

    Comma,

    ParenOpen,
    ParenClose,

    Equals,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Caret,
}

#[derive(Clone, Copy)]
enum ParseState {
    Word { start: usize, end: usize },
    WordAfterNumeric { start: usize, end: usize },
    Integer { value: i32 },
    Number { value: f64 },
    NumberWithDot { value: f64, trailing_digits: u32 },
    None,
}

impl ParseState {
    const MAX_INTEGER: i32 = 10000000;

    fn finish<'a>(&mut self, data: &'a str) -> Option<Token<'a>> {
        let token = match self {
            ParseState::Word { start, end } => {
                let cur_text = &data[*start..=*end];
                Some(Token::Word(cur_text))
            }
            ParseState::WordAfterNumeric { start, end } => {
                let cur_text = &data[*start..=*end];
                Some(Token::WordAfterNumeric(cur_text))
            }
            ParseState::Integer { value } => Some(Token::Integer(*value)),
            ParseState::Number { value } | ParseState::NumberWithDot { value, .. } => {
                Some(Token::Number(*value))
            }
            ParseState::None => None,
        };

        // Reset parse state
        *self = ParseState::None;

        token
    }
}

pub fn tokenize<'a>(data: &'a str) -> Result<Vec<Token<'a>>, Error<'a>> {
    let mut tokens = Vec::new();
    let mut parse_state = ParseState::None;

    for (i, ch) in data.chars().enumerate() {
        let ch = ch as u8;
        match ch {
            b'A'..=b'Z' | b'a'..=b'z' => match &mut parse_state {
                ParseState::Word { end, .. } | ParseState::WordAfterNumeric { end, .. } => {
                    *end += 1;
                }
                ParseState::Integer { value } => {
                    // This is an implicit multiplication, such as of the form 3x

                    tokens.push(Token::Integer(*value));
                    parse_state = ParseState::WordAfterNumeric { start: i, end: i };
                }
                ParseState::Number { value } => {
                    // This is an implicit multiplication, such as of the form 100000000x

                    tokens.push(Token::Number(*value));
                    parse_state = ParseState::WordAfterNumeric { start: i, end: i };
                }
                ParseState::NumberWithDot { .. } => {
                    // FIXME should 3.4x be valid syntax?
                    return Err(Error::UnexpectedCharacter(ch as char));
                }
                ParseState::None => {
                    parse_state = ParseState::Word { start: i, end: i };
                }
            },
            b'0'..=b'9' => {
                match &mut parse_state {
                    ParseState::Word { .. } | ParseState::WordAfterNumeric { .. } => {
                        return Err(Error::UnexpectedCharacter(ch as char));
                    }
                    ParseState::Integer { value } => {
                        let this_digit = ch - b'0';
                        *value = *value * 10 + this_digit as i32;

                        // Make Integer Number if it's close to overflowing
                        if *value > ParseState::MAX_INTEGER {
                            parse_state = ParseState::Number {
                                value: *value as f64,
                            };
                        }
                    }
                    ParseState::Number { value } => {
                        let this_digit = ch - b'0';
                        *value = *value * 10.0 + (this_digit as f64);
                    }
                    ParseState::NumberWithDot {
                        value,
                        trailing_digits,
                    } => {
                        *trailing_digits += 1;

                        let this_digit = ch - b'0';
                        let factor = 10_i64.pow(*trailing_digits as u32);
                        let digit_value = (this_digit as f64) / (factor as f64);
                        *value += digit_value;
                    }
                    ParseState::None => {
                        let this_digit = ch - b'0';

                        parse_state = ParseState::Integer {
                            value: this_digit as i32,
                        };
                    }
                }
            }
            b'.' => match &mut parse_state {
                ParseState::Word { .. } | ParseState::WordAfterNumeric { .. } => {
                    return Err(Error::UnexpectedCharacter(ch as char));
                }
                ParseState::Integer { value } => {
                    parse_state = ParseState::NumberWithDot {
                        value: *value as f64,
                        trailing_digits: 0,
                    };
                }
                ParseState::Number { value } => {
                    parse_state = ParseState::NumberWithDot {
                        value: *value,
                        trailing_digits: 0,
                    };
                }
                ParseState::NumberWithDot { .. } => {
                    return Err(Error::UnexpectedCharacter(ch as char));
                }
                ParseState::None => {
                    parse_state = ParseState::NumberWithDot {
                        value: 0.0,
                        trailing_digits: 0,
                    };
                }
            },
            b' ' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }
            }
            b'\n' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Newline);
            }
            b',' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Comma);
            }
            b'(' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::ParenOpen);
            }
            b')' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::ParenClose);
            }
            b'=' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Equals);
            }
            b'+' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Plus);
            }
            b'-' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Minus);
            }
            b'*' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Asterisk);
            }
            b'/' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Slash);
            }
            b'^' => {
                let token = parse_state.finish(data);
                if let Some(token) = token {
                    tokens.push(token);
                }

                tokens.push(Token::Caret);
            }
            _ => return Err(Error::UnexpectedCharacter(ch as char)),
        }
    }

    let last_token = parse_state.finish(data);
    if let Some(last_token) = last_token {
        tokens.push(last_token);
    }

    Ok(tokens)
}

#[derive(Clone, Copy)]
pub struct TokenReader<'a, 'b> {
    slice: &'a [Token<'b>],
    start: usize,
}

impl<'a, 'b> TokenReader<'a, 'b> {
    pub fn new(slice: &'a [Token<'b>]) -> Self {
        Self { slice, start: 0 }
    }

    pub fn next(&mut self) -> Token<'b> {
        let value = self.slice[self.start];
        self.start += 1;
        value
    }

    pub fn backtrack(&mut self, num_back: usize) {
        assert!(num_back <= self.start);
        self.start -= num_back;
    }

    pub fn is_at_end(&self) -> bool {
        self.start == self.slice.len()
    }
}
