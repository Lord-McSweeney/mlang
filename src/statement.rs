use crate::error::Error;
use crate::expression::{Expression, parse_expression};
use crate::tokenize::{Token, TokenReader};

#[derive(Debug)]
pub enum Statement {
    FunctionDef {
        name: String,
        args: Vec<String>,
        expr: Expression,
    },
    GlobalVariable {
        name: String,
        expr: Expression,
    },
}

pub fn parse_statements<'a>(tokens: &[Token<'a>]) -> Result<Vec<Statement>, Error<'a>> {
    let mut statements = Vec::new();
    let mut cur_start = 0;
    let mut cur_end = 0;

    for (i, token) in tokens.iter().enumerate() {
        if matches!(token, Token::Newline) {
            if cur_start != cur_end {
                let statement = parse_statement(&tokens[cur_start..cur_end])?;
                statements.push(statement);
            }

            cur_start = i + 1;
            cur_end = i + 1;
        } else {
            cur_end += 1;
        }
    }

    Ok(statements)
}

fn parse_statement<'a>(tokens: &[Token<'a>]) -> Result<Statement, Error<'a>> {
    let mut tokens = TokenReader::new(tokens);

    let first_token = tokens.next();
    let name = match first_token {
        Token::Word(word) => word.to_string(),
        other => return Err(Error::UnexpectedToken(other)),
    };

    let next_token = tokens.next();

    let statement = match next_token {
        Token::ParenOpen => {
            let mut args = Vec::new();

            loop {
                let next = tokens.next();
                match next {
                    Token::Word(name) => {
                        args.push(name.to_string());
                    }
                    other => return Err(Error::UnexpectedToken(other)),
                }

                let next = tokens.next();
                match next {
                    Token::Comma => {}
                    Token::ParenClose => break,
                    other => return Err(Error::UnexpectedToken(other)),
                }
            }

            match tokens.next() {
                Token::Equals => {}
                other => return Err(Error::UnexpectedToken(other)),
            }

            let expr = parse_expression(&mut tokens)?;

            Statement::FunctionDef { name, args, expr }
        }
        Token::Equals => {
            let expr = parse_expression(&mut tokens)?;

            Statement::GlobalVariable { name, expr }
        }
        other => return Err(Error::UnexpectedToken(other)),
    };

    Ok(statement)
}
