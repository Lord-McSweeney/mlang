use crate::error::Error;
use crate::expression::{Expression, parse_expression};
use crate::tokenize::{Token, TokenReader};

use std::collections::HashMap;

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

#[derive(Clone, Copy, Debug)]
pub enum DefinitionType {
    Function(usize),
    Variable,
}

#[derive(Clone, Copy)]
pub struct DefinitionContext<'a> {
    definition_map: &'a HashMap<String, DefinitionType>,
    local_defs: Option<&'a HashMap<String, DefinitionType>>,
}

impl<'a> DefinitionContext<'a> {
    pub fn lookup(&self, name: &String) -> Option<DefinitionType> {
        self.local_defs
            .and_then(|d| d.get(name))
            .or_else(|| self.definition_map.get(name))
            .copied()
    }
}

pub fn parse_statements<'a>(tokens: &[Token<'a>]) -> Result<Vec<Statement>, Error<'a>> {
    let mut definition_map = HashMap::new();

    definition_map.insert("sin".to_string(), DefinitionType::Function(1));
    definition_map.insert("cos".to_string(), DefinitionType::Function(1));
    definition_map.insert("tan".to_string(), DefinitionType::Function(1));

    definition_map.insert("csc".to_string(), DefinitionType::Function(1));
    definition_map.insert("sec".to_string(), DefinitionType::Function(1));
    definition_map.insert("cot".to_string(), DefinitionType::Function(1));

    definition_map.insert("sqrt".to_string(), DefinitionType::Function(1));
    definition_map.insert("erf".to_string(), DefinitionType::Function(1));

    definition_map.insert("e".to_string(), DefinitionType::Variable);

    let mut partial_statements = Vec::new();
    let mut cur_start = 0;
    let mut cur_end = 0;

    for (i, token) in tokens.iter().enumerate() {
        if matches!(token, Token::Newline) {
            if cur_start != cur_end {
                let result = parse_statement(&tokens[cur_start..cur_end], &mut definition_map)?;
                partial_statements.push(result);
            }

            cur_start = i + 1;
            cur_end = i + 1;
        } else {
            cur_end += 1;
        }
    }

    // We need the `definition_map` created before creating ASTs for expressions

    partial_statements
        .into_iter()
        .map(|(mut statement, tokens)| -> Result<Statement, Error> {
            match &mut statement {
                Statement::FunctionDef { expr, args, .. } => {
                    // Arguments are defined variables in function bodies
                    let mut local_defs = HashMap::with_capacity(args.len());
                    for arg in args {
                        local_defs.insert(arg.clone(), DefinitionType::Variable);
                    }

                    let definitions = DefinitionContext {
                        definition_map: &definition_map,
                        local_defs: Some(&local_defs),
                    };

                    let mut tokens = TokenReader::new(tokens);
                    *expr = parse_expression(&mut tokens, definitions)?;
                }
                Statement::GlobalVariable { expr, .. } => {
                    let definitions = DefinitionContext {
                        definition_map: &definition_map,
                        local_defs: None,
                    };

                    let mut tokens = TokenReader::new(tokens);
                    *expr = parse_expression(&mut tokens, definitions)?;
                }
            }

            Ok(statement)
        })
        .collect::<Result<Vec<_>, Error>>()
}

fn parse_statement<'a, 'b>(
    raw_tokens: &'b [Token<'a>],
    definition_map: &mut HashMap<String, DefinitionType>,
) -> Result<(Statement, &'b [Token<'a>]), Error<'a>> {
    let mut tokens = TokenReader::new(raw_tokens);

    let first_token = tokens.next();
    let name = match first_token {
        Token::Word(word) => word.to_string(),
        other => return Err(Error::UnexpectedToken(other)),
    };

    let next_token = tokens.next();

    let result = match next_token {
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

            assert!(args.len() != 0);
            definition_map.insert(name.clone(), DefinitionType::Function(args.len()));

            (
                Statement::FunctionDef {
                    name,
                    args,
                    expr: Expression::Placeholder,
                },
                &raw_tokens[tokens.current_pos()..],
            )
        }
        Token::Equals => {
            definition_map.insert(name.clone(), DefinitionType::Variable);

            (
                Statement::GlobalVariable {
                    name,
                    expr: Expression::Placeholder,
                },
                &raw_tokens[tokens.current_pos()..],
            )
        }
        other => return Err(Error::UnexpectedToken(other)),
    };

    Ok(result)
}
