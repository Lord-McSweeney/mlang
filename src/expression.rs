use crate::error::Error;
use crate::tokenize::{Token, TokenReader};

#[derive(Debug)]
pub enum Value {
    Integer(i32),
    Number(f64),
    Variable(String),
}

#[derive(Debug)]
pub enum Expression {
    // Placeholder used during AST construction, should never appear in
    // an actual constructed tree
    Placeholder,

    Value(Value),

    Add(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
}

const ADD_PRIORITY: u32 = 0;
const MULT_PRIORITY: u32 = 1;

#[derive(Clone, Copy, Debug)]
enum ExpressionContext {
    Ordered(u32),
    Parenthesis,
}

pub fn parse_expression<'a>(tokens: &mut TokenReader<'_, 'a>) -> Result<Expression, Error<'a>> {
    parse_expression_recursive(tokens, vec![])
}

fn parse_expression_recursive<'a>(
    tokens: &mut TokenReader<'_, 'a>,
    mut context: Vec<ExpressionContext>,
) -> Result<Expression, Error<'a>> {
    let mut cur_expression = Expression::Placeholder;

    while !tokens.is_at_end() {
        let next = tokens.next();
        match next {
            Token::Integer(value) => {
                if !matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                }

                cur_expression = Expression::Value(Value::Integer(value));
            }
            Token::Number(value) => {
                if !matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                }

                cur_expression = Expression::Value(Value::Number(value));
            }
            Token::Word(name) => {
                if !matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                }

                cur_expression = Expression::Value(Value::Variable(name.to_string()));
            }

            Token::ParenOpen => {
                if !matches!(cur_expression, Expression::Placeholder) {
                    unimplemented!("unimplemented implicit multiplication");
                }

                let mut new_context = context.clone();
                new_context.push(ExpressionContext::Parenthesis);

                let obj = parse_expression_recursive(tokens, new_context)?;
                cur_expression = obj;
            }

            Token::ParenClose => {
                if matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                } else {
                    let Some(topmost) = context.pop() else {
                        return Err(Error::UnexpectedToken(next));
                    };

                    if matches!(topmost, ExpressionContext::Parenthesis) {
                        return Ok(cur_expression);
                    }

                    for ctx in context.iter().rev() {
                        if matches!(ctx, ExpressionContext::Parenthesis) {
                            // If this parenthesis wasn't right after, let's backtrack by 1.
                            tokens.backtrack(1);
                            return Ok(cur_expression);
                        }
                    }

                    return Err(Error::UnexpectedToken(next));
                }
            }

            Token::Plus => {
                if matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                } else if let Some(ExpressionContext::Ordered(o)) = context.last() {
                    // handle a * b + c: this will force the (a * b) to be its
                    // own expression
                    if *o > ADD_PRIORITY {
                        tokens.backtrack(1);
                        return Ok(cur_expression);
                    }
                }

                let mut new_context = context.clone();
                new_context.push(ExpressionContext::Ordered(ADD_PRIORITY));

                let rhs = parse_expression_recursive(tokens, new_context)?;

                cur_expression = Expression::Add(Box::new(cur_expression), Box::new(rhs));
            }

            Token::Asterisk => {
                if matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                }

                let mut new_context = context.clone();
                new_context.push(ExpressionContext::Ordered(MULT_PRIORITY));

                let rhs = parse_expression_recursive(tokens, new_context)?;

                cur_expression = Expression::Multiply(Box::new(cur_expression), Box::new(rhs));
            }

            // unimplemented
            _ => unimplemented!("unimplemented token"),
        }
    }

    Ok(cur_expression)
}
