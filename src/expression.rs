use crate::error::Error;
use crate::statement::DefinitionType;
use crate::tokenize::{Token, TokenReader};

use std::collections::HashMap;

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

    Negate(Box<Expression>),

    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),

    FunctionCall(String, Box<[Expression]>),
}

const ADD_PRIORITY: u32 = 0;
const SUB_PRIORITY: u32 = 0;
const MUL_PRIORITY: u32 = 1;
const DIV_PRIORITY: u32 = 2;
const NEG_PRIORITY: u32 = 3;

#[derive(Clone, Copy, Debug)]
enum ExpressionContext {
    Ordered(u32),
    Parenthesis,
    FunctionArg,
}

pub fn parse_expression<'a>(
    tokens: &mut TokenReader<'_, 'a>,
    definition_map: &HashMap<String, DefinitionType>,
) -> Result<Expression, Error<'a>> {
    parse_expression_recursive(tokens, definition_map, vec![])
}

fn parse_expression_recursive<'a>(
    tokens: &mut TokenReader<'_, 'a>,
    definition_map: &HashMap<String, DefinitionType>,
    mut context: Vec<ExpressionContext>,
) -> Result<Expression, Error<'a>> {
    let mut cur_expression = Expression::Placeholder;

    while !tokens.is_at_end() {
        let next = tokens.next();
        match next {
            Token::Word(name) => {
                if !matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                }

                cur_expression = Expression::Value(Value::Variable(name.to_string()));
            }
            Token::WordAfterNumeric(name) => match cur_expression {
                Expression::Value(Value::Integer(other)) => {
                    let lhs = Expression::Value(Value::Integer(other));
                    let rhs = Expression::Value(Value::Variable(name.to_string()));

                    cur_expression = Expression::Multiply(Box::new(lhs), Box::new(rhs));
                }
                Expression::Value(Value::Number(other)) => {
                    let lhs = Expression::Value(Value::Number(other));
                    let rhs = Expression::Value(Value::Variable(name.to_string()));

                    cur_expression = Expression::Multiply(Box::new(lhs), Box::new(rhs));
                }
                _ => unreachable!(),
            },
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

            Token::Comma => {
                if matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                } else {
                    let Some(topmost) = context.pop() else {
                        return Err(Error::UnexpectedToken(next));
                    };

                    if matches!(topmost, ExpressionContext::FunctionArg) {
                        // Backtrack to avoid comma, caller will handle it
                        tokens.backtrack(1);
                        return Ok(cur_expression);
                    }

                    for ctx in context.iter().rev() {
                        if matches!(ctx, ExpressionContext::FunctionArg) {
                            // Backtrack to avoid comma, caller will handle it
                            tokens.backtrack(1);
                            return Ok(cur_expression);
                        }
                    }

                    return Err(Error::UnexpectedToken(next));
                }
            }

            Token::ParenOpen => {
                match cur_expression {
                    Expression::Placeholder => {
                        let mut new_context = context.clone();
                        new_context.push(ExpressionContext::Parenthesis);

                        let obj = parse_expression_recursive(tokens, definition_map, new_context)?;
                        cur_expression = obj;
                    }
                    Expression::Value(Value::Variable(ref name)) => {
                        // Depending on the definition of `name`, this is an
                        // implicit multiplication or a function call
                        let def = definition_map.get(name);
                        if let Some(DefinitionType::Function(num_args)) = def {
                            assert!(*num_args != 0);

                            let mut args = Vec::with_capacity(*num_args);
                            for _ in 0..num_args - 1 {
                                let mut new_context = context.clone();
                                new_context.push(ExpressionContext::FunctionArg);

                                let arg_expr = parse_expression_recursive(
                                    tokens,
                                    definition_map,
                                    new_context,
                                )?;
                                args.push(arg_expr);

                                match tokens.next() {
                                    Token::Comma => {}
                                    other => return Err(Error::UnexpectedToken(other)),
                                }
                            }

                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::Parenthesis);

                            let last_arg =
                                parse_expression_recursive(tokens, definition_map, new_context)?;
                            args.push(last_arg);

                            // Last arg has a parenthesis instead of a comma after it

                            cur_expression =
                                Expression::FunctionCall(name.clone(), args.into_boxed_slice());
                        } else {
                            // Implicit multiplication
                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::Parenthesis);

                            let rhs =
                                parse_expression_recursive(tokens, definition_map, new_context)?;
                            cur_expression =
                                Expression::Multiply(Box::new(cur_expression), Box::new(rhs));
                        }
                    }
                    _ => {
                        // This is an implicit multiplication, such as one of the
                        // form 3(x + 1) or x(x + 1)
                        let mut new_context = context.clone();
                        new_context.push(ExpressionContext::Parenthesis);

                        let rhs = parse_expression_recursive(tokens, definition_map, new_context)?;
                        cur_expression =
                            Expression::Multiply(Box::new(cur_expression), Box::new(rhs));
                    }
                }
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

                let rhs = parse_expression_recursive(tokens, definition_map, new_context)?;

                cur_expression = Expression::Add(Box::new(cur_expression), Box::new(rhs));
            }

            Token::Minus => {
                if matches!(cur_expression, Expression::Placeholder) {
                    // Handle negation, such as -3 or -x or -(y + 7)
                    let mut new_context = context.clone();
                    new_context.push(ExpressionContext::Ordered(NEG_PRIORITY));

                    let expr = parse_expression_recursive(tokens, definition_map, new_context)?;
                    cur_expression = Expression::Negate(Box::new(expr));
                    continue;
                } else if let Some(ExpressionContext::Ordered(o)) = context.last() {
                    // handle a * b - c: this will force the (a * b) to be its
                    // own expression
                    if *o > SUB_PRIORITY {
                        tokens.backtrack(1);
                        return Ok(cur_expression);
                    }
                }

                let mut new_context = context.clone();
                new_context.push(ExpressionContext::Ordered(SUB_PRIORITY));

                let rhs = parse_expression_recursive(tokens, definition_map, new_context)?;

                cur_expression = Expression::Subtract(Box::new(cur_expression), Box::new(rhs));
            }

            Token::Asterisk => {
                if matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                } else if let Some(ExpressionContext::Ordered(o)) = context.last() {
                    // handle -a * b: this will force the (-a) to be its
                    // own expression
                    if *o > MUL_PRIORITY {
                        tokens.backtrack(1);
                        return Ok(cur_expression);
                    }
                }

                let mut new_context = context.clone();
                new_context.push(ExpressionContext::Ordered(MUL_PRIORITY));

                let rhs = parse_expression_recursive(tokens, definition_map, new_context)?;

                cur_expression = Expression::Multiply(Box::new(cur_expression), Box::new(rhs));
            }

            Token::Slash => {
                if matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                } else if let Some(ExpressionContext::Ordered(o)) = context.last() {
                    // handle -a / b: this will force the (-a) to be its
                    // own expression
                    if *o > DIV_PRIORITY {
                        tokens.backtrack(1);
                        return Ok(cur_expression);
                    }
                }

                let mut new_context = context.clone();
                new_context.push(ExpressionContext::Ordered(DIV_PRIORITY));

                let rhs = parse_expression_recursive(tokens, definition_map, new_context)?;

                cur_expression = Expression::Divide(Box::new(cur_expression), Box::new(rhs));
            }

            // unimplemented
            _ => unimplemented!("unimplemented token"),
        }
    }

    if matches!(cur_expression, Expression::Placeholder) {
        Err(Error::UnexpectedEOI)
    } else if matches!(
        context.last(),
        Some(ExpressionContext::Parenthesis | ExpressionContext::FunctionArg)
    ) {
        // We should never get here in a Parenthesis or FunctionArg context, since
        // encountering a ParenClose or Comma, respectively, will always return
        // with a result. So if we got here, it means that we had a Parenthesis
        // context that didn't end in a ParenClose or a FunctionArg context that
        // didn't end in a Comma.
        Err(Error::UnexpectedEOI)
    } else {
        Ok(cur_expression)
    }
}
