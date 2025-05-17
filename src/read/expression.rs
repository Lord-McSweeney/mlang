use super::error::Error;
use super::statement::{DefinitionContext, DefinitionType};
use super::tokenize::{Token, TokenReader};

#[derive(Debug)]
pub enum Value {
    Integer(i32),
    Number(f64),
    Function(String, usize),
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
    Exp(Box<Expression>, Box<Expression>),

    FunctionCall(String, Box<[Expression]>),
}

const ADD_PRIORITY: u32 = 0;
const SUB_PRIORITY: u32 = 0;
const MUL_PRIORITY: u32 = 1;
const DIV_PRIORITY: u32 = 2;
const NEG_PRIORITY: u32 = 3;
const POW_PRIORITY: u32 = 4;

#[derive(Clone, Copy, Debug)]
enum ExpressionContext {
    Ordered(u32),
    Parenthesis,
    FunctionArg,
}

pub fn parse_expression<'a, 'b>(
    tokens: &mut TokenReader<'_, 'a>,
    definitions: DefinitionContext<'b>,
) -> Result<Expression, Error<'a>> {
    parse_expression_recursive(tokens, definitions, vec![])
}

fn parse_expression_recursive<'a, 'b>(
    tokens: &mut TokenReader<'_, 'a>,
    definitions: DefinitionContext<'b>,
    mut context: Vec<ExpressionContext>,
) -> Result<Expression, Error<'a>> {
    let mut cur_expression = Expression::Placeholder;

    macro_rules! forbid_func_in {
        ($value:expr) => {
            if let Expression::Value(Value::Function(name, _)) = $value {
                return Err(Error::UnexpectedFunction(name));
            }
        };
        ($value:expr, $value2:expr) => {
            if let Expression::Value(Value::Function(name, _)) = $value {
                return Err(Error::UnexpectedFunction(name));
            }

            if let Expression::Value(Value::Function(name, _)) = $value2 {
                return Err(Error::UnexpectedFunction(name));
            }
        };
    }

    while let Some(next) = tokens.next() {
        match next {
            Token::Word(name) => {
                if !matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                }

                let name = name.to_string();

                let expr = match definitions.lookup(&name) {
                    Some(DefinitionType::Function(num_args)) => {
                        Expression::Value(Value::Function(name.clone(), num_args))
                    }
                    Some(DefinitionType::Variable) => Expression::Value(Value::Variable(name)),
                    None => return Err(Error::ReferencedUnboundVariable(name)),
                };

                cur_expression = expr;
            }
            Token::WordAfterNumeric(name) => {
                let name = name.to_string();

                let lhs = match cur_expression {
                    cur_expression @ Expression::Value(Value::Integer(_)) => cur_expression,
                    cur_expression @ Expression::Value(Value::Number(_)) => cur_expression,
                    _ => unreachable!(),
                };

                let rhs = match definitions.lookup(&name) {
                    Some(DefinitionType::Function(num_args)) => {
                        assert!(num_args != 0);

                        // This is a function call after an implicit multiplication,
                        // such as 3f(x); right now we're at the `f`, so let's expect
                        // the next token to be an open parenthesis.
                        match tokens.next().ok_or(Error::UnexpectedEOI)? {
                            Token::ParenOpen => {}
                            other @ Token::Comma
                            | other @ Token::ParenClose
                            | other @ Token::Equals => {
                                return Err(Error::UnexpectedToken(other));
                            }
                            _ => return Err(Error::UnexpectedFunction(name)),
                        }

                        let mut args = Vec::with_capacity(num_args);
                        for _ in 0..num_args - 1 {
                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::FunctionArg);

                            let arg_expr =
                                parse_expression_recursive(tokens, definitions, new_context)?;

                            // We don't have first-class functions yet
                            forbid_func_in!(arg_expr);

                            args.push(arg_expr);

                            match tokens.next().ok_or(Error::UnexpectedEOI)? {
                                Token::Comma => {}
                                other => return Err(Error::UnexpectedToken(other)),
                            }
                        }

                        let mut new_context = context.clone();
                        // Last arg has a parenthesis instead of a comma after it
                        new_context.push(ExpressionContext::Parenthesis);

                        let last_arg =
                            parse_expression_recursive(tokens, definitions, new_context)?;

                        forbid_func_in!(last_arg);

                        args.push(last_arg);

                        let result =
                            Expression::FunctionCall(name.clone(), args.into_boxed_slice());

                        if matches!(tokens.peek(), Some(Token::Caret)) {
                            let _ = tokens.next();

                            // Parse the value after the caret...
                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::Ordered(POW_PRIORITY));

                            let rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                            // 3 ^ f isn't legal
                            forbid_func_in!(rhs);

                            Expression::Exp(Box::new(result), Box::new(rhs))
                        } else {
                            result
                        }
                    }
                    Some(DefinitionType::Variable) => {
                        let result = Expression::Value(Value::Variable(name));

                        if matches!(tokens.peek(), Some(Token::Caret)) {
                            let _ = tokens.next();

                            // Parse the value after the caret...
                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::Ordered(POW_PRIORITY));

                            let rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                            // f ^ 3 isn't legal...?
                            forbid_func_in!(rhs);

                            Expression::Exp(Box::new(result), Box::new(rhs))
                        } else {
                            result
                        }
                    }
                    None => return Err(Error::ReferencedUnboundVariable(name)),
                };

                cur_expression = Expression::Multiply(Box::new(lhs), Box::new(rhs));
            }
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

                        let obj = parse_expression_recursive(tokens, definitions, new_context)?;
                        cur_expression = obj;
                    }
                    Expression::Value(Value::Function(ref name, num_args)) => {
                        assert!(num_args != 0);

                        let mut args = Vec::with_capacity(num_args);
                        for _ in 0..num_args - 1 {
                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::FunctionArg);

                            let arg_expr =
                                parse_expression_recursive(tokens, definitions, new_context)?;

                            // We don't have first-class functions yet
                            forbid_func_in!(arg_expr);

                            args.push(arg_expr);

                            match tokens.next().ok_or(Error::UnexpectedEOI)? {
                                Token::Comma => {}
                                other => return Err(Error::UnexpectedToken(other)),
                            }
                        }

                        let mut new_context = context.clone();
                        // Last arg has a parenthesis instead of a comma after it
                        new_context.push(ExpressionContext::Parenthesis);

                        let last_arg =
                            parse_expression_recursive(tokens, definitions, new_context)?;

                        forbid_func_in!(last_arg);

                        args.push(last_arg);

                        cur_expression =
                            Expression::FunctionCall(name.clone(), args.into_boxed_slice());
                    }
                    Expression::Value(Value::Variable(_)) => {
                        // Implicit multiplication
                        let mut new_context = context.clone();
                        new_context.push(ExpressionContext::Parenthesis);

                        let mul_rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                        forbid_func_in!(mul_rhs);

                        // Parse something like y(x + 2)^5, which is a special case
                        // because implicit multiplication is too special to work
                        // with the regular regular "push a context and recursively
                        // parse the next value" approach
                        let result = if matches!(tokens.peek(), Some(Token::Caret)) {
                            let _ = tokens.next();

                            // Parse the value after the caret...
                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::Ordered(POW_PRIORITY));

                            let pow_rhs =
                                parse_expression_recursive(tokens, definitions, new_context)?;

                            // 3 ^ f isn't legal
                            forbid_func_in!(pow_rhs);

                            Expression::Exp(Box::new(mul_rhs), Box::new(pow_rhs))
                        } else {
                            // Just use the expression if it wasn't the special-case
                            mul_rhs
                        };

                        cur_expression =
                            Expression::Multiply(Box::new(cur_expression), Box::new(result));
                    }
                    _ => {
                        // This is an implicit multiplication, such as one of the
                        // form 3(x + 1) or x(x + 1)
                        let mut new_context = context.clone();
                        new_context.push(ExpressionContext::Parenthesis);

                        let mul_rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                        forbid_func_in!(mul_rhs);

                        // Parse something like 3(x + 2)^5, which is a special case
                        // because implicit multiplication is too special to work
                        // with the regular regular "push a context and recursively
                        // parse the next value" approach
                        let result = if matches!(tokens.peek(), Some(Token::Caret)) {
                            let _ = tokens.next();

                            // Parse the value after the caret...
                            let mut new_context = context.clone();
                            new_context.push(ExpressionContext::Ordered(POW_PRIORITY));

                            let pow_rhs =
                                parse_expression_recursive(tokens, definitions, new_context)?;

                            // 3 ^ f isn't legal
                            forbid_func_in!(pow_rhs);

                            Expression::Exp(Box::new(mul_rhs), Box::new(pow_rhs))
                        } else {
                            // Just use the expression if it wasn't the special-case
                            mul_rhs
                        };

                        cur_expression =
                            Expression::Multiply(Box::new(cur_expression), Box::new(result));
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

                let rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                // f + 3 isn't legal
                forbid_func_in!(cur_expression, rhs);

                cur_expression = Expression::Add(Box::new(cur_expression), Box::new(rhs));
            }

            Token::Minus => {
                if matches!(cur_expression, Expression::Placeholder) {
                    // Handle negation, such as -3 or -x or -(y + 7)
                    let mut new_context = context.clone();
                    new_context.push(ExpressionContext::Ordered(NEG_PRIORITY));

                    let expr = parse_expression_recursive(tokens, definitions, new_context)?;
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

                let rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                // f - 3 isn't legal
                forbid_func_in!(cur_expression, rhs);

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

                let rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                // f * 3 isn't legal
                forbid_func_in!(cur_expression, rhs);

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

                let rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                // f / 3 isn't legal
                forbid_func_in!(cur_expression, rhs);

                cur_expression = Expression::Divide(Box::new(cur_expression), Box::new(rhs));
            }

            Token::Caret => {
                if matches!(cur_expression, Expression::Placeholder) {
                    return Err(Error::UnexpectedToken(next));
                }

                // Nothing is greater than POW priority, so we don't check here

                let mut new_context = context.clone();
                new_context.push(ExpressionContext::Ordered(POW_PRIORITY));

                let rhs = parse_expression_recursive(tokens, definitions, new_context)?;

                // f ^ 3 isn't legal...?
                forbid_func_in!(cur_expression, rhs);

                cur_expression = Expression::Exp(Box::new(cur_expression), Box::new(rhs));
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
