use crate::read::expression::{Expression, Value};
use crate::read::statement::Statement;

pub fn emit_latex(statements: &[Statement]) -> String {
    let mut result = String::new();

    for statement in statements {
        match statement {
            Statement::FunctionDef { name, args, expr } => {
                result.push_str(&name);
                result.push_str("\\left(");

                for arg in args.iter().rev().skip(1).rev() {
                    result.push_str(arg);
                    result.push_str(", ");
                }
                result.push_str(args.last().expect("args.len() != 0"));

                result.push_str("\\right)=");

                result.push_str(&emit_latex_expression(expr, 0));
            }
            Statement::GlobalVariable { name, expr } => {
                result.push_str(&name);

                result.push_str("=");

                result.push_str(&emit_latex_expression(expr, 0));
            }
        }

        result.push('\n');
    }

    result
}

const ADD_PRIORITY: u32 = 0;
const SUB_PRIORITY: u32 = 0;
const MUL_PRIORITY: u32 = 1;
const DIV_PRIORITY: u32 = 2;
const NEG_PRIORITY: u32 = 3;
const POW_PRIORITY: u32 = 4;

fn emit_latex_expression(expression: &Expression, last_priority: u32) -> String {
    match expression {
        Expression::Placeholder => unreachable!(),

        Expression::Value(Value::Integer(int)) => int.to_string(),
        Expression::Value(Value::Number(num)) => num.to_string(),
        Expression::Value(Value::Function(name, _)) => name.clone(),
        Expression::Value(Value::Variable(name)) => name.clone(),

        Expression::Negate(expr) => {
            let mut result = String::new();
            if last_priority > NEG_PRIORITY {
                result.push_str("\\left(");
            }

            result.push('-');
            result.push_str(&emit_latex_expression(&expr, NEG_PRIORITY));

            if last_priority > NEG_PRIORITY {
                result.push_str("\\right)");
            }

            result
        }

        Expression::Add(lhs, rhs) => {
            let mut result = String::new();
            if last_priority > ADD_PRIORITY {
                result.push_str("\\left(");
            }

            result.push_str(&emit_latex_expression(&lhs, ADD_PRIORITY));
            result.push('+');
            result.push_str(&emit_latex_expression(&rhs, ADD_PRIORITY));

            if last_priority > ADD_PRIORITY {
                result.push_str("\\right)");
            }

            result
        }

        Expression::Subtract(lhs, rhs) => {
            let mut result = String::new();
            if last_priority > SUB_PRIORITY {
                result.push_str("\\left(");
            }

            result.push_str(&emit_latex_expression(&lhs, SUB_PRIORITY));
            result.push('-');
            result.push_str(&emit_latex_expression(&rhs, SUB_PRIORITY));

            if last_priority > SUB_PRIORITY {
                result.push_str("\\right)");
            }

            result
        }

        Expression::Multiply(lhs, rhs) => {
            let mut result = String::new();
            if last_priority > MUL_PRIORITY {
                result.push_str("\\left(");
            }

            result.push_str(&emit_latex_expression(&lhs, MUL_PRIORITY));
            result.push_str(" \\cdot ");
            result.push_str(&emit_latex_expression(&rhs, MUL_PRIORITY));

            if last_priority > MUL_PRIORITY {
                result.push_str("\\right)");
            }

            result
        }

        Expression::Divide(lhs, rhs) => {
            let mut result = String::new();
            if last_priority > DIV_PRIORITY {
                result.push_str("\\left(");
            }

            result.push_str("\\frac{");
            result.push_str(&emit_latex_expression(&lhs, DIV_PRIORITY));
            result.push_str("}{");
            result.push_str(&emit_latex_expression(&rhs, DIV_PRIORITY));
            result.push('}');

            if last_priority > DIV_PRIORITY {
                result.push_str("\\right)");
            }

            result
        }

        Expression::Exp(lhs, rhs) => {
            let mut result = String::new();
            if last_priority > POW_PRIORITY {
                result.push_str("\\left(");
            }

            result.push_str(&emit_latex_expression(&lhs, POW_PRIORITY));
            result.push_str("^{");
            result.push_str(&emit_latex_expression(&rhs, POW_PRIORITY));
            result.push('}');

            if last_priority > POW_PRIORITY {
                result.push_str("\\right)");
            }

            result
        }

        Expression::FunctionCall(name, args) => {
            let mut result = String::new();

            if name.len() != 1 {
                if name == "sqrt" {
                    assert!(args.len() == 1);

                    result.push_str("\\sqrt{");
                    result.push_str(&emit_latex_expression(&args[0], 0));
                    result.push('}');
                } else {
                    todo!()
                }
            } else {
                result.push_str(&name);
                result.push_str("\\left(");

                for arg in args.iter().rev().skip(1).rev() {
                    result.push_str(&emit_latex_expression(&arg, 0));
                    result.push_str(", ");
                }

                let last_arg = args.last().expect("args.len() != 0");
                result.push_str(&emit_latex_expression(&last_arg, 0));

                result.push_str("\\right)");
            }

            result
        }
    }
}
