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
                result.push_str(args.iter().next_back().expect("args.len() != 0"));

                result.push_str("\\right)=");

                result.push_str(&emit_latex_expression(expr));
            }
            Statement::GlobalVariable { name, expr } => {
                result.push_str(&name);

                result.push_str("=");

                result.push_str(&emit_latex_expression(expr));
            }
        }

        result.push('\n');
    }

    result
}

fn emit_latex_expression(expression: &Expression) -> String {
    match expression {
        Expression::Value(Value::Integer(int)) => int.to_string(),
        Expression::Value(Value::Number(num)) => num.to_string(),
        Expression::Value(Value::Function(name, _)) => name.clone(),
        Expression::Value(Value::Variable(name)) => name.clone(),

        Expression::Negate(expr) => {
            let mut result = "-".to_string();
            result.push_str(&emit_latex_expression(&expr));

            result
        }

        Expression::Add(lhs, rhs) => {
            let mut result = emit_latex_expression(&lhs);
            result.push('+');
            result.push_str(&emit_latex_expression(&rhs));

            result
        }

        Expression::Subtract(lhs, rhs) => {
            let mut result = emit_latex_expression(&lhs);
            result.push('-');
            result.push_str(&emit_latex_expression(&rhs));

            result
        }

        Expression::Placeholder => unreachable!(),

        _ => todo!(),
    }
}
