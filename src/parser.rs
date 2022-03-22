use nom::{
    self,
    bytes::{self, complete::tag},
    character::complete::digit1,
    combinator::{map_res, recognize},
};

use crate::ast::{self, LetKind, List};

pub fn parser(input: &str) -> nom::IResult<&str, ast::Program> {
    nom::multi::many0(defn)(input)
}

fn defn(input: &str) -> nom::IResult<&str, ast::Defn> {
    // Parse all spaces
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, def) = nom::branch::alt((def_fun, def_val, def_exp))(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;

    Ok((input, def))
}

fn def_exp(input: &str) -> nom::IResult<&str, ast::Defn> {
    let (input, expr) = expr(input)?;
    Ok((input, ast::Defn::Expr(expr)))
}

fn def_val(input: &str) -> nom::IResult<&str, ast::Defn> {
    let (input, _) = open_paren(input)?;
    let (input, _) = tag("val")(input)?;
    let (input, _) = nom::character::complete::multispace1(input)?;
    let (input, name) = pure_str(input)?;
    let (input, exp) = spaced_expr(input)?;
    let (input, _) = close_paren(input)?;
    Ok((input, ast::Defn::Val(name, exp)))
}

fn def_fun(input: &str) -> nom::IResult<&str, ast::Defn> {
    let (input, _) = open_paren(input)?;
    let (input, _) = tag("define")(input)?;
    let (input, _) = nom::character::complete::multispace1(input)?;
    let (input, name) = pure_str(input)?;
    let (input, _) = open_paren(input)?;
    let (input, formals) =
        nom::multi::separated_list0(nom::character::complete::multispace1, pure_str)(input)?;
    let (input, _) = close_paren(input)?;
    let (input, exp) = spaced_expr(input)?;
    let (input, _) = close_paren(input)?;
    Ok((input, ast::Defn::Define(name, formals, exp)))
}

pub fn expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, expr) = nom::branch::alt((
        literal_expr,
        prim,
        var,
        set_expr,
        if_expr,
        lambda_expr,
        let_expr,
        begin_expr,
        apply_expr,
    ))(input)?;
    Ok((input, expr))
}

fn spaced_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, expr) = expr(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    Ok((input, expr))
}

fn literal_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, value) = nom::branch::alt((int, bool, quote))(input)?;
    Ok((input, ast::Expr::Literal(value)))
}

fn prim(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, s) = recognize(nom::branch::alt((
        tag("+"),
        tag("-"),
        tag("/"),
        tag("*"),
        tag("="),
        tag("car"),
        tag("cdr"),
        tag("null?"),
        tag("cons"),
        tag("&&"),
        tag("||"),
    )))(input)?;
    Ok((
        input,
        match s {
            "+" => ast::Expr::Primitive(ast::Prim::Add),
            "-" => ast::Expr::Primitive(ast::Prim::Sub),
            "/" => ast::Expr::Primitive(ast::Prim::Div),
            "*" => ast::Expr::Primitive(ast::Prim::Mul),
            "=" => ast::Expr::Primitive(ast::Prim::Eq),
            "car" => ast::Expr::Primitive(ast::Prim::Car),
            "cdr" => ast::Expr::Primitive(ast::Prim::Cdr),
            "null?" => ast::Expr::Primitive(ast::Prim::IsNull),
            "cons" => ast::Expr::Primitive(ast::Prim::Cons),
            "&&" => ast::Expr::Primitive(ast::Prim::ShortAnd),
            "||" => ast::Expr::Primitive(ast::Prim::ShortOr),
            _ => panic!("impossible"),
        },
    ))
}

fn var(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, value) = str(input)?;
    match value {
        ast::Literal::Str(s) => Ok((input, ast::Expr::Var(s))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

fn set_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, _) = open_paren(input)?;
    let (input, _) = nom::bytes::complete::tag("set")(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, value1) = nom::character::complete::alpha1(input)?;
    let (input, value2) = nom::character::complete::alphanumeric0(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, exp) = expr(input)?;
    let (input, _) = close_paren(input)?;
    Ok((
        input,
        ast::Expr::Set(value1.to_string() + &value2.to_string(), Box::new(exp)),
    ))
}

fn if_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, _) = open_paren(input)?;
    let (input, _) = nom::bytes::complete::tag("if")(input)?;
    let (input, expr1) = spaced_expr(input)?;
    let (input, expr2) = spaced_expr(input)?;
    let (input, expr3) = spaced_expr(input)?;
    let (input, _) = close_paren(input)?;
    Ok((
        input,
        ast::Expr::If(Box::new(expr1), Box::new(expr2), Box::new(expr3)),
    ))
}

fn apply_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, _) = open_paren(input)?;
    let (input, expr) = expr(input)?;
    let (input, expr_list) = nom::multi::many0(spaced_expr)(input)?;
    let (input, _) = close_paren(input)?;
    Ok((input, ast::Expr::Apply(Box::new(expr), expr_list)))
}

fn begin_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, _) = open_paren(input)?;
    let (input, _) = nom::bytes::complete::tag("begin")(input)?;
    let (input, expr_list) = nom::multi::many0(spaced_expr)(input)?;
    let (input, _) = close_paren(input)?;
    Ok((input, ast::Expr::Begin(expr_list)))
}

fn lambda_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, _) = open_paren(input)?;
    let (input, _) = nom::bytes::complete::tag("lambda")(input)?;
    let (input, _) = nom::character::complete::multispace1(input)?;
    let (input, _) = open_paren(input)?;
    let (input, formals) =
        nom::multi::separated_list0(nom::character::complete::multispace1, str)(input)?;
    let (input, _) = close_paren(input)?;
    let unwrapped_formals: Vec<String> = formals
        .into_iter()
        .map(|i| match i {
            ast::Literal::Str(s) => s,
            _ => panic!("Impossible"),
        })
        .collect();
    let (input, expr) = spaced_expr(input)?;
    let (input, _) = close_paren(input)?;
    Ok((
        input,
        ast::Expr::Lambda((unwrapped_formals, Box::new(expr))),
    ))
}

fn let_expr(input: &str) -> nom::IResult<&str, ast::Expr> {
    let (input, _) = open_paren(input)?;
    let (input, kind) = let_kind(input)?;
    let (input, _) = nom::character::complete::multispace1(input)?;
    let (input, bindings) = let_bindings(input)?;
    let (input, expr) = spaced_expr(input)?;
    Ok((input, ast::Expr::Let(kind, bindings, Box::new(expr))))
}

fn let_kind(input: &str) -> nom::IResult<&str, LetKind> {
    let (input, kind_str) =
        recognize(nom::branch::alt((tag("let*"), tag("letrec"), tag("let"))))(input)?;
    match kind_str {
        "let" => Ok((input, LetKind::Let)),
        "let*" => Ok((input, LetKind::LetStar)),
        "letrec" => Ok((input, LetKind::LetRec)),
        _ => panic!("impossible"),
    }
}

fn let_bindings(input: &str) -> nom::IResult<&str, Vec<(String, ast::Expr)>> {
    let (input, _) = open_paren(input)?;
    let (input, bindings) = nom::multi::many0(binding)(input)?;
    let (input, _) = close_paren(input)?;
    Ok((input, bindings))
}

fn binding(input: &str) -> nom::IResult<&str, (String, ast::Expr)> {
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, _) = nom::character::complete::char('[')(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, val) = pure_str(input)?;
    let (input, expr) = spaced_expr(input)?;
    let (input, _) = nom::character::complete::char(']')(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    Ok((input, (val, expr)))
}

fn literal(input: &str) -> nom::IResult<&str, ast::Literal> {
    nom::branch::alt((int, bool, str, cons))(input)
}

fn int(input: &str) -> nom::IResult<&str, ast::Literal> {
    let (input, result) = map_res(recognize(digit1), str::parse)(input)?;
    Ok((input, ast::Literal::Int(result)))
}

fn bool(input: &str) -> nom::IResult<&str, ast::Literal> {
    let (input, res) = nom::branch::alt((true_str, false_str))(input)?;
    Ok((input, ast::Literal::Bool(res)))
}

fn true_str(input: &str) -> nom::IResult<&str, bool> {
    let (input, _) = bytes::complete::tag("#t")(input)?;
    Ok((input, true))
}

fn false_str(input: &str) -> nom::IResult<&str, bool> {
    let (input, _) = bytes::complete::tag("#f")(input)?;
    Ok((input, false))
}

fn str(input: &str) -> nom::IResult<&str, ast::Literal> {
    let (input, string) = pure_str(input)?;
    Ok((input, ast::Literal::Str(string)))
}

fn pure_str(input: &str) -> nom::IResult<&str, String> {
    let (input, value1) = nom::character::complete::alpha1(input)?;
    let (input, value2) = nom::character::complete::alphanumeric0(input)?;
    Ok((input, value1.to_string() + &value2.to_string()))
}

fn cons(input: &str) -> nom::IResult<&str, ast::Literal> {
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, _) = nom::character::complete::char('(')(input)?;
    let (input, values) =
        nom::multi::separated_list0(nom::character::complete::space1, literal)(input)?;
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, _) = nom::character::complete::char(')')(input)?;

    let cons_list = values
        .into_iter()
        .rev()
        .fold(List::Nil, |acc, cur| List::Cons(cur, Box::new(acc)));

    Ok((input, ast::Literal::List(Box::new(cons_list))))
}

fn quote(input: &str) -> nom::IResult<&str, ast::Literal> {
    let (input, _) = nom::character::complete::char('\'')(input)?;
    nom::branch::alt((int, bool, str, cons))(input)
}

fn open_paren(input: &str) -> nom::IResult<&str, &str> {
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, _) = nom::character::complete::char('(')(input)?;
    nom::character::complete::multispace0(input)
}

fn close_paren(input: &str) -> nom::IResult<&str, &str> {
    let (input, _) = nom::character::complete::multispace0(input)?;
    let (input, _) = nom::character::complete::char(')')(input)?;
    nom::character::complete::multispace0(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nil() {
        let my_list_str1 = "()";
        let my_list_str2 = "(     )";
        match cons(my_list_str1) {
            Ok((_, l)) => println!("{:?}", l),
            Err(_) => panic!("Errored on {}", my_list_str1),
        }
        match cons(my_list_str2) {
            Ok((_, l)) => println!("{:?}", l),
            Err(_) => panic!("Errored on {}", my_list_str2),
        }
    }

    #[test]
    fn test_cons() {
        let my_list_str1 = "(1 2 3 4 5)";
        match cons(my_list_str1) {
            Ok((_, l)) => println!("{:?}", l),
            Err(_) => panic!("Errored on {}", my_list_str1),
        }
        let my_list_str2 = "(1 #t 4 5)";
        match cons(my_list_str2) {
            Ok((_, l)) => println!("{:?}", l),
            Err(_) => panic!("Errored on {}", my_list_str2),
        }
        let my_list_str3 = "(1 (1 2 3) (1 2 3))";
        match cons(my_list_str3) {
            Ok((_, l)) => println!("{:?}", l),
            Err(e) => panic!("Errored on {}", e),
        }
    }

    #[test]
    fn test_set() {
        let set_expr_str1 = "(set hello 'hello)";
        match expr(set_expr_str1) {
            Ok((_, e)) => match e {
                ast::Expr::Set(_, _) => (),
                _ => panic!("Expected {} to be a setexpr", set_expr_str1),
            },
            Err(_) => todo!(),
        }
        let set_expr_str2 = "(set hello (set x 3))";
        match expr(set_expr_str2) {
            Ok((_, e)) => match e {
                ast::Expr::Set(_, _) => (),
                _ => panic!("Expected {} to be a setexpr", set_expr_str2),
            },
            Err(_) => todo!(),
        }
    }

    #[test]
    fn test_apply() {
        let apply_expr_str1 = "(add3 1 2 3)";
        match expr(apply_expr_str1) {
            Ok((_, e)) => match e {
                ast::Expr::Apply(e, elist) => println!("Apply: {:?} {:?}", e, elist),
                _ => panic!("Expected {} to be a setexpr", apply_expr_str1),
            },
            Err(_) => todo!(),
        }

        let apply_expr_str2 = "((if #t plus sub) 1 2 3)";
        match expr(apply_expr_str2) {
            Ok((_, e)) => match e {
                ast::Expr::Apply(e, elist) => println!("Apply: {:?} {:?}", e, elist),
                _ => panic!("Expected {} to be a setexpr", apply_expr_str2),
            },
            Err(_) => todo!(),
        }
    }
    #[test]
    fn test_let() {
        let let_expr_1 = "(let ([x 3] [y 2]) x)";
        match expr(let_expr_1) {
            Ok((_, e)) => match e {
                ast::Expr::Let(kind, bindings, body) => {
                    println!("let: {:?} {:?} {:?}", kind, bindings, body)
                }
                _ => panic!("Expected {} to be a setexpr", let_expr_1),
            },
            Err(e) => panic!("{:?}", e),
        }
        let let_expr_2 = "(let* ([x 3]) 5)";
        match expr(let_expr_2) {
            Ok((_, e)) => match e {
                ast::Expr::Let(kind, bindings, body) => {
                    println!("let: {:?} {:?} {:?}", kind, bindings, body)
                }
                _ => panic!("Expected {} to be a setexpr", let_expr_2),
            },
            Err(e) => panic!("{:?}", e),
        }
    }
}
