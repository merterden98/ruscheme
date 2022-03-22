use crate::eval::Env;

pub type Program = Vec<Defn>;
pub type Bindings = Vec<(String, Expr)>;

#[derive(Debug)]
pub enum Defn {
    Define(String, Vec<String>, Expr),
    Val(String, Expr),
    Expr(Expr),
}

pub type Lambda = (Vec<String>, Box<Expr>);

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(String),
    Primitive(Prim),
    Set(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Lambda(Lambda),
    Let(LetKind, Bindings, Box<Expr>),
    Begin(Vec<Expr>),
}
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Str(String),
    List(Box<List>),
}

#[derive(Debug, Clone)]
pub enum Prim {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Car,
    Cdr,
    Cons,
    IsNull,
    ShortAnd,
    ShortOr,
}

#[derive(Debug, Clone)]
pub enum List {
    Nil,
    Cons(Literal, Box<List>),
}

#[derive(Debug, Clone)]
pub enum LetKind {
    Let,
    LetStar,
    LetRec,
}

#[derive(Debug, Clone)]
pub enum Value {
    Sym(String),
    Num(i64),
    Bool(bool),
    Nil,
    Pair(Box<Value>, Box<Value>),
    Closure(Lambda, Env),
    Prim(Prim),
}

impl Value {
    pub fn project_boolean(&self) -> Value {
        match self {
            Value::Bool(b) => Value::Bool(*b),
            _ => Value::Bool(true),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Sym(s) => s.clone(),
            Value::Num(i) => i.to_string(),
            Value::Bool(b) => {
                if *b {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            Value::Nil => "nil".to_string(),
            Value::Pair(car, cdr) => {
                let mut pair_str = "".to_string();
                let mut ptr = cdr;
                pair_str.push('(');
                pair_str.push_str(car.to_string().as_str());
                loop {
                    match &**ptr {
                        Value::Nil => break,
                        Value::Pair(hd, tl) => {
                            pair_str.push(' ');
                            pair_str.push_str(hd.to_string().as_str());
                            ptr = tl;
                        }
                        v => {
                            pair_str.push_str(" . ");
                            pair_str.push_str(&v.to_string());
                            break;
                        }
                    }
                }
                pair_str.push(')');
                pair_str
            }
            Value::Closure(_, _) => "lambda".to_string(),
            Value::Prim(p) => match p {
                Prim::Add => "+".to_string(),
                Prim::Sub => "-".to_string(),
                Prim::Mul => "*".to_string(),
                Prim::Div => "/".to_string(),
                Prim::Eq => "=".to_string(),
                Prim::Car => "car".to_string(),
                Prim::Cdr => "cdr".to_string(),
                Prim::IsNull => "null?".to_string(),
                Prim::Cons => "cons".to_string(),
                Prim::ShortAnd => "&&".to_string(),
                Prim::ShortOr => "||".to_string(),
            },
        }
    }
}

// Tests:
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast() {
        let ast = Expr::Literal(Literal::Int(1));
        match ast {
            Expr::Literal(Literal::Int(i)) => assert_eq!(i, 1),
            _ => panic!("Unexpected AST variant"),
        }
    }
}
