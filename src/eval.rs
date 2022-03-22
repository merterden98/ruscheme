use std::collections::HashMap;

use crate::ast::{Defn, Expr, List, Literal, Prim, Program, Value};
use quick_error::quick_error;
pub type Env = HashMap<String, Box<Value>>;

pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: HashMap::new(),
        }
    }

    fn with_env(env: Env) -> Evaluator {
        Evaluator { env }
    }

    fn list_to_pair(&self, l: Box<List>) -> Value {
        let mut ptr = l;
        let mut lit_vec: Vec<Literal> = Vec::new();
        while let List::Cons(v, rest) = *ptr {
            ptr = rest;
            lit_vec.push(v);
        }

        lit_vec.into_iter().rev().fold(Value::Nil, |acc, cur| {
            let val = match cur {
                Literal::Int(i) => Value::Num(i),
                Literal::Bool(b) => Value::Bool(b),
                Literal::Str(s) => Value::Sym(s),
                Literal::List(l) => self.list_to_pair(l),
            };
            Value::Pair(Box::new(val), Box::new(acc))
        })
    }
}

pub trait Evaluates {
    fn eval_program(&mut self, pgm: Program) -> Result<String, RunTimeError>;
    fn eval_defn(&mut self, defn: Defn) -> Result<String, RunTimeError>;
    fn eval_expr(&mut self, expr: Expr) -> Result<Value, RunTimeError>;
    fn apply_prim(&mut self, prim: Prim, e_list: Vec<Expr>) -> Result<Value, RunTimeError>;
}

impl Evaluates for Evaluator {
    fn eval_program(&mut self, pgm: Program) -> Result<String, RunTimeError> {
        let mut print = String::new();
        for defn in pgm {
            print = self.eval_defn(defn)?;
        }
        Ok(print)
    }

    fn eval_defn(&mut self, defs: Defn) -> Result<String, RunTimeError> {
        match defs {
            Defn::Expr(e) => {
                self.eval_defn(Defn::Val("it".to_string(), e))?;
                Ok(self.env.get("it").unwrap().to_string())
            }
            Defn::Val(name, e) => {
                let val = self.eval_expr(e)?;
                self.env.insert(name, Box::new(val.clone()));
                Ok(val.to_string())
            }
            Defn::Define(name, formals, body) => {
                self.eval_defn(Defn::Val(
                    name.clone(),
                    Expr::Lambda((formals, Box::new(body))),
                ))?;
                Ok(name)
            }
        }
    }
    fn eval_expr(&mut self, expr: Expr) -> Result<Value, RunTimeError> {
        match expr {
            Expr::Literal(v) => match v {
                crate::ast::Literal::Int(i) => Ok(Value::Num(i)),
                crate::ast::Literal::Bool(b) => Ok(Value::Bool(b)),
                crate::ast::Literal::Str(s) => Ok(Value::Sym(s)),
                crate::ast::Literal::List(l) => Ok(self.list_to_pair(l)),
            },
            Expr::Var(ident) => {
                if let Some(v) = self.env.get(&ident) {
                    Ok(*v.clone())
                } else {
                    Err(RunTimeError::UnboundVar(ident))
                }
            }
            Expr::Set(ident, e) => {
                if let Some(_) = self.env.get(&ident) {
                    let new_val = self.eval_expr(*e)?;
                    self.env.insert(ident, Box::new(new_val.clone()));
                    Ok(new_val)
                } else {
                    panic!()
                }
            }
            Expr::If(e1, e2, e3) => {
                let v = self.eval_expr(*e1)?;
                match v.project_boolean() {
                    Value::Bool(true) => self.eval_expr(*e2),
                    Value::Bool(false) => self.eval_expr(*e3),
                    _ => panic!(),
                }
            }
            Expr::Apply(e, e_list) => match self.eval_expr(*e)? {
                Value::Closure((formals, body), env) => {
                    let binding_vals: Vec<Result<Value, RunTimeError>> =
                        e_list.into_iter().map(|e| self.eval_expr(e)).collect();
                    let mut new_env = env.clone();
                    for (formal, val) in formals.into_iter().zip(binding_vals) {
                        new_env.insert(formal, Box::new(val?));
                    }
                    Evaluator::with_env(new_env).eval_expr(*body)
                }
                Value::Prim(p) => self.apply_prim(p, e_list),
                _ => panic!(),
            },
            Expr::Lambda((formals, body)) => Ok(Value::Closure((formals, body), self.env.clone())),
            Expr::Let(crate::ast::LetKind::Let, bindings, body) => {
                let vals: Vec<(String, Result<Value, RunTimeError>)> = bindings
                    .into_iter()
                    .map(|(ident, e)| (ident, self.eval_expr(e)))
                    .collect();

                for val in vals {
                    self.env.insert(val.0, Box::new(val.1?.clone()));
                }

                self.eval_expr(*body)
            }
            Expr::Let(crate::ast::LetKind::LetStar, bindings, body) => {
                for binding in bindings {
                    let (ident, e) = binding;
                    let val = self.eval_expr(e)?;
                    self.env.insert(ident, Box::new(val.clone()));
                }

                self.eval_expr(*body)
            }
            Expr::Let(crate::ast::LetKind::LetRec, bindings, body) => {
                bindings.iter().for_each(|(ident, e)| match e {
                    Expr::Lambda(_) => {
                        self.env.insert(ident.clone(), Box::new(Value::Nil));
                    }
                    _ => panic!("Needs to be a Lambda"),
                });
                let closures: Vec<Result<Value, RunTimeError>> = bindings
                    .iter()
                    .map(|(_, e)| self.eval_expr(e.clone()))
                    .collect();
                for (binding, cls) in bindings.into_iter().zip(closures) {
                    let (ident, _) = binding;
                    let val = cls?;
                    self.env.insert(ident, Box::new(val.clone()));
                }

                self.eval_expr(*body)
            }
            Expr::Begin(es) => {
                let mut last = Value::Bool(false);
                for e in es {
                    last = self.eval_expr(e)?;
                }
                Ok(last)
            }
            Expr::Primitive(p) => Ok(Value::Prim(p)),
        }
    }

    fn apply_prim(&mut self, prim: Prim, e_list: Vec<Expr>) -> Result<Value, RunTimeError> {
        match prim {
            Prim::Add => {
                if e_list.len() != 2 {
                    panic!("Expected 2 parameters to +, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    let v2 = self.eval_expr(e_list[1].to_owned())?;
                    match (&v1, &v2) {
                        (Value::Num(v1), Value::Num(v2)) => Ok(Value::Num(v1 + v2)),
                        _ => panic!("Expected: int+int, got {:?} {:?}", v1, v2),
                    }
                }
            }
            Prim::Sub => {
                if e_list.len() != 2 {
                    panic!("Expected 2 parameters to -, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    let v2 = self.eval_expr(e_list[1].to_owned())?;
                    match (&v1, &v2) {
                        (Value::Num(v1), Value::Num(v2)) => Ok(Value::Num(v1 - v2)),
                        _ => panic!("Expected: int-int, got {:?} {:?}", v1, v2),
                    }
                }
            }
            Prim::Mul => {
                if e_list.len() != 2 {
                    panic!("Expected 2 parameters to *, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    let v2 = self.eval_expr(e_list[1].to_owned())?;
                    match (&v1, &v2) {
                        (Value::Num(v1), Value::Num(v2)) => Ok(Value::Num(v1 * v2)),
                        _ => panic!("Expected: int*int, got {:?} {:?}", v1, v2),
                    }
                }
            }
            Prim::Div => {
                if e_list.len() != 2 {
                    panic!("Expected 2 parameters to +, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    let v2 = self.eval_expr(e_list[1].to_owned())?;
                    match (&v1, &v2) {
                        (Value::Num(v1), Value::Num(v2)) => Ok(Value::Num(v1 / v2)),
                        _ => panic!("Expected: int+int, got {:?} {:?}", v1, v2),
                    }
                }
            }
            Prim::Eq => {
                if e_list.len() != 2 {
                    panic!("Expected 2 parameters to +, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    let v2 = self.eval_expr(e_list[1].to_owned())?;
                    match (&v1, &v2) {
                        (Value::Num(v1), Value::Num(v2)) => Ok(Value::Bool(v1 == v2)),
                        (Value::Sym(s1), Value::Sym(s2)) => Ok(Value::Bool(s1.eq(s2))),
                        (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 == b2)),
                        (Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
                        _ => Ok(Value::Bool(false)),
                    }
                }
            }
            Prim::Car => {
                if e_list.len() != 1 {
                    panic!("Expected 1 parameters to +, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    match &v1 {
                        Value::Pair(v, _) => Ok(*v.to_owned()),
                        _ => panic!("Unable to car {}", v1.to_string()),
                    }
                }
            }
            Prim::Cdr => {
                if e_list.len() != 1 {
                    panic!("Expected 1 parameters to +, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    match &v1 {
                        Value::Pair(_, vs) => Ok(*vs.to_owned()),
                        _ => panic!("Unable to cdr {}", v1.to_string()),
                    }
                }
            }
            Prim::IsNull => {
                if e_list.len() != 1 {
                    panic!("Expected 1 parameters to +, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    match &v1 {
                        Value::Nil => Ok(Value::Bool(true)),
                        _ => Ok(Value::Bool(false)),
                    }
                }
            }
            Prim::Cons => {
                if e_list.len() != 2 {
                    panic!("Expected 2 parameters to +, got {}", e_list.len())
                } else {
                    let v1 = self.eval_expr(e_list[0].to_owned())?;
                    let v2 = self.eval_expr(e_list[1].to_owned())?;
                    Ok(Value::Pair(Box::new(v1), Box::new(v2)))
                }
            }
            Prim::ShortAnd => {
                for e in e_list {
                    match self.eval_expr(e.to_owned())?.project_boolean() {
                        Value::Bool(b) => {
                            if !b {
                                return Ok(Value::Bool(false));
                            }
                        }
                        _ => panic!("Expected: bool, got {:?}", e),
                    };
                }
                Ok(Value::Bool(true))
            }
            Prim::ShortOr => {
                for e in e_list {
                    let v = self.eval_expr(e.to_owned())?;
                    match v.project_boolean() {
                        Value::Bool(b) => {
                            if b {
                                return Ok(v);
                            }
                        }
                        _ => panic!("Expected: bool, got {:?}", v),
                    };
                }
                Ok(Value::Bool(false))
            }
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum RunTimeError {
        UnboundVar(var: String) { }
    }
}
