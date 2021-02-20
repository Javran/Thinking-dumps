use std::collections::HashMap;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

pub struct Forth {
    env: Env,
    stack: Vec<Value>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

type Env = HashMap<String, Action>;

#[derive(Clone)]
enum Instr {
    Num(Value),
    Op(String),
}

enum Stmt {
    Instr(Instr),
    WordDef { name: String, body: Vec<Instr> },
}

#[derive(Clone)]
enum Action {
    Prim(fn(&mut Forth) -> ForthResult),
    Closure {
        env: Env,
        body: Vec<Instr>, // TODO: perhaps just needs a reference?
    },
}

fn parse_instr(raw: &str) -> Instr {
    match str::parse::<Value>(raw) {
        Ok(v) => Instr::Num(v),
        Err(_) => Instr::Op(raw.to_lowercase()),
    }
}

fn parse_stmt(tokens: &[&str]) -> Result<Stmt, Error> {
    println!("{:?}", tokens);
    // This implicitly requires that input is not empty.
    let (hd, tl) = tokens.split_at(1);
    match *hd.first().unwrap() {
        ":" => {
            // Recognize syntax: ':' <word name> <stmt>* ';'
            // While we can rule out word names that consists of only digits
            // it seems to suggest that should be interpreted as a runtime error rather than a syntactic one.
            if tl.len() <= 1 || tl.last() != Some(&";") {
                panic!("Incomplete word declaration.");
            }
            let name = tl[0];
            if let Ok(_) = str::parse::<Value>(name) {
                return Err(Error::InvalidWord);
            }
            Ok(Stmt::WordDef {
                name: name.to_lowercase(),
                body: tl[1..tl.len() - 1]
                    .iter()
                    .map(|tok| parse_instr(tok))
                    .collect(),
            })
        },
        _ => Ok(Stmt::Instr(parse_instr(hd[0])))
    }
}

fn parse(raw: &str) -> Result<Vec<Stmt>, Error> {
    let tokens: Vec<&str> = raw.split(' ').collect();
    let mut stmts = vec![];
    if tokens.is_empty() {
        return Ok(vec![]);
    }

    let mut i = 0;
    while i < tokens.len() {
        if tokens[i] == ":" {
            let mut j = i + 1;
            while j < tokens.len() && tokens[j] != ";" {
                j += 1;
            }
            if j == tokens.len() {
                return Err(Error::InvalidWord);
            }
            stmts.push(parse_stmt(&tokens[i..=j])?);
            i = j + 1;
        } else {
            stmts.push(Stmt::Instr(parse_instr(tokens[i])));
            i += 1;
        }
    }
    Ok(stmts)
}

impl Forth {
    fn pop(&mut self) -> Result<Value, Error> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Err(Error::StackUnderflow),
        }
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v)
    }

    pub fn new() -> Forth {
        let env: Env = {
            let mut m = HashMap::new();
            // TODO: macros
            m.insert(
                "+".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    let b = state.pop()?;
                    let a = state.pop()?;
                    Ok(state.push(a + b))
                }),
            );

            m.insert(
                "-".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    let b = state.pop()?;
                    let a = state.pop()?;
                    Ok(state.push(a - b))
                }),
            );

            m.insert(
                "*".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    let b = state.pop()?;
                    let a = state.pop()?;
                    Ok(state.push(a * b))
                }),
            );

            m.insert(
                "/".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    let b = state.pop()?;
                    if b == 0 {
                        return Err(Error::DivisionByZero);
                    }
                    let a = state.pop()?;
                    Ok(state.push(a / b))
                }),
            );

            m.insert(
                "dup".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    let a = state.pop()?;
                    state.push(a);
                    state.push(a);
                    Ok(())
                }),
            );
            m.insert(
                "drop".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    state.pop()?;
                    Ok(())
                }),
            );

            m.insert(
                "swap".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    let b = state.pop()?;
                    let a = state.pop()?;
                    state.push(b);
                    state.push(a);
                    Ok(())
                }),
            );

            m.insert(
                "over".to_string(),
                Action::Prim(|state: &mut Forth| -> Result<(), Error> {
                    let b = state.pop()?;
                    let a = state.pop()?;
                    state.push(a);
                    state.push(b);
                    state.push(a);
                    Ok(())
                }),
            );
            m
        };

        Forth { env, stack: vec![] }
    }

    pub fn stack(&self) -> &[Value] {
        &self.stack
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        parse(input)?
            .iter()
            .try_for_each(|stmt| self.eval_stmt(&stmt))
    }

    fn eval_action(&mut self, action: &Action) -> ForthResult {
        match action {
            Action::Prim(f) => f(self),
            Action::Closure { env, body } => {
                let genv = self.env.clone();
                self.env = env.clone();
                let result = body
                    .iter()
                    .try_for_each(|instr| self.eval_stmt(&Stmt::Instr(instr.clone())));
                self.env = genv.clone();
                result
            }
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> ForthResult {
        match stmt {
            Stmt::Instr(Instr::Num(v)) => {
                self.push(*v);
                Ok(())
            }
            Stmt::Instr(Instr::Op(sym)) => match self.env.get(sym).map(|x| x.clone()) {
                Some(action) => self.eval_action(&action),
                None => Err(Error::UnknownWord),
            },
            Stmt::WordDef { name, body } => {
                self.env.insert(
                    name.clone(),
                    Action::Closure {
                        env: self.env.clone(),
                        body: body.to_vec(),
                    },
                );
                Ok(())
            }
        }
    }
}
