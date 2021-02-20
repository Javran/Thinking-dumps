// Using immutable data structure to reduce cloning cost on `Env` (see below).
use im::HashMap;

pub type Value = i32;
type FResult<R> = Result<R, Error>;
pub type ForthResult = FResult<()>;

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
        // ideally this can be borrowed from Stmt,
        // but I don't want lifetime to spread everywhere.
        body: Vec<Instr>,
    },
}

fn parse_instr(raw: &str) -> Instr {
    match str::parse::<Value>(raw) {
        Ok(v) => Instr::Num(v),
        Err(_) => Instr::Op(raw.to_lowercase()),
    }
}

// Accepts a slice representing a word declaration without the surrounding ':' and ';'.
fn parse_word_decl(tokens: &[&str]) -> FResult<Stmt> {
    if tokens.is_empty() {
        return Err(Error::InvalidWord);
    }

    let (hd, tl) = tokens.split_at(1);
    let name = hd[0];
    if str::parse::<Value>(name).is_ok() {
        return Err(Error::InvalidWord);
    }
    Ok(Stmt::WordDef {
        name: name.to_lowercase(),
        body: tl.iter().map(|tok| parse_instr(tok)).collect(),
    })
}

fn parse(raw: &str) -> FResult<Vec<Stmt>> {
    let tokens: Vec<&str> = raw.split(' ').collect();
    let mut stmts = vec![];
    if tokens.is_empty() {
        return Ok(vec![]);
    }

    // Old fashion index manipulations.
    // This would be more doable using iterators,
    // should `take_while` also return the remaining iterator rather than ignoring it.
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
            stmts.push(parse_word_decl(&tokens[i + 1..j])?);
            i = j + 1;
        } else {
            stmts.push(Stmt::Instr(parse_instr(tokens[i])));
            i += 1;
        }
    }
    Ok(stmts)
}

impl Default for Forth {
    fn default() -> Forth {
        let env: Env = {
            let mut m = HashMap::new();

            macro_rules! define_op {
                // Note that operands are listed in the order they are popped.
                ($op_name:expr, $state: ident, $($operand: ident,)+ $body:block) => {
                    m.insert(
                        $op_name.to_lowercase(),
                        Action::Prim(|$state: &mut Forth| -> ForthResult {
                            $(
                                let $operand = $state.pop()?;
                            )*
                            $body
                        }),
                    );
                };
            }

            define_op!("+", st, b, a, {
                st.push(a + b);
                Ok(())
            });
            define_op!("-", st, b, a, {
                st.push(a - b);
                Ok(())
            });
            define_op!("*", st, b, a, {
                st.push(a * b);
                Ok(())
            });
            define_op!("/", st, b, a, {
                if b == 0 {
                    return Err(Error::DivisionByZero);
                }
                st.push(a / b);
                Ok(())
            });
            define_op!("drop", st, _a, { Ok(()) });

            // For `dup`: While it could be more performant
            // if we treat Forth stack as a vector rather than using stack interfaces,
            // I don't feel it's necessary to go that extra mile.
            // Same applies to `swap` and `over`.
            define_op!("dup", st, a, {
                st.push(a);
                st.push(a);
                Ok(())
            });
            define_op!("swap", st, b, a, {
                st.push(b);
                st.push(a);
                Ok(())
            });
            define_op!("over", st, b, a, {
                st.push(a);
                st.push(b);
                st.push(a);
                Ok(())
            });

            m
        };

        Forth { env, stack: vec![] }
    }
}

impl Forth {
    fn pop(&mut self) -> FResult<Value> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Err(Error::StackUnderflow),
        }
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v)
    }

    pub fn new() -> Forth {
        Forth::default()
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
                let tmp_env = self.env.clone();
                self.env = env.clone();
                let result = body
                    .iter()
                    .try_for_each(|instr| self.eval_stmt(&Stmt::Instr(instr.clone())));
                self.env = tmp_env;
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
            Stmt::Instr(Instr::Op(sym)) =>
            // seems silly that this needs to be cloned
            // but I don't have a better way to work around this borrowing issue.
            {
                match self.env.get(sym).cloned() {
                    Some(action) => self.eval_action(&action),
                    None => Err(Error::UnknownWord),
                }
            }
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
