// Using immutable data structure to reduce cloning cost on `Env` (see below).
use im::HashMap;

pub type Value = i32;
// More typing but less typing :)
type FResult<R> = Result<R, Error>;
pub type ForthResult = FResult<()>;

pub struct Forth {
    // Ideally Env should be passed around rather than being stored as a member of
    // Forth, but the way test suite is implemented doesn't pass around
    // any objects between `Forth::eval` calls, so all states must be kept here.
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

/// `Instr` represents basic Forth instructions.
/// Note that operation's name is always in lowercase.
#[derive(Clone)]
enum Instr {
    Num(Value),
    Op(String),
}

/// `Stmt` represents a Forth statement,
/// which is either an `Instr` or a word definition (`WordDef`).
/// Note that word name is always in lowercase.
enum Stmt {
    Instr(Instr),
    WordDef {
        name: String,
        // We have this distinction between `Instr` and `Stmt`
        // so that body doesn't have to be a `Vec<Stmt>`.
        // It's possible, but it comes with a lot more complexity and caveats
        // that this exercise's test suite doesn't specify.
        body: Vec<Instr>,
    },
}

/// `Action` stores actions available on current Forth machine.
/// - `Prim` are primitive actions that ships with Forth's initial environment.
/// - `Closure` represents user-defined words and the environment the word is defined in.
#[derive(Clone)]
enum Action {
    Prim(fn(&mut Forth) -> ForthResult),
    Closure {
        env: Env,
        // Those are usually moved from `WordDef`.
        body: Vec<Instr>,
    },
}

fn parse_instr(raw: &str) -> Instr {
    match str::parse::<Value>(raw) {
        Ok(v) => Instr::Num(v),
        Err(_) => Instr::Op(raw.to_lowercase()),
    }
}

/// Parses a slice representing a word declaration without the surrounding ':' and ';'.
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
    // clippy gets upset if I don't implement Default.
    fn default() -> Forth {
        let env: Env = {
            let mut m = HashMap::new();

            macro_rules! define_op {
                // Note that operands are listed in the order they are popped.
                // I would prefer if arguments are reversed,
                // but I haven't found a good way to do that without hurting readablity.
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

    // Unless we want to put a maximum stack limit, `push` can't fail.
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
            .into_iter()
            .try_for_each(|stmt| self.eval_stmt(stmt))
    }

    fn eval_action(&mut self, action: &Action) -> ForthResult {
        match action {
            Action::Prim(f) => f(self),
            Action::Closure { env, body } => {
                // The idea of a closure is that it carries its own environment,
                // which was captured during its word definition.
                // Here we need to swap out self.env temporarily in order to
                // evaluate closure body correctly and then swap back self.env after it's done.
                // Again, ideally Env should be passed around rather than being a member of Forth.

                // Hopefully using an immutable data structure reduces costs of
                // all those clonings below.
                let mut clo_env = env.clone();
                std::mem::swap(&mut clo_env, &mut self.env);
                let result = body
                    .iter()
                    .try_for_each(|instr| self.eval_stmt(Stmt::Instr(instr.clone())));
                std::mem::swap(&mut clo_env, &mut self.env);
                result
            }
        }
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> ForthResult {
        match stmt {
            Stmt::Instr(Instr::Num(v)) => {
                self.push(v);
                Ok(())
            }
            Stmt::Instr(Instr::Op(sym)) =>
            // seems silly that this needs to be cloned
            // but I don't have a better way to work around this borrowing issue.
            {
                match self.env.get(&sym).cloned() {
                    Some(action) => self.eval_action(&action),
                    None => Err(Error::UnknownWord),
                }
            }
            Stmt::WordDef { name, body } => {
                self.env.insert(
                    name,
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
