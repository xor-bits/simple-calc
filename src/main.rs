use std::{error::Error, fmt, io::stdin, iter::Peekable};

pub type Result<T, E = Box<dyn Error>> = core::result::Result<T, E>;

//

fn main() {
    for line in stdin().lines().map_while(Result::ok) {
        if let Err(err) = run(line.as_str()) {
            eprintln!("Error: {err}");
        }
    }
}

fn run(input: &str) -> Result<()> {
    println!("\ninput  = \"{input}\"");

    let tokens = tokenize(input).collect::<Result<Vec<Token>>>()?;
    println!("tokens = {tokens:?}");

    let ast = Ast::parse(tokens.into_iter())?;
    println!("ast    = {ast}");

    let result = ast.eval();
    println!("result = {result}");

    Ok(())
}

//

pub fn tokenize(str: &str) -> impl Iterator<Item = Result<Token>> + '_ {
    let mut i = 0;
    let mut chars = str.chars();

    core::iter::from_fn(move || {
        let ch = chars.next()?;

        let ch_i = i;
        i += ch.len_utf8();

        Some(Ok(match ch {
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Mult,
            '/' => Token::Div,
            '^' => Token::Pow,
            '0'..='9' => {
                let len = chars
                    .clone()
                    .take_while(char::is_ascii_digit)
                    .map(char::len_utf8)
                    .sum::<usize>()
                    + ch.len_utf8();
                let num = &str[ch_i..ch_i + len];
                chars = str[ch_i + len..].chars();
                i += len - ch.len_utf8();

                match num.parse() {
                    Ok(num) => Token::Num(num),
                    Err(e) => return Some(Err(e.into())),
                }
            }
            ch => {
                return Some(Err(format!("unexpected token '{ch}'").into()));
            }
        }))
    })
}

//

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    /// '('
    ParenOpen,

    /// ')'
    ParenClose,

    /// '+'
    Plus,

    /// '-'
    Minus,

    /// '*'
    Mult,

    /// '/'
    Div,

    /// '^'
    Pow,

    /// 'int(.int)'
    Num(f64),

    Eoi,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Mult => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Pow => write!(f, "^"),
            Token::Num(num) => write!(f, "{num}"),
            Token::Eoi => write!(f, "<EOI>"),
        }
    }
}

//

/// fairly cheap to clone
#[derive(Debug, Clone)]
pub struct Ast {
    arena: SparseVec<Node>,
    root: Node,

    const_neg_one: Option<Key>,
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_node(f, self.root)
    }
}

impl Ast {
    pub const fn number(num: f64) -> Self {
        Self {
            arena: SparseVec::new(),
            root: Node::Num(num),

            const_neg_one: None,
        }
    }

    fn fmt_node(&self, f: &mut fmt::Formatter, node: Node) -> fmt::Result {
        match node {
            Node::Add(lhs, rhs) => {
                write!(f, "(")?;
                self.fmt_node(f, *self.arena.get(lhs).unwrap())?;
                write!(f, "+")?;
                self.fmt_node(f, *self.arena.get(rhs).unwrap())?;
                write!(f, ")")
            }
            Node::Mul(lhs, rhs) => {
                write!(f, "(")?;
                self.fmt_node(f, *self.arena.get(lhs).unwrap())?;
                write!(f, "*")?;
                self.fmt_node(f, *self.arena.get(rhs).unwrap())?;
                write!(f, ")")
            }
            Node::Pow(lhs, rhs) => {
                write!(f, "(")?;
                self.fmt_node(f, *self.arena.get(lhs).unwrap())?;
                write!(f, "^")?;
                self.fmt_node(f, *self.arena.get(rhs).unwrap())?;
                write!(f, ")")
            }
            Node::Num(num) => write!(f, "{num}"),
        }
    }

    pub fn eval(&self) -> f64 {
        self.eval_node(self.root)
    }

    pub fn eval_node(&self, node: Node) -> f64 {
        match node {
            Node::Add(lhs, rhs) => {
                self.eval_node(*self.arena.get(lhs).unwrap())
                    + self.eval_node(*self.arena.get(rhs).unwrap())
            }
            Node::Mul(lhs, rhs) => {
                self.eval_node(*self.arena.get(lhs).unwrap())
                    * self.eval_node(*self.arena.get(rhs).unwrap())
            }
            Node::Pow(lhs, rhs) => self
                .eval_node(*self.arena.get(lhs).unwrap())
                .powf(self.eval_node(*self.arena.get(rhs).unwrap())),
            Node::Num(num) => num,
        }
    }

    pub fn parse(tokens: impl Iterator<Item = Token>) -> Result<Self> {
        let mut this = Self::number(0.0);
        let mut tokens = tokens.peekable();
        let root_key = this.parse_expr(&mut tokens)?;

        if let Some(extra) = tokens.next() {
            return Err(format!("extra tokens '{extra}', ...").into());
        }

        this.root = *this.arena.get(root_key).unwrap();

        Ok(this)
    }

    fn const_neg_one(&mut self) -> Key {
        *self
            .const_neg_one
            .get_or_insert_with(|| self.arena.insert(Node::Num(-1.0)))
    }

    fn parse_expr(&mut self, tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Key> {
        let mut lhs = self.parse_term(tokens)?;

        while let Some(Token::Plus | Token::Minus) = tokens.peek() {
            let is_plus = tokens.next() == Some(Token::Plus);

            let mut rhs = self.parse_term(tokens)?;
            if !is_plus {
                let neg_one = self.const_neg_one();
                rhs = self.arena.insert(Node::Mul(rhs, neg_one))
            }

            lhs = self.arena.insert(Node::Add(lhs, rhs));
        }

        Ok(lhs)
    }

    fn parse_term(&mut self, tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Key> {
        let mut lhs = self.parse_atom(tokens)?;

        while let Some(Token::Mult | Token::Div) = tokens.peek() {
            let is_mult = tokens.next() == Some(Token::Mult);

            let mut rhs = self.parse_atom(tokens)?;
            if !is_mult {
                let neg_one = self.const_neg_one();
                rhs = self.arena.insert(Node::Pow(rhs, neg_one))
            }

            lhs = self.arena.insert(Node::Mul(lhs, rhs));
        }

        Ok(lhs)
    }

    fn parse_atom(&mut self, tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Key> {
        if let Some(expr) = self.parse_paren(tokens) {
            return expr;
        }

        let mut lhs = self.parse_num(tokens)?;

        while let Some(Token::Pow) = tokens.peek() {
            _ = tokens.next();

            let rhs = self.parse_atom(tokens)?;

            lhs = self.arena.insert(Node::Pow(lhs, rhs));
        }

        Ok(lhs)
    }

    fn parse_num(&mut self, tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Key> {
        match tokens.next().unwrap_or(Token::Eoi) {
            Token::Minus => {
                let neg_one = self.const_neg_one();
                let unary_target = self.parse_atom(tokens)?;
                Ok(self.arena.insert(Node::Mul(unary_target, neg_one)))
            }
            Token::Num(num) => Ok(self.arena.insert(Node::Num(num))),
            other => Err(format!("unexpected token '{other}'").into()),
        }
    }

    fn parse_paren(
        &mut self,
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Option<Result<Key>> {
        let Some(Token::ParenOpen) = tokens.peek() else {
            return None;
        };

        _ = tokens.next();
        let expr = self.parse_expr(tokens);

        let next = tokens.next();
        if next != Some(Token::ParenClose) {
            return Some(Err(format!(
                "expected ')' but got '{}'",
                next.unwrap_or(Token::Eoi)
            )
            .into()));
        }

        Some(expr)
    }
}

//

/// really cheap to copy
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Node {
    // ik. a real tree like structure with Box would be simpler
    // but I like this
    Add(Key, Key),
    Mul(Key, Key),
    Pow(Key, Key),
    Num(f64),
}

//

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key(usize);

//

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SparseVec<T> {
    inner: Vec<Option<T>>,
    free: Vec<Key>,
}

impl<T> SparseVec<T> {
    pub const fn new() -> Self {
        Self {
            inner: Vec::new(),
            free: Vec::new(),
        }
    }

    pub fn insert(&mut self, val: T) -> Key {
        if let Some(key) = self.free.pop() {
            self.inner[key.0] = Some(val);
            key
        } else {
            let key = Key(self.inner.len());
            self.inner.push(Some(val));
            key
        }
    }

    pub fn remove(&mut self, key: Key) -> Option<T> {
        let val = self.inner.get_mut(key.0)?.take()?;
        self.free.push(key);
        Some(val)
    }

    /// all keys become invalid
    pub fn clear(&mut self) {
        self.free.clear();
        self.inner.clear();
    }

    pub fn drain(&mut self) -> impl Iterator<Item = T> + '_ {
        self.free.clear();
        self.inner.drain(..).flatten()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, Key)> {
        self.inner
            .iter()
            .enumerate()
            .flat_map(|(i, val)| Some((val.as_ref()?, Key(i))))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&mut T, Key)> {
        self.inner
            .iter_mut()
            .enumerate()
            .flat_map(|(i, val)| Some((val.as_mut()?, Key(i))))
    }

    pub fn get(&self, key: Key) -> Option<&T> {
        self.inner.get(key.0)?.as_ref()
    }

    pub fn get_mut(&mut self, key: Key) -> Option<&mut T> {
        self.inner.get_mut(key.0)?.as_mut()
    }
}
