
mod lexer;


#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    InEq,
    And,
    Or,
}

// TODO: Consider using values here? (for optimizations?)
#[derive(Debug)]
pub enum Lit<'a> {
    Number(&'a str)
}

#[derive(Debug)]
pub enum Expression<'a> {
    Prim(Box<Expression<'a>>, Op, Box<Expression<'a>>),
    Var(&'a str),
    Lit(Lit<'a>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Let { mut_: bool, var: &'a str, value: Option<Expression<'a>> }
}

pub type Ast<'a> = Vec<Statement<'a>>;


#[cfg(test)]
mod tests {
    use super::lexer;
    #[test]
    fn it_works() {
        let source = include_str!("../test.coon");
        for (i, res) in lexer::lex(source).enumerate() {
            let token = res.expect("Lexer error!");
            println!("{:03}: {:?}", i+1, token);
        }
        panic!("SUCCES!");
    }
}