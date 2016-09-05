#[macro_use]
extern crate pest; // pest = "0.4.0"

use pest::prelude::*;

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

// TODO: Unary minus :/
// TODO: Unary NOT
impl_rdp! {
    grammar! {
        expression = _{ // the underscore tells pest that this rule is silent
            { ["("] ~ expression ~ [")"] | literal | access }
            comparison      = { equality | inequality | and | or }
            addition        = { plus | minus }
            multiplication  = { times | slash }
        }
        number = @{ 
            (["-"]? ~ ['0'..'9']+ ~ ["."] ~ ['0'..'9']*) |
            (["-"]? ~ ['0'..'9']+)
        }
        literal = { number }
        
        plus = { ["+"] }
        minus = { ["-"] }
        times = { ["*"] }
        slash = { ["/"] }
        equality = { ["=="] }
        inequality = { ["!="] }
        or = { ["||"] }
        and = { ["&&"] }
        equals = { ["="] }
        
        whitespace = _{ [" "] }
        newline = _{ ["\r\n"] | ["\n"] }
        linebreak = _{ newline | eoi }
        
        let_ = { ["let"] }
        variable = @{ 
            (['a'..'z'] | ['A'..'Z'] | ["_"]) ~ 
            (['a'..'z'] | ['A'..'Z'] | ["_"] | ['0'..'9'])*
        }
        
        access = { variable }
        assignment = { let_? ~ variable ~ equals ~ expression ~ linebreak }
        
        statement = { assignment }
        
        coon = { (statement | newline)* ~ eoi }
    }
    
    process! {
        ast(&self) -> Ast {
            (&coon: ast()) => {
                println!("Parsing!");
                Vec::new()
            }
        }
        compute(&self) -> f32 {
            (&number: number) => number.parse::<f32>().unwrap(),
            (_: addition, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::plus => left + right,
                    Rule::minus => left - right,
                    _ => unreachable!(),
                }
            },
            (_: multiplication, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::times => left * right,
                    Rule::slash => left / right,
                    _ => unreachable!(),
                }
            }
        }
    }
}

pub enum CompileError {
    SyntaxError(String)
}

pub fn compile(text: &str) -> Result<String, CompileError> {
    let mut input = StringInput::new(text);
    
    let output = String::new();
    Ok(output)
}


#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;
    use super::Rdp;
    use super::pest::prelude::*;
    #[test]
    fn it_works() {
        let text = "let a = 2 + 2\nlet b = a\n\n";
        //let text = "a";
        let calc = "(3 + (9 + 3.5 * 00004.3 + (03 + 1.5) / 2 - 4)) * 2";
        let mut input = StringInput::new(text);
        let mut parser = Rdp::new(input);
        assert!(parser.coon());
        //assert!(parser.variable());
        assert!(parser.end());
        println!("Queue: {:?}", parser.queue());
        let ast = parser.ast();
        println!("AST: {:?}", ast);
        //
        
        panic!("SUCCES!");
        
        /*let file = File::open("test.coon").expect("Could not open file");
        let mut text = String::new();
        file.read_to_string(&mut text).expect("Could not read file");
        let output = compile(text).expect("Could not compile");
        println!("---------- OUTPUT ----------");
        println!("{}", output);*/
    }
}
