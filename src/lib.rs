#![feature(question_mark)]

mod debug;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests {
    use super::lexer;
    use super::parser;
    #[test]
    fn it_works() {
        let source = include_str!("../test.coon");
        for (i, res) in lexer::lex(source).enumerate() {
            let token = res.expect("Lexer error!");
            println!("{:03}: {:?}", i+1, token);
        }
        let ast = match parser::parse(source) {
            Ok(ast) => ast,
            Err(err) => {
                err.explain();
                panic!("ERR");
            }
        };
        println!("========= Statements =========");
        for (i, statement) in ast.iter().enumerate() {
            println!("{:03}: {:?}", i+1, statement);
        }
        panic!("SUCCES!");
    }
}