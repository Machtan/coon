
use lexer::{Token, Lexer, LexerError, lex};
use debug;

#[derive(Debug)]
pub enum Acc<'a> {
    Var(&'a str),
    Index(Box<Acc<'a>>, Box<Expression<'a>>),
    Member(Vec<&'a str>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    Number(&'a str),
    Access(Acc<'a>),
    BinOp(Box<Expression<'a>>, &'a str, Box<Expression<'a>>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Let {
        mut_: bool,
        var: &'a str,
        type_: Option<&'a str>,
        value: Option<Expression<'a>>
    }
}

pub type Ast<'a> = Vec<Statement<'a>>;

#[derive(Debug)]
pub enum ParseErrorKind<'a> {
    LexerError(LexerError<'a>),
    InvalidSyntax(Token<'a>),
    UnfinishedStatement { start: usize },
}

#[derive(Debug)]
pub struct ParseError<'a> {
    source: &'a str,
    kind: ParseErrorKind<'a>,
}

impl<'a> ParseError<'a> {
    pub fn new(source: &'a str, kind: ParseErrorKind<'a>) -> ParseError<'a> {
        ParseError {
            source: source,
            kind: kind,
        }
    }
    
    pub fn explain(&self) {
        use self::ParseErrorKind::*;
        match self.kind {
            LexerError(ref _err) => {
                println!("Lexer Error"); // TODO: err.explain()
            },
            InvalidSyntax(ref token) => {
                let (line, col) = debug::get_position(self.source, token.start);
                println!("Invalid syntax at {}:{} (unexpected {:?}):", line, col, token.data);
                debug::show_invalid_character(self.source, token.start);
            }
            UnfinishedStatement { start } => {
                let (line, col) = debug::get_position(self.source, start);
                println!("Unfinished statement starting at {}:{} :", line, col);
                debug::show_unclosed(self.source, start);
            }
        }
    }
}

impl<'a> From<LexerError<'a>> for ParseError<'a> {
    fn from(err: LexerError<'a>) -> ParseError<'a> {
        ParseError::new(err.source, ParseErrorKind::LexerError(err))
    }
}

pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

struct Parser<'a> {
    text: &'a str,
    tokens: Lexer<'a>,
    ast: Ast<'a>,
}

#[derive(Debug, Clone, Copy)]
enum LetParseState<'a> {
    AfterLet,
    AfterMut,
    AfterVar { mut_: bool, var: &'a str },
    AfterColon { mut_: bool, var: &'a str},
    AfterType { mut_: bool, var: &'a str, type_: Option<&'a str>},
}

#[derive(Debug, Clone, Copy)]
enum ExprParseState {
    Empty
}

impl<'a> Parser<'a> {
    pub fn new(text: &'a str) -> Parser<'a> {
        Parser {
            text: text,
            tokens: lex(text),
            ast: Vec::new(),
        }
    }
    
    fn parse_expr(&mut self) -> ParseResult<'a, Expression<'a>>{
        use self::ExprParseState::*;
        let mut state = Empty;
        while let Some(token) = self.tokens.next() {
            
        }
        match state {
            Empty => unimplemented!(),
            //_ => unimplemented!(),
        }
    }
    
    #[inline]
    fn add_statement(&mut self, statement: Statement<'a>) {
        println!("{:?}", statement);
        self.ast.push(statement);
    }
    
    #[inline]
    fn error<T>(&self, kind: ParseErrorKind<'a>) -> ParseResult<'a, T> {
        Err(ParseError::new(self.text, kind))
    }
    
    // Expected: let ~ ws ~ (mut ~ ws)? ~ var ~ (ws? ~ : ~ ws? ~ var) ~ ws? ~ = ~ ws? ~ EXPR ~ ws? ~ linebreak
    fn parse_let(&mut self, start: usize) -> ParseResult<'a, ()> {
        use self::LetParseState::*;
        use lexer::TokenData::*;
        use lexer::Kw::*;
        use self::ParseErrorKind::*;
        let mut state = AfterLet;
        
        while let Some(token) = self.tokens.next() {
            let token = token?;
            state = match (state, token.data) {
                (_, Whitespace) => continue,
                (AfterLet, Keyword(Mut)) => {
                    AfterMut
                }
                (AfterLet, Identifier(var)) => {
                    AfterVar { mut_: false, var: var }
                }
                (AfterMut, Identifier(var)) => {
                    AfterVar { mut_: true, var: var }
                }
                (AfterVar { mut_, var }, Colon) => {
                    AfterColon { mut_: mut_, var: var }
                }
                (AfterVar { mut_, var }, Eq) => {
                    let expr = self.parse_expr()?;
                    let statement = Statement::Let {
                        mut_: mut_, var: var, type_: None, value: Some(expr)
                    };
                    self.add_statement(statement);
                    return Ok(());
                }
                (AfterVar { mut_, var }, Newline) => {
                    let statement = Statement::Let { 
                        mut_: mut_, var: var, type_: None, value: None
                    };
                    self.add_statement(statement);
                    return Ok(());
                }
                (AfterColon { mut_, var }, Identifier(type_)) => {
                    AfterType { mut_: mut_, var: var, type_: Some(type_)}
                }
                (AfterType { mut_, var, type_ }, Eq) => {
                    let expr = self.parse_expr()?;
                    let statement = Statement::Let {
                        mut_: mut_, var: var, type_: type_, value: Some(expr)
                    };
                    self.add_statement(statement);
                    return Ok(());
                }
                (AfterType { mut_, var, type_ }, Newline) => {
                    let statement = Statement::Let {
                        mut_: mut_, var: var, type_: type_, value: None
                    };
                    self.add_statement(statement);
                    return Ok(());
                }

                _ => {
                    println!("Error: parse_let: (state: {:?})", state);
                    return self.error(InvalidSyntax(token));
                }
            };
        }
        match state {
            AfterVar { mut_, var} => {
                let statement = Statement::Let { 
                    mut_: mut_, var: var, type_: None, value: None
                };
                self.add_statement(statement);
                Ok(())
            }
            AfterType { mut_, var, type_ } => {
                let statement = Statement::Let { 
                    mut_: mut_, var: var, type_: type_, value: None
                };
                self.add_statement(statement);
                Ok(())
            }
            _ => {
                self.error(UnfinishedStatement{ start: start })
            }
        }
    }
    
    pub fn parse(mut self) -> ParseResult<'a, Ast<'a>> {
        use self::ParseErrorKind::*;
        use lexer::TokenData::*;
        use lexer::Kw::*;
        
        while let Some(token) = self.tokens.next() {
            let token = token?;
            match token.data {
                Whitespace => continue,
                Newline => continue,
                Keyword(Let) => self.parse_let(token.start)?,
                other => println!("Unhandled token: {:?}", other),
            }
        }
        Ok(self.ast)
    }
}

pub fn parse<'a>(text: &'a str) -> ParseResult<'a, Ast<'a>> {
    Parser::new(text).parse()
}