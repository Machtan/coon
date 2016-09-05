use std::iter::{Iterator, Peekable};
use std::str::CharIndices;

#[derive(Debug)]
pub enum Kw {
    Let,
}

#[derive(Debug)]
pub enum TokenData<'a> {
    Keyword(Kw),
    Var(&'a str),
    Number(&'a str),
    Whitespace,
    Newline,
    Arrow,
    
    EqEq,
    Or,
    And,
    BinOr,
    BinAnd,
    
    Eq,
    Plus,
    Minus,
    Times,
    Slash,
    
    PlusEq,
    MinusEq,
    TimesEq,
    SlashEq,
    
    Dot,
    Comma,
    Colon,
    CurlyOpen,
    CurlyClose,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
}

impl<'a> TokenData<'a> {
    fn identifier(text: &'a str) -> TokenData<'a> {
        use self::TokenData::*;
        match text {
            "let" => Keyword(Kw::Let),
            _ => Var(text),
        }
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    start: usize,
    len: usize,
    data: TokenData<'a>,
}

impl<'a> Token<'a> {
    pub fn new(start: usize, len: usize, data: TokenData<'a>) -> Token<'a> {
        Token {
            start: start,
            len: len,
            data: data,
        }
    }
}

#[derive(Debug)]
pub enum LexerErrorKind {
    InvalidWhitespace,
    InvalidNumber,
}

#[derive(Debug)]
pub struct LexerError<'a> {
    kind: LexerErrorKind,
    start: usize,
    pos: usize,
    source: &'a str,
}

impl<'a> LexerError<'a> {
    pub fn new(source: &'a str, start: usize, pos: usize, kind: LexerErrorKind) -> LexerError<'a> {
        LexerError {
            kind: kind,
            start: start,
            pos: pos,
            source: source,
        }
    }
}

type LexerResult<'a> = Result<Token<'a>, LexerError<'a>>;

#[derive(Debug)]
pub struct Lexer<'a> {
    text: &'a str,
    pos: usize,
    chars: Peekable<CharIndices<'a>>,
    finished: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Lexer<'a> {
        Lexer {
            text: text,
            pos: 0,
            chars: text.char_indices().peekable(),
            finished: false,
        }
    }
    
    #[inline]
    fn end(&mut self) -> usize {
        if let Some(&(i, _)) = self.chars.peek() {
            i
        } else {
            self.text.len()
        }
    }
    
    fn eat_identifier(&mut self) -> LexerResult<'a> {
        let start = self.pos;
        let end = self.end();
        self.pos = end;
        let text = &self.text[start .. end];
        Ok(Token::new(start, end-start, TokenData::identifier(text)))
    }
    
    fn eat_number(&mut self) -> LexerResult<'a> {
        let start = self.pos;
        let end = self.end();
        self.pos = end;
        let text = &self.text[start .. end];
        Ok(Token::new(start, end-start, TokenData::Number(text)))
    }
    
    #[inline]
    fn eat_token(&mut self, data: TokenData<'a>) -> LexerResult<'a> {
        let start = self.pos;
        let end = self.end();
        let len = end - start;
        self.pos = end;
        Ok(Token::new(start, len, data))
    }
    
    #[inline]
    fn eat_if_next(&mut self, ch: char, next: TokenData<'a>, else_: TokenData<'a>) -> LexerResult<'a> {
        if self.peek_is(ch) {
            self.chars.next();
            self.eat_token(next)
        } else {
            self.eat_token(else_)
        }
    }
    
    fn error(&mut self, pos: usize, kind: LexerErrorKind) -> LexerResult<'a> {
        self.finished = true;
        Err(LexerError::new(self.text, self.pos, pos, kind))
    }
    
    fn read_identifier(&mut self) -> LexerResult<'a> {
        while let Some(&(i, ch)) = self.chars.peek() {
            match ch {
                'a'...'z' | 'A' ... 'Z' | '_' | '0'...'9' => {
                    self.chars.next();
                }
                _ => break,
            }
        }
        self.eat_identifier()
    }
    
    fn read_whitespace(&mut self) -> LexerResult<'a> {
        while let Some(&(i, ch)) = self.chars.peek() {
            if ch == '\r' || ch == '\n' {
                break;
            } else if ch.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
        self.eat_token(TokenData::Whitespace)
    }
    
    fn read_number(&mut self, mut after_dot: bool) -> LexerResult<'a> {
        use self::LexerErrorKind::*;
        while let Some(&(i, ch)) = self.chars.peek() {
            match ch {
                '.'  => {
                    if after_dot {
                        return self.error(i, InvalidNumber);
                    } else {
                        after_dot = true;
                        self.chars.next();
                    }
                }
                '0' ... '9' => {
                    self.chars.next();
                }
                _ => break,
            }
        }
        self.eat_number()
    }
    
    #[inline]
    fn peek_is(&mut self, ch: char) -> bool {
        if let Some(&(_, c)) = self.chars.peek() {
            c == ch
        } else {
            false
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexerResult<'a>;
    
    fn next(&mut self) -> Option<Self::Item> {
        use self::TokenData::*;
        use self::LexerErrorKind::*;
        if self.finished {
            return None;
        }
        if let Some((i, ch)) = self.chars.next() {
            Some(match ch {
                'a'...'z' | 'A' ... 'Z' | '_' => {
                    self.read_identifier()
                }
                '\r' => {
                    if self.peek_is('\n') {
                        self.chars.next();
                        self.eat_token(Newline)
                    } else {
                        self.error(i, InvalidWhitespace)
                    }
                }
                '\n' => {
                    self.eat_token(Newline)
                }
                ch if ch.is_whitespace() => {
                    self.read_whitespace()
                }
                '0' ... '9' => {
                    self.read_number(false)
                }
                '.' => {
                    if let Some(&(i, ch)) = self.chars.peek() {
                        match ch {
                            '0' ... '9' => {
                                self.read_number(true)
                            }
                            _ => {
                                self.eat_token(Dot)
                            }
                        }
                    } else {
                        self.eat_token(Dot)
                    }
                }
                '=' => self.eat_if_next('=', EqEq, Eq),
                '|' => self.eat_if_next('|', Or, BinOr),
                '&' => self.eat_if_next('&', And, BinAnd),
                '+' => self.eat_if_next('=', PlusEq, Plus),
                '-' => {
                    if let Some(&(i, ch)) = self.chars.peek() {
                        match ch {
                            '>' => {
                                self.chars.next();
                                self.eat_token(Arrow)
                            }
                            '=' => {
                                self.chars.next();
                                self.eat_token(MinusEq)
                            }
                            _ => self.eat_token(Minus)
                        }
                    } else {
                        self.eat_token(Minus)
                    }
                },
                '*' => self.eat_if_next('=', TimesEq, Times),
                '/' => self.eat_if_next('=', SlashEq, Slash),
                ',' => self.eat_token(Comma),
                '{' => self.eat_token(CurlyOpen),
                '}' => self.eat_token(CurlyClose),
                '(' => self.eat_token(ParenOpen),
                ')' => self.eat_token(ParenClose),
                '[' => self.eat_token(BracketOpen),
                ']' => self.eat_token(BracketClose),
                ':' => self.eat_token(Colon),
                _ => {
                    println!("Cannot handle '{}'", ch);
                    unimplemented!();
                }
            })
        } else {
            self.finished = true;
            None
        }
    }
}


pub fn lex(text: &str) -> Lexer {
    Lexer::new(text)
}