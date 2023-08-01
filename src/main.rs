#![allow(dead_code)]

use logos::{Logos, Lexer, Skip};

#[derive(Debug, PartialEq, Default)]
struct Context {
    line: usize,
    column: usize,
}

fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    println!("newline cb: {:?}", lex.extras);
    Skip
}

fn string_callback(lex: &mut Lexer<Token>) -> () {
    println!("context: {:?}", lex.extras);
    let lines = lex.slice().lines();
    let newline_count = lines.clone().count();
    lex.extras.line += newline_count;
    lex.extras.column = lines.last().unwrap_or("").chars().count();
    println!("context: {:?}", lex.extras);
}

fn attach_loc(lex: &mut Lexer<Token>) {
    lex.extras.column = lex.span().end;
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = Context)]
enum Token {
    Error,

    #[regex(r"[\r\n]+", newline_callback)]
    Newline,

    #[regex(r"[ \t\f,]+", |_| Skip)]
    Whitespace,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token("{")]
    OpenCurly,

    #[token("}")]
    CloseCurly,

    #[token("[")]
    OpenSquare,

    #[token("]")]
    CloseSquare,

    #[token("nil")]
    Nil,

    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Boolean(bool),

    #[token("'")]
    Quote,

    #[token("`")]
    SyntaxQuote,

    #[token("~")]
    Unquote,

    #[token("~@")]
    UnquoteSplicing,

    #[token("^")]
    #[token("#^")]
    Meta,

    #[token("@")]
    Deref,

    #[token("#'")]
    Var,

    #[token("#(")]
    Fn,

    #[token("#:")]
    NamespacedMap,

    #[token("#=")]
    Eval,

    #[token("#?")]
    ReaderConditional,

    #[token("#\"")]
    Regex,

    #[token("#_")]
    Discard,

    #[token("#{")]
    Set,

    #[regex("#![^\r\n]*")]
    #[regex(";[^\r\n]*")]
    Comment,

    #[regex("([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")]
    Integer,

    #[regex("[-+]?[0-9]+/[0-9]+")]
    Ratio,

    #[regex("[-+]?[0-9]+M")]
    #[regex("[-+]?[0-9]+\\.[0-9]*M?")]
    #[regex("[-+]?[0-9]+(\\.[0-9]*)?[eE][-+]?[0-9]+M?")]
    Decimal,

    #[token("##Inf")]
    #[token("##-Inf")]
    Inf,

    #[token("##NaN")]
    NaN,

    #[regex(r#"([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])+"#, symbol_validator)]
    #[regex(r#"([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])+/([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]]*)?"#, symbol_validator)]
    Symbol,

    #[regex(r#"::?([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])+"#, symbol_validator)]
    #[regex(r#"::?([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])+/([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]]*)?"#, symbol_validator)]
    Keyword,

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, string_callback)]
    String,
}

fn symbol_validator(lex: &mut Lexer<Token>) -> bool {
    let slice = lex.slice();
    println!("{:?}", slice);
    !(slice.ends_with('/') || slice.matches('/').count() > 1)
}

fn main() {
    let lex = Token::lexer("sdf \"asdf \n asdf\r\n\r\n\"asdf\n");
    for token in lex {
    }
}
