#![allow(dead_code)]

use std::{fs, error::Error};

use logos::{Lexer, Logos, Skip, Span};

type LexError = (String, Span);

#[derive(Debug, PartialEq, Default)]
struct Context {
    line: usize,
    column: usize,
}

#[derive(Debug, PartialEq)]
struct Loc {
    line: usize,
    column: usize,
    value: String,
}

impl Default for Loc {
    fn default() -> Self {
        Loc {
            line: 1,
            column: 1,
            value: "".to_owned(),
        }
    }
}

fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    Skip
}

fn string_callback(lex: &mut Lexer<Token>) -> Loc {
    let lines = lex.slice().lines();
    let newline_count = lines.clone().count();
    let start_line = lex.extras.line;
    let start_column = lex.extras.column;
    lex.extras.line += newline_count - 1;
    lex.extras.column = lex.span().end;
    Loc {
        line: start_line,
        column: start_column,
        value: lex.slice().to_owned(),
    }
}

fn symbol_validator(lex: &mut Lexer<Token>) -> Option<Loc> {
    let slice = lex.slice();
    if slice.ends_with('/') || slice.matches('/').count() > 1 {
        None
    } else {
        Some(Loc {
            line: lex.extras.line,
            column: lex.span().start - lex.extras.column,
            value: slice.to_string(),
        })
    }
}

fn attach_loc(lex: &mut Lexer<Token>) -> Loc {
    Loc {
        line: lex.extras.line,
        column: lex.span().start - lex.extras.column,
        value: lex.slice().to_string(),
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(extras = Context)]
enum Token {
    Error,

    #[regex(r"\r", |_| Skip)]
    #[regex(r"\n", newline_callback)]
    Newline,

    #[regex(r"[ \t\f,]+", |_| Skip)]
    Whitespace,

    #[token("(", attach_loc)]
    OpenParen(Loc),

    #[token(")", attach_loc)]
    CloseParen(Loc),

    #[token("{", attach_loc)]
    OpenCurly(Loc),

    #[token("}", attach_loc)]
    CloseCurly(Loc),

    #[token("[", attach_loc)]
    OpenSquare(Loc),

    #[token("]", attach_loc)]
    CloseSquare(Loc),

    #[token("nil", attach_loc)]
    Nil(Loc),

    #[token("false", attach_loc)]
    #[token("true", attach_loc)]
    Boolean(Loc),

    #[regex(r#"\\."#, attach_loc)]
    Char(Loc),

    #[token("'", attach_loc)]
    Quote(Loc),

    #[token("`", attach_loc)]
    SyntaxQuote(Loc),

    #[token("~", attach_loc)]
    Unquote(Loc),

    #[token("~@", attach_loc)]
    UnquoteSplicing(Loc),

    #[token("^", attach_loc)]
    #[token("#^", attach_loc)]
    Meta(Loc),

    #[token("@", attach_loc)]
    Deref(Loc),

    #[token("#'", attach_loc)]
    Var(Loc),

    #[token("#(", attach_loc)]
    Fn(Loc),

    #[regex(r"%\d*", attach_loc)]
    FnArg(Loc),

    #[token("#:", attach_loc)]
    NamespacedMap(Loc),

    #[token("#=", attach_loc)]
    Eval(Loc),

    #[token("#?", attach_loc)]
    ReaderConditional(Loc),

    #[token("#\"", attach_loc)]
    Regex(Loc),

    #[token("#_", attach_loc)]
    Discard(Loc),

    #[token("#{", attach_loc)]
    Set(Loc),

    #[regex("#![^\r\n]*", attach_loc)]
    #[regex(";[^\r\n]*", attach_loc)]
    Comment(Loc),

    #[regex("([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?", attach_loc, priority=2)]
    Integer(Loc),

    #[regex("[-+]?[0-9]+/[0-9]+", attach_loc)]
    Ratio(Loc),

    #[regex("[-+]?[0-9]+M", attach_loc)]
    #[regex("[-+]?[0-9]+\\.[0-9]*M?", attach_loc)]
    #[regex("[-+]?[0-9]+(\\.[0-9]*)?[eE][-+]?[0-9]+M?", attach_loc)]
    Decimal(Loc),

    #[token("##Inf", attach_loc)]
    #[token("##-Inf", attach_loc)]
    Inf(Loc),

    #[token("##NaN", attach_loc)]
    NaN(Loc),

    #[token("/", attach_loc)]
    #[regex(r#"([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*#?"#, symbol_validator)]
    #[regex(r#"([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*/([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]][^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]*)?#?"#, symbol_validator)]
    Symbol(Loc),

    #[token("::?/", attach_loc)]
    #[regex(r#"::?([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*"#, symbol_validator)]
    #[regex(r#"::?([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*/([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]][^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]*)?"#, symbol_validator)]
    Keyword(Loc),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, string_callback)]
    String(Loc),
}

fn main() -> Result<(), Box<dyn Error>> {
    let core_clj: String = fs::read_to_string("core.clj")?.parse()?;
    let start = std::time::Instant::now();
    let lex = Token::lexer(&core_clj);
    let duration = start.elapsed();
    println!("how long: {:?}", duration);

    let lex = Token::lexer(r#""The version info for Clojure core, as a map containing :major :minor :incremental and :qualifier keys. Feature releases may increment :minor and/or :major, bugfix releases will increment :incremental. Possible values of :qualifier include \"GA\", \"SNAPSHOT\", \"RC-x\" \"BETA-x\""#);
    println!("{:?}", lex.collect::<Vec<_>>());

    // let mut tokens = Vec::new();
    // let mut errors = Vec::new();
    // for result in lex {
    //     match result {
    //         Ok(token) => {
    //             tokens.push(token);
    //         }
    //         Err(e) => {
    //             println!("{:?}", tokens.iter().rev().take(10).rev().collect::<Vec<_>>());
    //             errors.push(e);
    //         }
    //     }
    // }
    // if errors.is_empty() {
    //     println!("Good job")
    // } else {
    //     println!("{:?}", Err::<(), Vec<(String, Span)>>(errors))
    // }

    Ok(())
}
