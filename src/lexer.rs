// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use core::fmt;

use logos::{Lexer, Logos};

fn float_parser<'source>(lex: &mut Lexer<'source, Token<'source>>) -> f64 {
    match lex.slice() {
        "##Inf" => f64::INFINITY,
        "##-Inf" => f64::NEG_INFINITY,
        "##NaN" => f64::NAN,
        num => num.parse().unwrap_or(0.0),
    }
}

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum Token<'source> {
    Error,

    #[regex(r"[, \t\r\n]+", logos::skip)]
    Whitespace,

    #[regex("(#!|;)[^\r\n]*[\r\n]+", logos::skip, priority = 2)]
    Comment,

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

    #[regex("(true|false)", |lex| lex.slice() == "true")]
    Boolean(bool),

    #[regex(r#"\\."#, |lex| lex.slice().chars().next().unwrap())]
    Char(char),

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

    #[regex(r"%(\d*|&)")]
    FnArg(&'source str),

    #[token("#:")]
    NamespacedMap,

    #[token("#=")]
    Eval,

    #[token("#?(")]
    ReaderConditional,

    #[token("#\"\"")]
    #[regex(r#"#"[^"]+""#)]
    Regex(&'source str),

    #[token("#_", logos::skip)]
    Discard,

    #[token("#{")]
    Set,

    #[regex(r#"#([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*"#)]
    #[regex(r#"#([\D&&[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*/([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]][^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]*)?"#)]
    TaggedLiteral(&'source str),

    #[regex("[-+]?0N?", priority = 2)]
    #[regex("[-+]?[1-9][0-9]*N?", priority = 2)]
    #[regex("[-+]?0[xX][0-9A-Fa-f]+N?", priority = 2)]
    #[regex("[-+]?0[0-7]+N?", priority = 2)]
    #[regex("[-+]?[1-9][0-9]?[rR][0-9A-Za-z]+N?", priority = 2)]
    Integer(&'source str),

    #[regex("[-+]?[0-9]+/[0-9]+")]
    Ratio(&'source str),

    #[regex("##(-?Inf|NaN)", float_parser)]
    #[regex("[-+]?[0-9]+M", float_parser)]
    #[regex("[-+]?[0-9]+\\.[0-9]*M?", float_parser)]
    #[regex("[-+]?[0-9]+(\\.[0-9]*)?[eE][-+]?[0-9]+M?", float_parser)]
    Decimal(f64),

    #[regex(r#"([\D&&[^ \t\r\n\f,#'%":;@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*#?"#)]
    #[regex(r#"([\D&&[^ \t\r\n\f,#'%":;@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*/([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]][^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]*)?#?"#)]
    Symbol(&'source str),

    #[regex(r#"::?([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*"#)]
    #[regex(r#"::?([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]])[^ \t\r\n\f,/#'%";@\^`~()\[\]{}\\]*/([\D&&[^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]][^ \t\r\n\f,#'%";@\^`~()\[\]{}\\]*)?"#)]
    Keyword(&'source str),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    String(&'source str),
}

fn float_printer(num: &f64) -> String {
    if num.is_nan() {
        String::from("##NaN")
    } else if num.is_infinite() {
        if num.is_sign_positive() {
            String::from("##Inf")
        } else {
            String::from("##-Inf")
        }
    } else {
        num.to_string()
    }
}

impl<'source> fmt::Display for Token<'source> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Error => write!(f, "<error>"),
            Self::Whitespace => write!(f, " "),
            Self::Comment => write!(f, "<comment>"),
            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),
            Self::OpenCurly => write!(f, "{{"),
            Self::CloseCurly => write!(f, "}}"),
            Self::OpenSquare => write!(f, "["),
            Self::CloseSquare => write!(f, "]"),
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Char(c) => write!(f, "{}", c),
            Self::Quote => write!(f, "'"),
            Self::SyntaxQuote => write!(f, "`"),
            Self::Unquote => write!(f, "~"),
            Self::UnquoteSplicing => write!(f, "~@"),
            Self::Meta => write!(f, "^"),
            Self::Deref => write!(f, "@"),
            Self::Var => write!(f, "#'"),
            Self::Fn => write!(f, "#("),
            Self::FnArg(a) => write!(f, "{}", a),
            Self::NamespacedMap => write!(f, "#:"),
            Self::Eval => write!(f, "#="),
            Self::ReaderConditional => write!(f, "#?"),
            Self::Regex(re) => write!(f, "{}", re),
            Self::Discard => write!(f, "#_"),
            Self::Set => write!(f, "#{{"),
            Self::TaggedLiteral(t) => write!(f, "#{}", t),
            Self::Integer(i) => write!(f, "{}", i),
            Self::Ratio(r) => write!(f, "{}", r),
            Self::Decimal(d) => write!(f, "{}", float_printer(d)),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::Keyword(k) => write!(f, "{}", k),
            Self::String(s) => write!(f, "{}", s),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use logos::{Lexer, Logos};
//     fn output_printer<'source>(lex: Lexer<'source, Token<'source>>) -> String {
//         lex
//             .clone()
//             .map(|t| match t {
//                 Ok(t) => t.to_string(),
//                 Err(t) => format!("<Error ({} {:?})>", t.0, t.1),
//             })
//             .collect::<Vec<_>>()
//             .join("")
//     }

//     #[test]
//     fn display() {
//         let file = "(def example 1) [+ 1 1.2 3/4 ##NaN] #{:a :b/c ::d/e} {'asdf 'a/b @s #'qwerty}";
//         let lex = Token::lexer(file);
//         assert_eq!(output_printer(lex), String::from(file));
//     }
// }
