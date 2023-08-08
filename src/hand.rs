// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![allow(dead_code, unused_imports)]

use std::{fmt, iter::Peekable, ops::Range, slice::Iter};

use logos::Span;

use crate::lexer::Token;

#[derive(Debug, Clone, Copy, Default)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
}

impl TextSpan {
    pub fn new(start: usize, end: usize) -> Self {
        TextSpan { start, end }
    }
}

impl From<Range<usize>> for TextSpan {
    fn from(r: Range<usize>) -> Self {
        TextSpan {
            start: r.start,
            end: r.end,
        }
    }
}

impl From<&Range<usize>> for TextSpan {
    fn from(r: &Range<usize>) -> Self {
        TextSpan {
            start: r.start,
            end: r.end,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Nil {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct Char {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: char,
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: isize,
}

#[derive(Debug, Clone)]
pub struct Ratio {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub numerator: isize,
    pub denominator: isize,
}

#[derive(Debug, Clone)]
pub struct Decimal {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: f64,
}

#[derive(Debug, Clone)]
pub enum Number {
    Integer(Integer),
    Ratio(Ratio),
    Decimal(Decimal),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub ns: Option<String>,
    pub name: String,
}

#[derive(Debug, Clone, Default)]
pub struct Keyword {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub ns: Option<String>,
    pub name: String,
}

impl PartialEq for Keyword {
    fn eq(&self, other: &Self) -> bool {
        self.ns == other.ns && self.name == other.name
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ns {
            Some(ns) => write!(f, ":{}/{}", ns, self.name),
            None => write!(f, ":{}", self.name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CljString {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Regex {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct List {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Vector {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Map {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub span: TextSpan,
    pub meta: Option<Box<Node>>,
    pub value: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Node {
    Nil(Nil),
    Boolean(Boolean),
    Char(Char),
    Number(Number),
    Symbol(Symbol),
    Keyword(Keyword),
    CljString(CljString),
    Regex(Regex),

    List(List),
    Vector(Vector),
    Map(Map),
    Set(Set),

    Delimiter(TextSpan),
    Eof,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedDelimiter(TextSpan, String),
    InvalidNumber(TextSpan, String),
    InvalidToken(TextSpan, String),
    InvalidKeyword(TextSpan, String),
    InvalidSymbol(TextSpan, String),
    InvalidMetaToken(TextSpan, String),
    InvalidMetaTarget(TextSpan, String),
    UnexpectedEOF(TextSpan, String),
}

const DEREF: &str = "splint/deref";
const EVAL: &str = "splint/read-eval";
const FN: &str = "splint/fn";
const META: &str = "splint/meta";
const QUOTE: &str = "splint/quote";
const RE_PATTERN: &str = "splint/re-pattern";
const SYNTAX_QUOTE: &str = "splint/syntax-quote";
const UNQUOTE: &str = "splint/unquote";
const UNQUOTE_SPLICING: &str = "splint/unquote-splicing";
const VAR: &str = "splint/var";

const TAG: &str = "tag";
const TAGGED_LITERAL: &str = "splint/tagged-literal";

impl Node {
    #[inline]
    pub fn nil(span: TextSpan) -> Self {
        Self::Nil(Nil { span, meta: None })
    }

    #[inline]
    pub fn boolean(span: TextSpan, value: bool) -> Self {
        Self::Boolean(Boolean {
            span,
            meta: None,
            value,
        })
    }

    #[inline]
    pub fn char(span: TextSpan, value: char) -> Self {
        Self::Char(Char {
            span,
            meta: None,
            value,
        })
    }

    #[inline]
    pub fn integer(span: TextSpan, value: isize) -> Self {
        Self::Number(Number::Integer(Integer {
            span,
            meta: None,
            value,
        }))
    }

    #[inline]
    pub fn ratio(span: TextSpan, numerator: isize, denominator: isize) -> Self {
        Self::Number(Number::Ratio(Ratio {
            span,
            meta: None,
            numerator,
            denominator,
        }))
    }

    #[inline]
    pub fn decimal(span: TextSpan, value: f64) -> Self {
        Self::Number(Number::Decimal(Decimal {
            span,
            meta: None,
            value,
        }))
    }

    #[inline]
    pub fn string(span: TextSpan, value: String) -> Self {
        Self::CljString(CljString {
            span,
            meta: None,
            value,
        })
    }

    #[inline]
    pub fn symbol(span: TextSpan, ns: Option<String>, name: String) -> Self {
        Self::Symbol(Symbol {
            span,
            meta: None,
            ns,
            name,
        })
    }

    #[inline]
    pub fn keyword(span: TextSpan, ns: Option<String>, name: String) -> Self {
        Self::Keyword(Keyword {
            span,
            meta: None,
            ns,
            name,
        })
    }

    #[inline]
    pub fn list(span: TextSpan, value: Vec<Node>) -> Self {
        Self::List(List {
            span,
            meta: None,
            value,
        })
    }

    #[inline]
    pub fn map(span: TextSpan, value: Vec<Node>) -> Self {
        Self::Map(Map {
            span,
            meta: None,
            value,
        })
    }

    #[inline]
    pub fn set(span: TextSpan, value: Vec<Node>) -> Self {
        Self::Set(Set {
            span,
            meta: None,
            value,
        })
    }
}

pub type TokenSpan<'source> = (Token<'source>, TextSpan);

#[derive(Debug, Clone)]
pub struct Cursor<'source> {
    position: usize,
    tokens: Vec<TokenSpan<'source>>,
    expected_delimiter: Option<Token<'source>>,
}

impl<'source> Cursor<'source> {
    pub fn new(tokens: Vec<TokenSpan<'source>>) -> Self {
        Self {
            position: 0,
            tokens,
            expected_delimiter: None,
        }
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn is_eof(&self) -> bool {
        self.position == self.tokens.len()
    }

    pub fn peek(&self) -> Option<TokenSpan> {
        if self.position < self.tokens.len() {
            return Some(self.tokens[self.position]);
        }
        None
    }

    pub fn pop(&mut self) -> Option<TokenSpan> {
        if self.position < self.tokens.len() {
            let token = self.tokens[self.position];
            self.position += 1;
            return Some(token);
        }
        None
    }

    pub fn read_ratio(&self, i: String, text_span: TextSpan) -> Result<Node, ParseError> {
        if let Some((n, d)) = i.split_once('/') {
            let numerator = (*n).parse::<_>().unwrap_or(0);
            let denominator = (*d).parse::<_>().unwrap_or(0);
            Ok(Node::ratio(text_span, numerator, denominator))
        } else {
            Err(ParseError::InvalidNumber(text_span, i.to_string()))
        }
    }

    pub fn read_symbol(&self, s: String, text_span: TextSpan) -> Result<Node, ParseError> {
        if s.starts_with('/') && s.len() > 1 {
            Err(ParseError::InvalidSymbol(text_span, s))
        } else if s != "/" && s.contains('/') {
            if let Some((ns, name)) = s.split_once('/') {
                Ok(Node::symbol(
                    text_span,
                    Some(ns.to_string()),
                    name.to_string(),
                ))
            } else {
                Err(ParseError::InvalidSymbol(text_span, s))
            }
        } else {
            Ok(Node::symbol(text_span, None, s.to_string()))
        }
    }

    pub fn read_keyword(&self, s: String, text_span: TextSpan) -> Result<Node, ParseError> {
        let auto_resolve = s.starts_with("::");
        let stripped = if auto_resolve {
            s.strip_prefix("::").unwrap()
        } else {
            s.strip_prefix(':').unwrap()
        };

        if stripped.starts_with('/') && stripped.len() > 1 {
            Err(ParseError::InvalidKeyword(text_span, stripped.to_string()))
        } else if stripped != "/" && stripped.contains('/') {
            if let Some((ns, name)) = stripped.split_once('/') {
                Ok(Node::keyword(
                    text_span,
                    Some(ns.to_string()),
                    name.to_string(),
                ))
            } else {
                Err(ParseError::InvalidKeyword(text_span, stripped.to_string()))
            }
        } else {
            Ok(Node::keyword(text_span, None, stripped.to_string()))
        }
    }

    pub fn read_list(&mut self, init_span: TextSpan) -> Result<(TextSpan, Vec<Node>), ParseError> {
        let mut span = init_span;
        let mut nodes = vec![];
        loop {
            match self.next_node() {
                Ok(Node::Delimiter(text_span)) => {
                    span.end = text_span.end;
                    break;
                }
                Ok(node) => nodes.push(node),
                Err(e) => return Err(e),
            }
        }
        Ok((span, nodes))
    }

    pub fn read_meta(&mut self, text_span: TextSpan) -> Result<Node, ParseError> {
        match self.next_node() {
            Ok(Node::Eof) => Err(ParseError::UnexpectedEOF(text_span, String::from("^"))),
            Ok(Node::Delimiter(ts)) => Err(ParseError::UnexpectedDelimiter(ts, String::from("^"))),
            Ok(meta_node) => match self.next_node() {
                Ok(Node::Eof) => Err(ParseError::UnexpectedEOF(text_span, String::from("^"))),
                Ok(node) => {
                    let desugared = match meta_node {
                        Node::Keyword(k) => {
                            let span = k.span;
                            let m = Map {
                                span,
                                meta: None,
                                value: vec![
                                    Node::Keyword(k.clone()),
                                    Node::Boolean(Boolean {
                                        span,
                                        meta: None,
                                        value: true,
                                    }),
                                ],
                            };
                            Node::Map(m)
                        }
                        Node::Symbol(s) => {
                            let span = s.span;
                            let m = Map {
                                span,
                                meta: None,
                                value: vec![
                                    Node::keyword(text_span, None, TAG.to_string()),
                                    Node::Symbol(s.clone()),
                                ],
                            };
                            Node::Map(m)
                        }
                        Node::CljString(s) => {
                            let span = s.span;
                            let m = Map {
                                span,
                                meta: None,
                                value: vec![
                                    Node::keyword(text_span, None, TAG.to_string()),
                                    Node::CljString(s.clone()),
                                ],
                            };
                            Node::Map(m)
                        }
                        m @ Node::Map(_) => m,
                        _ => return Err(ParseError::InvalidMetaToken(text_span, String::from(""))),
                    };
                    match node {
                        Node::Nil(_)
                        | Node::Boolean(_)
                        | Node::Char(_)
                        | Node::Number(_)
                        | Node::Keyword(_)
                        | Node::CljString(_)
                        | Node::Regex(_) => {
                            Err(ParseError::InvalidMetaTarget(text_span, String::from("")))
                        }
                        Node::Symbol(mut c) => {
                            c.meta = Some(Box::new(desugared));
                            Ok(Node::Symbol(c))
                        }
                        Node::List(mut c) => {
                            c.meta = Some(Box::new(desugared));
                            Ok(Node::List(c))
                        }
                        Node::Vector(mut c) => {
                            c.meta = Some(Box::new(desugared));
                            Ok(Node::Vector(c))
                        }
                        Node::Map(mut c) => {
                            c.meta = Some(Box::new(desugared));
                            Ok(Node::Map(c))
                        }
                        Node::Set(mut c) => {
                            c.meta = Some(Box::new(desugared));
                            Ok(Node::Set(c))
                        }
                        _ => Err(ParseError::InvalidToken(
                            text_span,
                            format!("Invalid token: {:?}", node),
                        )),
                    }
                }
                e => e,
            },
            e => e,
        }
    }

    pub fn read_wrapper(
        &mut self,
        text_span: TextSpan,
        symbol: String,
    ) -> Result<Node, ParseError> {
        match self.next_node() {
            Ok(Node::Eof) => Err(ParseError::UnexpectedEOF(text_span, String::from(""))),
            Ok(Node::Delimiter(ts)) => Err(ParseError::UnexpectedDelimiter(ts, String::from(""))),
            Ok(node) => Ok(Node::list(
                text_span,
                vec![Node::symbol(text_span, None, symbol), node],
            )),
            e => e,
        }
    }

    pub fn read_fn(&mut self, text_span: TextSpan) -> Result<Node, ParseError> {
        let existing_expected_delimiter = self.expected_delimiter;
        self.expected_delimiter = Some(Token::CloseParen);
        let list = match self.read_list(text_span) {
            Ok((span, nodes)) => {
                let mut nodes = nodes;
                let mut fn_nodes = vec![Node::symbol(text_span, None, FN.to_string())];
                fn_nodes.append(&mut nodes);
                Ok(Node::list(span, fn_nodes))
            }
            Err(e) => Err(e),
        };
        self.expected_delimiter = existing_expected_delimiter;
        list
    }

    // :uneval (fn [{:keys [uneval next]}]
    //     (cond
    //      (identical? uneval :splint/disable)
    //      (vary-meta next assoc :splint/disable true)
    //      (and (seqable? (:splint/disable uneval))
    //           (seq (:splint/disable uneval)))
    //      (vary-meta next assoc :splint/disable (seq (:splint/disable uneval)))
    //      :else
    //      next))
    pub fn read_discard(&mut self, text_span: TextSpan) -> Result<Node, ParseError> {
        match (self.next_node(), self.next_node()) {
            (Err(e), _) | (_, Err(e)) => Err(e),
            (Ok(Node::Eof), _) | (_, Ok(Node::Eof)) => {
                Err(ParseError::UnexpectedEOF(text_span, String::from("#_")))
            }
            (Ok(Node::Delimiter(_)), _) | (_, Ok(Node::Delimiter(_))) => Err(
                ParseError::UnexpectedDelimiter(text_span, String::from("#_")),
            ),
            (Ok(discard), Ok(mut next)) => {
                todo!()
                // if let Node::Keyword(k) = discard {
                //     if k == (Keyword {
                //         ns: Some(String::from("splint")),
                //         name: String::from("disable"),
                //         ..Default::default()
                //     }) {
                //     } else {
                //         Ok(next)
                //     }
                // } else if let Node::Vector(v) = discard {
                // } else {
                //     Ok(next)
                // }
            }
        }
    }

    pub fn next_node(&mut self) -> Result<Node, ParseError> {
        if let Some((token, text_span)) = self.pop() {
            return match token {
                Token::Nil => Ok(Node::nil(text_span)),
                Token::Boolean(b) => Ok(Node::boolean(text_span, b)),
                Token::Char(c) => Ok(Node::char(text_span, c)),
                Token::Integer(i) => Ok(Node::integer(text_span, i.parse::<_>().unwrap_or(0))),
                Token::Ratio(i) => {
                    let i = i.to_string();
                    self.read_ratio(i, text_span)
                }
                Token::Decimal(f) => Ok(Node::decimal(text_span, f)),
                Token::String(s) => Ok(Node::string(text_span, (*s).to_string())),
                Token::Symbol(s) => {
                    let s = s.to_string();
                    self.read_symbol(s, text_span)
                }
                Token::Keyword(s) => {
                    let s = s.to_string();
                    self.read_symbol(s, text_span)
                }
                Token::FnArg(s) => {
                    let s = s.to_string();
                    self.read_symbol(s, text_span)
                }
                Token::Meta => self.read_meta(text_span),
                Token::Discard => self.read_discard(text_span),
                Token::ReaderConditional => todo!(),
                Token::TaggedLiteral(_) => todo!(),
                Token::NamespacedMap => todo!(),
                Token::Eval => self.read_wrapper(text_span, EVAL.to_string()),
                Token::Deref => self.read_wrapper(text_span, DEREF.to_string()),
                Token::Quote => self.read_wrapper(text_span, QUOTE.to_string()),
                Token::SyntaxQuote => self.read_wrapper(text_span, SYNTAX_QUOTE.to_string()),
                Token::Unquote => self.read_wrapper(text_span, UNQUOTE.to_string()),
                Token::UnquoteSplicing => {
                    self.read_wrapper(text_span, UNQUOTE_SPLICING.to_string())
                }
                Token::Var => self.read_wrapper(text_span, VAR.to_string()),
                Token::Regex(re) => {
                    let node = Node::Regex(Regex {
                        span: text_span,
                        meta: None,
                        value: re.to_string(),
                    });
                    Ok(Node::list(
                        text_span,
                        vec![Node::symbol(text_span, None, RE_PATTERN.to_string()), node],
                    ))
                }
                Token::OpenParen => {
                    let existing_expected_delimiter = self.expected_delimiter;
                    self.expected_delimiter = Some(Token::CloseParen);
                    let list = match self.read_list(text_span) {
                        Ok((span, nodes)) => Ok(Node::list(span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    list
                }
                Token::CloseParen => {
                    let ret = match self.expected_delimiter {
                        Some(Token::CloseParen) => Ok(Node::Delimiter(text_span)),
                        _ => Err(ParseError::UnexpectedDelimiter(
                            text_span,
                            String::from(")"),
                        )),
                    };
                    self.expected_delimiter = None;
                    ret
                }
                Token::Fn => self.read_fn(text_span),
                Token::OpenCurly => {
                    let existing_expected_delimiter = self.expected_delimiter;
                    self.expected_delimiter = Some(Token::CloseCurly);
                    let map = match self.read_list(text_span) {
                        Ok((span, nodes)) => Ok(Node::map(span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    map
                }
                Token::CloseCurly => {
                    let ret = match self.expected_delimiter {
                        Some(Token::CloseCurly) => Ok(Node::Delimiter(text_span)),
                        _ => Err(ParseError::UnexpectedDelimiter(
                            text_span,
                            String::from("}"),
                        )),
                    };
                    self.expected_delimiter = None;
                    ret
                }
                Token::Set => {
                    let existing_expected_delimiter = self.expected_delimiter;
                    self.expected_delimiter = Some(Token::CloseCurly);
                    let map = match self.read_list(text_span) {
                        Ok((span, nodes)) => Ok(Node::set(span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    map
                }
                Token::OpenSquare => {
                    let existing_expected_delimiter = self.expected_delimiter;
                    self.expected_delimiter = Some(Token::CloseSquare);
                    let map = match self.read_list(text_span) {
                        Ok((span, nodes)) => Ok(Node::map(span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    map
                }
                Token::CloseSquare => {
                    let ret = match self.expected_delimiter {
                        Some(Token::CloseSquare) => Ok(Node::Delimiter(text_span)),
                        _ => Err(ParseError::UnexpectedDelimiter(
                            text_span,
                            String::from("]"),
                        )),
                    };
                    self.expected_delimiter = None;
                    ret
                }
                Token::Comment => todo!(),
                Token::Error => todo!(),
                Token::Whitespace => todo!(),
                // token => Err(ParseError::InvalidToken(text_span, token.to_string())),
            };
        }
        Ok(Node::Eof)
    }
}

pub fn parse(input: Vec<TokenSpan>) -> Result<Vec<Node>, ParseError> {
    let mut cursor = Cursor::new(input);
    let mut ret = vec![];
    loop {
        match cursor.next_node() {
            Ok(Node::Eof) => break,
            Ok(node) => ret.push(node),
            Err(e) => return Err(e),
        }
    }
    Ok(ret)
}
