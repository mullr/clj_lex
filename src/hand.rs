// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![allow(dead_code, unused_imports)]

use std::{fmt, iter::Peekable, ops::Range, slice::Iter};

use bumpalo::{collections::Vec, vec, Bump};
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
pub struct Nil<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
}

#[derive(Debug, Clone)]
pub struct Boolean<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct Char<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: char,
}

#[derive(Debug, Clone)]
pub struct Integer<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: isize,
}

#[derive(Debug, Clone)]
pub struct Ratio<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub numerator: isize,
    pub denominator: isize,
}

#[derive(Debug, Clone)]
pub struct Decimal<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: f64,
}

#[derive(Debug, Clone)]
pub enum Number<'a> {
    Integer(&'a Integer<'a>),
    Ratio(&'a Ratio<'a>),
    Decimal(&'a Decimal<'a>),
}

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub ns: Option<String>,
    pub name: String,
}

#[derive(Debug, Clone, Default)]
pub struct Keyword<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub ns: Option<String>,
    pub name: String,
}

impl<'a> PartialEq for Keyword<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ns == other.ns && self.name == other.name
    }
}

impl<'a> fmt::Display for Keyword<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ns {
            Some(ns) => write!(f, ":{}/{}", ns, self.name),
            None => write!(f, ":{}", self.name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CljString<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Regex<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct List<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    // TODO i'm not sure we really want vecs of refs here... seems like unnecessary indirection.
    pub value: Vec<'a, &'a Node<'a>>,
}

#[derive(Debug, Clone)]
pub struct Vector<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: Vec<'a, &'a Node<'a>>,
}

#[derive(Debug, Clone)]
pub struct Map<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: Vec<'a, &'a Node<'a>>,
}

#[derive(Debug, Clone)]
pub struct Set<'a> {
    pub span: TextSpan,
    pub meta: Option<&'a Node<'a>>,
    pub value: Vec<'a, &'a Node<'a>>,
}

#[derive(Debug)]
pub enum Node<'a> {
    Nil(&'a Nil<'a>),
    Boolean(&'a Boolean<'a>),
    Char(&'a Char<'a>),
    Number(&'a Number<'a>),
    Symbol(&'a mut Symbol<'a>),
    Keyword(&'a Keyword<'a>),
    CljString(&'a CljString<'a>),
    Regex(&'a Regex<'a>),

    List(&'a mut List<'a>),
    Vector(&'a mut Vector<'a>),
    Map(&'a mut Map<'a>),
    Set(&'a mut Set<'a>),

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

impl<'a> Node<'a> {
    #[inline]
    pub fn nil(arena: &'a Bump, span: TextSpan) -> &'a mut Self {
        arena.alloc(Self::Nil(arena.alloc(Nil { span, meta: None })))
    }

    #[inline]
    pub fn boolean(arena: &'a Bump, span: TextSpan, value: bool) -> &'a mut Self {
        arena.alloc(Self::Boolean(arena.alloc(Boolean {
            span,
            meta: None,
            value,
        })))
    }

    #[inline]
    pub fn char(arena: &'a Bump, span: TextSpan, value: char) -> &'a mut Self {
        arena.alloc(Self::Char(arena.alloc(Char {
            span,
            meta: None,
            value,
        })))
    }

    #[inline]
    pub fn integer(arena: &'a Bump, span: TextSpan, value: isize) -> &'a mut Self {
        arena.alloc(Self::Number(arena.alloc(Number::Integer(arena.alloc(
            Integer {
                span,
                meta: None,
                value,
            },
        )))))
    }

    #[inline]
    pub fn ratio(
        arena: &'a Bump,
        span: TextSpan,
        numerator: isize,
        denominator: isize,
    ) -> &'a mut Self {
        arena.alloc(Self::Number(arena.alloc(Number::Ratio(arena.alloc(
            Ratio {
                span,
                meta: None,
                numerator,
                denominator,
            },
        )))))
    }

    #[inline]
    pub fn decimal(arena: &'a Bump, span: TextSpan, value: f64) -> &'a mut Self {
        arena.alloc(Self::Number(arena.alloc(Number::Decimal(arena.alloc(
            Decimal {
                span,
                meta: None,
                value,
            },
        )))))
    }

    #[inline]
    pub fn string(arena: &'a Bump, span: TextSpan, value: String) -> &'a mut Self {
        arena.alloc(Self::CljString(arena.alloc(CljString {
            span,
            meta: None,
            value,
        })))
    }

    #[inline]
    pub fn symbol(
        arena: &'a Bump,
        span: TextSpan,
        ns: Option<String>,
        name: String,
    ) -> &'a mut Self {
        arena.alloc(Self::Symbol(arena.alloc(Symbol {
            span,
            meta: None,
            ns,
            name,
        })))
    }

    #[inline]
    pub fn keyword(
        arena: &'a Bump,
        span: TextSpan,
        ns: Option<String>,
        name: String,
    ) -> &'a mut Self {
        arena.alloc(Self::Keyword(arena.alloc(Keyword {
            span,
            meta: None,
            ns,
            name,
        })))
    }

    #[inline]
    pub fn list(arena: &'a Bump, span: TextSpan, value: Vec<'a, &'a Node<'a>>) -> &'a mut Self {
        arena.alloc(Self::List(arena.alloc(List {
            span,
            meta: None,
            value,
        })))
    }

    #[inline]
    pub fn map(arena: &'a Bump, span: TextSpan, value: Vec<'a, &'a Node<'a>>) -> &'a mut Self {
        arena.alloc(Self::Map(arena.alloc(Map {
            span,
            meta: None,
            value,
        })))
    }

    #[inline]
    pub fn set(arena: &'a Bump, span: TextSpan, value: Vec<'a, &'a Node<'a>>) -> &'a mut Self {
        arena.alloc(Self::Set(arena.alloc(Set {
            span,
            meta: None,
            value,
        })))
    }
}

pub type TokenSpan<'source> = (Token<'source>, TextSpan);

#[derive(Debug, Clone)]
pub struct Cursor<'source, 'a> {
    position: usize,
    tokens: Vec<'a, TokenSpan<'source>>,
    expected_delimiter: Option<Token<'source>>,
}

impl<'source, 'a> Cursor<'source, 'a> {
    pub fn new(tokens: Vec<'a, TokenSpan<'source>>) -> Self {
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

    pub fn read_ratio(
        &self,
        arena: &'a Bump,
        i: String,
        text_span: TextSpan,
    ) -> Result<&'a mut Node<'a>, ParseError> {
        if let Some((n, d)) = i.split_once('/') {
            let numerator = (*n).parse::<_>().unwrap_or(0);
            let denominator = (*d).parse::<_>().unwrap_or(0);
            Ok(Node::ratio(arena, text_span, numerator, denominator))
        } else {
            Err(ParseError::InvalidNumber(text_span, i.to_string()))
        }
    }

    pub fn read_symbol(
        &self,
        arena: &'a Bump,
        s: String,
        text_span: TextSpan,
    ) -> Result<&'a mut Node<'a>, ParseError> {
        if s.starts_with('/') && s.len() > 1 {
            Err(ParseError::InvalidSymbol(text_span, s))
        } else if s != "/" && s.contains('/') {
            if let Some((ns, name)) = s.split_once('/') {
                Ok(Node::symbol(
                    arena,
                    text_span,
                    Some(ns.to_string()),
                    name.to_string(),
                ))
            } else {
                Err(ParseError::InvalidSymbol(text_span, s))
            }
        } else {
            Ok(Node::symbol(arena, text_span, None, s.to_string()))
        }
    }

    pub fn read_keyword(
        &self,
        arena: &'a Bump,
        s: String,
        text_span: TextSpan,
    ) -> Result<&'a Node<'a>, ParseError> {
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
                    arena,
                    text_span,
                    Some(ns.to_string()),
                    name.to_string(),
                ))
            } else {
                Err(ParseError::InvalidKeyword(text_span, stripped.to_string()))
            }
        } else {
            Ok(Node::keyword(arena, text_span, None, stripped.to_string()))
        }
    }

    pub fn read_list(
        &mut self,

        arena: &'a Bump,
        init_span: TextSpan,
    ) -> Result<(TextSpan, Vec<'a, &'a Node<'a>>), ParseError> {
        let mut span = init_span;
        let mut nodes = Vec::new_in(arena);
        loop {
            match self.next_node(arena) {
                Ok(Node::Delimiter(text_span)) => {
                    span.end = text_span.end;
                    break;
                }
                Ok(node) => nodes.push(node as &_),
                Err(e) => return Err(e),
            }
        }
        Ok((span, nodes))
    }

    pub fn read_meta(
        &mut self,
        arena: &'a Bump,
        text_span: TextSpan,
    ) -> Result<&'a mut Node<'a>, ParseError> {
        match self.next_node(arena) {
            Ok(Node::Eof) => Err(ParseError::UnexpectedEOF(text_span, String::from("^"))),
            Ok(Node::Delimiter(ts)) => Err(ParseError::UnexpectedDelimiter(*ts, String::from("^"))),
            Ok(meta_node) => match self.next_node(arena) {
                Ok(Node::Eof) => Err(ParseError::UnexpectedEOF(text_span, String::from("^"))),
                Ok(node) => {
                    let desugared: &'a mut Node<'a> = match meta_node {
                        Node::Keyword(k) => {
                            let span = k.span;
                            let m = Map {
                                span,
                                meta: None,
                                value: vec![
                                    in arena;
                                    arena.alloc(Node::Keyword(k)) as &_,
                                    arena.alloc(Node::Boolean(arena.alloc(
                                        Boolean {
                                            span,
                                            meta: None,
                                            value: true,
                                        }))),
                                ],
                            };
                            arena.alloc(Node::Map(arena.alloc(m)))
                        }
                        Node::Symbol(s) => {
                            let span = s.span;
                            let m = Map {
                                span,
                                meta: None,
                                value: vec![
                                    in arena;
                                    Node::keyword(arena, text_span, None, TAG.to_string()) as &_,
                                    arena.alloc(Node::Symbol(arena.alloc(s.clone()))),
                                ],
                            };
                            arena.alloc(Node::Map(arena.alloc(m)))
                        }
                        Node::CljString(s) => {
                            let span = s.span;
                            let m = Map {
                                span,
                                meta: None,
                                value: vec![
                                    in arena;
                                    Node::keyword(arena, text_span, None, TAG.to_string()) as &_,
                                    arena.alloc(Node::CljString(s)),
                                ],
                            };
                            arena.alloc(Node::Map(arena.alloc(m)))
                        }
                        m @ Node::Map(_) => m,
                        _ => return Err(ParseError::InvalidMetaToken(text_span, String::from(""))),
                    };
                    let node: &'a mut Node<'a> = node;
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
                        Node::Symbol(c) => {
                            c.meta = Some(desugared);
                            Ok(arena.alloc(Node::Symbol(c)))
                        }
                        Node::List(c) => {
                            c.meta = Some(desugared);
                            Ok(arena.alloc(Node::List(c)))
                        }
                        Node::Vector(c) => {
                            c.meta = Some(desugared);
                            Ok(arena.alloc(Node::Vector(c)))
                        }
                        Node::Map(c) => {
                            c.meta = Some(desugared);
                            Ok(arena.alloc(Node::Map(c)))
                        }
                        Node::Set(c) => {
                            c.meta = Some(desugared);
                            Ok(arena.alloc(Node::Set(c)))
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
        arena: &'a Bump,
        text_span: TextSpan,
        symbol: String,
    ) -> Result<&'a mut Node<'a>, ParseError> {
        match self.next_node(arena) {
            Ok(Node::Eof) => Err(ParseError::UnexpectedEOF(text_span, String::from(""))),
            Ok(Node::Delimiter(ts)) => Err(ParseError::UnexpectedDelimiter(*ts, String::from(""))),
            Ok(node) => Ok(Node::list(
                arena,
                text_span,
                vec![
                    in arena;
                    Node::symbol(arena, text_span, None, symbol) as &_,
                    node
                ],
            )),
            e => e,
        }
    }

    pub fn read_fn(
        &mut self,
        arena: &'a Bump,
        text_span: TextSpan,
    ) -> Result<&'a mut Node<'a>, ParseError> {
        let existing_expected_delimiter = self.expected_delimiter;
        self.expected_delimiter = Some(Token::CloseParen);
        let list = match self.read_list(arena, text_span) {
            Ok((span, nodes)) => {
                let mut nodes = nodes;
                let mut fn_nodes =
                    vec![in arena; Node::symbol(arena, text_span, None, FN.to_string()) as &_];
                fn_nodes.append(&mut nodes);
                Ok(Node::list(arena, span, fn_nodes))
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
    pub fn read_discard(
        &mut self,
        arena: &'a Bump,
        text_span: TextSpan,
    ) -> Result<&'a mut Node<'a>, ParseError> {
        match (self.next_node(arena), self.next_node(arena)) {
            (Err(e), _) | (_, Err(e)) => Err(e),
            (Ok(Node::Eof), _) | (_, Ok(Node::Eof)) => {
                Err(ParseError::UnexpectedEOF(text_span, String::from("#_")))
            }
            (Ok(Node::Delimiter(_)), _) | (_, Ok(Node::Delimiter(_))) => Err(
                ParseError::UnexpectedDelimiter(text_span, String::from("#_")),
            ),
            (Ok(_discard), Ok(mut _next)) => {
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

    pub fn next_node(&mut self, arena: &'a Bump) -> Result<&'a mut Node<'a>, ParseError> {
        let x = { self.pop() };
        if let Some((token, text_span)) = x {
            return match token {
                Token::Nil => Ok(Node::nil(arena, text_span)),
                Token::Boolean(b) => Ok(Node::boolean(arena, text_span, b)),
                Token::Char(c) => Ok(Node::char(arena, text_span, c)),
                Token::Integer(i) => {
                    Ok(Node::integer(arena, text_span, i.parse::<_>().unwrap_or(0)))
                }
                Token::Ratio(i) => {
                    let i = i.to_string();
                    self.read_ratio(arena, i, text_span)
                }
                Token::Decimal(f) => Ok(Node::decimal(arena, text_span, f)),
                Token::String(s) => Ok(Node::string(arena, text_span, (*s).to_string())),
                Token::Symbol(s) => {
                    let s = s.to_string();
                    self.read_symbol(arena, s, text_span)
                }
                Token::Keyword(s) => {
                    let s = s.to_string();
                    self.read_symbol(arena, s, text_span)
                }
                Token::FnArg(s) => {
                    let s = s.to_string();
                    self.read_symbol(arena, s, text_span)
                }
                Token::Meta => self.read_meta(arena, text_span),
                Token::Discard => self.read_discard(arena, text_span),
                Token::ReaderConditional => todo!(),
                Token::TaggedLiteral(_) => todo!(),
                Token::NamespacedMap => todo!(),
                Token::Eval => self.read_wrapper(arena, text_span, EVAL.to_string()),
                Token::Deref => self.read_wrapper(arena, text_span, DEREF.to_string()),
                Token::Quote => self.read_wrapper(arena, text_span, QUOTE.to_string()),
                Token::SyntaxQuote => self.read_wrapper(arena, text_span, SYNTAX_QUOTE.to_string()),
                Token::Unquote => self.read_wrapper(arena, text_span, UNQUOTE.to_string()),
                Token::UnquoteSplicing => {
                    self.read_wrapper(arena, text_span, UNQUOTE_SPLICING.to_string())
                }
                Token::Var => self.read_wrapper(arena, text_span, VAR.to_string()),
                Token::Regex(re) => {
                    let node = arena.alloc(Node::Regex(arena.alloc(Regex {
                        span: text_span,
                        meta: None,
                        value: re.to_string(),
                    })));
                    Ok(Node::list(
                        arena,
                        text_span,
                        vec![
                            in arena;
                            Node::symbol(arena, text_span, None, RE_PATTERN.to_string()) as &_,
                            node
                        ],
                    ))
                }
                Token::OpenParen => {
                    let existing_expected_delimiter = self.expected_delimiter;
                    self.expected_delimiter = Some(Token::CloseParen);
                    let list = match self.read_list(arena, text_span) {
                        Ok((span, nodes)) => Ok(Node::list(arena, span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    list
                }
                Token::CloseParen => {
                    let ret = match self.expected_delimiter {
                        Some(Token::CloseParen) => Ok(arena.alloc(Node::Delimiter(text_span))),
                        _ => Err(ParseError::UnexpectedDelimiter(
                            text_span,
                            String::from(")"),
                        )),
                    };
                    self.expected_delimiter = None;
                    ret
                }
                Token::Fn => self.read_fn(arena, text_span),
                Token::OpenCurly => {
                    let existing_expected_delimiter = self.expected_delimiter;
                    self.expected_delimiter = Some(Token::CloseCurly);
                    let map = match self.read_list(arena, text_span) {
                        Ok((span, nodes)) => Ok(Node::map(arena, span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    map
                }
                Token::CloseCurly => {
                    let ret = match self.expected_delimiter {
                        Some(Token::CloseCurly) => Ok(arena.alloc(Node::Delimiter(text_span))),
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
                    let map = match self.read_list(arena, text_span) {
                        Ok((span, nodes)) => Ok(Node::set(arena, span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    map
                }
                Token::OpenSquare => {
                    let existing_expected_delimiter = self.expected_delimiter;
                    self.expected_delimiter = Some(Token::CloseSquare);
                    let map = match self.read_list(arena, text_span) {
                        Ok((span, nodes)) => Ok(Node::map(arena, span, nodes)),
                        Err(e) => Err(e),
                    };
                    self.expected_delimiter = existing_expected_delimiter;
                    map
                }
                Token::CloseSquare => {
                    let ret = match self.expected_delimiter {
                        Some(Token::CloseSquare) => Ok(arena.alloc(Node::Delimiter(text_span))),
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
        Ok(arena.alloc(Node::Eof))
    }
}

pub fn parse<'a>(
    input: Vec<'a, TokenSpan<'_>>,
    arena: &'a Bump,
) -> Result<Vec<'a, &'a mut Node<'a>>, ParseError> {
    let mut cursor = Cursor::new(input);
    let mut ret = Vec::new_in(arena);
    loop {
        match cursor.next_node(arena) {
            Ok(Node::Eof) => break,
            Ok(node) => ret.push(node),
            Err(e) => return Err(e),
        }
    }
    Ok(ret)
}
