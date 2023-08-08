// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![allow(dead_code)]

use chumsky::input::ValueInput;
use chumsky::prelude::*;

use crate::lexer::Token;

// nil -> :nil
// true/false -> :boolean
// \\c -> :char
// 1 -> :number
// :hello -> :keyword
// \"hello\" -> :string
// hello -> :symbol
// {:a :b} -> :map
// #{:a :b} -> :set
// [:a :b] -> :vector
// (1 2 3) -> :list

#[derive(Debug, Clone)]
pub enum Node<'source> {
    Error,

    Nil,
    Boolean(bool),
    Char(char),
    Integer(&'source str),
    Ratio(&'source str),
    Decimal(f64),
    Symbol(&'source str),
    Keyword(&'source str),
    String(&'source str),

    List(Vec<Self>),
    Vector(Vec<Self>),
    Map(Vec<Self>),
    Set(Vec<Self>),
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

pub fn parser<'source, I>() -> impl Parser<
    'source,
    I,
    Vec<Node<'source>>,
    extra::Err<Rich<'source, Token<'source>>>
> where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    let symbol = select! {
        Token::Symbol(s) => Node::Symbol(s),
        Token::FnArg(n) => Node::Symbol(n),
    };
    let keyword = select! { Token::Keyword(k) => Node::Keyword(k) };
    let r#string = select! { Token::String(s) => Node::String(s) };

    let literal = select! {
        Token::Nil => Node::Nil,
        Token::Boolean(b) => Node::Boolean(b),
        Token::Char(c) => Node::Char(c),
        Token::Integer(i) => Node::Integer(i),
        Token::Ratio(r) => Node::Ratio(r),
        Token::Decimal(d) => Node::Decimal(d),
    };

    let atom = choice((literal, symbol, keyword, r#string));

    recursive(|sexp| {
        let list = sexp
            .clone()
            .repeated()
            .collect()
            .delimited_by(just(Token::OpenParen), just(Token::CloseParen))
            .map(Node::List);

        let vector = sexp
            .clone()
            .repeated()
            .collect()
            .delimited_by(just(Token::OpenSquare), just(Token::CloseSquare))
            .map(Node::Vector);

        let map = sexp
            .clone()
            .repeated()
            .collect()
            .delimited_by(just(Token::OpenCurly), just(Token::CloseCurly))
            .map(Node::Map);

        let set = sexp
            .clone()
            .repeated()
            .collect()
            .delimited_by(just(Token::Set), just(Token::CloseCurly))
            .map(Node::Set);

        let deref = just(Token::Deref)
            .ignore_then(sexp.clone())
            .map(|x| Node::List(vec![Node::Symbol(DEREF), x]));

        let eval = just(Token::Eval)
            .ignore_then(sexp.clone())
            .map(|x| Node::List(vec![Node::Symbol(EVAL), x]));

        let r#fn = sexp
            .clone()
            .repeated()
            .collect()
            .delimited_by(just(Token::Fn), just(Token::CloseParen))
            .map(|x| {
                Node::List(vec![Node::Symbol(FN), Node::Vector(vec![]), Node::List(x)])
            });

        let meta = just(Token::Meta)
            .ignore_then(choice((keyword, map.clone(), symbol, r#string)))
            .map(|x| {
                let desugared = match x {
                    Node::Keyword(k) => Node::Map(vec![Node::Keyword(k), Node::Boolean(true)]),
                    Node::Symbol(s) => Node::Map(vec![Node::Keyword(TAG), Node::Symbol(s)]),
                    Node::String(s) => Node::Map(vec![Node::Keyword(TAG), Node::String(s)]),
                    m @ Node::Map(_) => m,
                    _ => unreachable!("Matched wrong meta type: {:#?}", x),
                };
                Node::List(vec![Node::Symbol(META), desugared])
            });

        let quote = just(Token::Quote)
            .ignore_then(sexp.clone())
            .map(|x| Node::List(vec![Node::Symbol(QUOTE), x]));

        let reader_conditional = sexp
            .clone()
            .repeated()
            .collect()
            .delimited_by(just(Token::ReaderConditional), just(Token::CloseParen))
            .map(Node::List);

        let regex = select! { Token::Regex(r) => Node::String(r) }
            .map(|r| Node::List(vec![Node::Symbol(RE_PATTERN), r]));

        let syntax_quote = just(Token::SyntaxQuote)
            .ignore_then(sexp.clone())
            .map(|x| Node::List(vec![Node::Symbol(SYNTAX_QUOTE), x]));

        let unquote = just(Token::Unquote)
            .ignore_then(sexp.clone())
            .map(|x| Node::List(vec![Node::Symbol(UNQUOTE), x]));

        let unquote_splicing = just(Token::UnquoteSplicing)
            .ignore_then(sexp.clone())
            .map(|x| Node::List(vec![Node::Symbol(UNQUOTE_SPLICING), x]));

        let var = just(Token::Var)
            .ignore_then(symbol)
            .map(|x| Node::List(vec![Node::Symbol(VAR), x]));

        let tagged_literal = select! { Token::TaggedLiteral(t) => Node::Symbol(t) }
            .then(sexp.clone())
            .map(|(tag, value)| {
                Node::Map(vec![
                    Node::Symbol(TAGGED_LITERAL),
                    Node::List(vec![tag, value]),
                ])
            });

        choice((
                atom,
                list,
                vector,
                map,
                set,
                deref,
                eval,
                r#fn,
                meta,
                quote,
                reader_conditional,
                regex,
                syntax_quote,
                unquote,
                unquote_splicing,
                var,
                tagged_literal,
        ))
    })
            .repeated()
            .collect::<Vec<_>>()
}
