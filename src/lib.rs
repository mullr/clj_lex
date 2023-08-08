// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![allow(unused_imports)]

mod hand;
mod lexer;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::input::Stream;
use chumsky::prelude::*;
use logos::Logos;
use std::{error::Error, fs};

use hand::{parse, TokenSpan};
pub use hand::{Node, ParseError};
use lexer::Token;

pub fn parse_clj(source: &str) -> Result<Vec<Node>, ParseError> {
    let tokens = Token::lexer(source)
        .spanned()
        .map::<TokenSpan, _>(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        })
        .collect::<Vec<_>>();

    parse(tokens)
}
