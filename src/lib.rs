// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![allow(unused_imports)]

mod hand;
mod lexer;

use bumpalo::{collections::Vec, Bump};
use logos::Logos;
use std::{error::Error, fs};

use hand::{parse, TokenSpan};
pub use hand::{Node, ParseError};
use lexer::Token;

pub fn parse_clj<'source, 'arena>(
    source: &'source str,
    arena: &'arena Bump,
) -> Result<Vec<'arena, &'arena mut Node<'arena>>, ParseError>
where
    'source: 'arena,
{
    let estimated_capacity = source.len() / 3; // This is a guess...
    let mut tokens = Vec::with_capacity_in(estimated_capacity, arena);

    for (tok, span) in Token::lexer(source).spanned() {
        match tok {
            Ok(tok) => tokens.push((tok, span.into())),
            Err(()) => tokens.push((Token::Error, span.into())),
        }
    }

    parse(tokens, arena)
}
