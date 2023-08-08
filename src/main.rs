// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![allow(unused_imports)]

mod hand;
mod lexer;
mod parser;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::input::Stream;
use chumsky::prelude::*;
use logos::Logos;
use std::{error::Error, fs};

use crate::hand::{parse, TokenSpan};
use crate::lexer::Token;
use crate::parser::parser;

fn main() -> Result<(), Box<dyn Error>> {
    let core_clj: String = fs::read_to_string("core.clj")?.parse()?;
    let start = std::time::Instant::now();
    let tokens = Token::lexer(&core_clj)
        .spanned()
        .map::<TokenSpan, _>(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        })
        .collect::<Vec<_>>();

    let parsed_file = parse(tokens);
    println!("total time: {:?}", start.elapsed());
    if parsed_file.is_ok() {
        println!("parsed whole file!");
    }
    // println!("file: {:?}", parsed_file);

    // match parsed_file.into_result() {
    //     // If parsing was successful, attempt to evaluate the s-expression
    //     Ok(sexpr) => println!("{:?}", sexpr.iter().take(10).collect::<Vec<_>>()),
    //     // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
    //     // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
    //     // with Rust's built-in `Display` trait, but it's a little crude
    //     Err(errs) => {
    //         println!("errors");
    //         for err in errs {
    //             Report::build(ReportKind::Error, (), err.span().start)
    //                 .with_code(3)
    //                 .with_message(err.to_string())
    //                 .with_label(
    //                     Label::new(err.span().into_range())
    //                         .with_message(err.reason().to_string())
    //                         .with_color(Color::Red),
    //                 )
    //                 .finish()
    //                 .eprint(Source::from(&core_clj))
    //                 .unwrap();
    //         }
    //     }
    // }

    Ok(())
}
