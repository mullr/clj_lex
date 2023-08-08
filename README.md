# Clojure Linter in Rust???

I wrote [Splint](https://github.com/NoahTheDuke/splint) and I am incredibly proud of it.
However, I'm reaching the limit to the speed I can achieve with it, and I feel like
trying something new! We'll see how this goes.

## Structure of the code

main.rs is obvious. lexer.rs uses [Logos](https://github.com/maciejhirsz/logos) to lex
clojure text into discrete tokens. parser.rs uses
[chumsky](https://github.com/zesterer/chumsky) but I abandoned that because it wasn't
fast enough for my taste. hand.rs is my hand-rolled parser that I'm still working on.
It's ugly and long and has lots of small inefficiencies, but I like it so far and I'm
excited to push it further.

I don't think I have the right set of enum variants for `Node`, and I'm still unsure
about how I'm doing `.meta`.
