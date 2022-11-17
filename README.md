## **typelalr** - a very stupid experiment

Type-level lexer/parser generators for Typescript. Aka a really bad idea.

## What is this?

This is a Rust program that generates type-level tokenizers and parsers for Typescript. The lexers support regex and the parsers work on LALR(1) grammars. In theory this will allow you to generate lexers/parsers for any LALR(1) grammar that can run in Typescript's type system, for example one that resembles a programming language (see the Lisp example below).

## Why

There are some wacky projects implemented entirely within Typescript's type system like: [Hypescript](https://github.com/ronami/HypeScript) (a minimal implementation of Typescript) or [ts-sql](https://github.com/codemix/ts-sql) (an SQL database). All of these require lexing and parsing some grammar (ex/ Typescript code or SQL).

If you want to build something similar you'll need to write a lexer or parser by hand, which is no fun, lexer/parser generators alleviate you from that problem. So I whipped out the Dragon Book and started hacking on this very stupid idea.

## How does it work?

You write your grammar in a Typescript-esque language like so:

```typescript
// basic_lisp.tlalr

export start SExpr: ({ kind: "sexpr", exprs: Expr[] }) = [
    "(" <exprs: Exprs> ")" => ({ kind: "sexpr", exprs })
]

export Exprs: (Expr[]) = [
    <e1: Expr> <e2: Exprs> => ([e1, ...e2]),
    <e: Expr> => ([e])
]

export Expr: () = [
    <int: Int> => ({ kind: "expr", value: int }),
    <sym: Symbol> => ({ kind: "expr", value: sym }),
]

export Int: ({ kind: "int", int: number }) = [
    <int: r"[1-9][0-9]*"> => ({ kind: "int", int: parseInt(int) })
]

export Symbol: ({ kind: "sym", sym: string }) = [
    <sym: r"[^() ]*"> => ({ kind: "sym", sym })
]
```

Running the Rust program will generate Typescript types containing the DFA state machine to lex the input, and the parsing tables and parser to parse the tokens. Then you can import it and use it like so:

```typescript
import { Lex } from "./lexer.gen.ts";
import { Parse } from "./parser.gen.ts";

// Tokens result:
// [
//   { kind: "(", value: "(" },
//   { kind: "sym", value: "add" },
//   { kind: "[1-9][0-9]+]", value: "34" },
//   { kind: "[1-9][0-9]+]", value: "35" },
//   { kind: ")", value: ")" },
// ];
type tokens = Lex<"(add 34 35)">;

// Parse result:
// {
//     kind: "sexpr",
//     exprs: [
//         { kind: "sym", sym: "add" },
//         { kind: "int", int: "34" },
//         { kind: "int", int: "35" },
//     ]
// }
type ast = Parse<tokens>;
```

You can try out this lisp example by cloning the repo and opening the [ts/parse.ts](ts/parse.ts) file.

## Status

This was an experiment I made several months ago, the above Lisp example works perfectly. However with longer/more complex inputs Typescript hits the
recursion limit, even though the type-level code is all tail calls so it should be tail call optimized.

I tried to see if there was a bug in the generated code causing an infinite loop, but it's very difficult to debug type-level code. The best you can do is return template literal strings as an adhoc way of
achieving "printf debugging", but for some reason the string placeholders print out garbage when the recursion level is high 🤷‍♀️

Laying the project to rest now, maybe later I'll come back to it and fix the type-level code using a modified version of the Typescript compiler altered to enable debugging.
