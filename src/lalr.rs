use std::{collections::HashMap, fs};

use bumpalo::Bump;
use indexmap::IndexSet;

use crate::{
    lalr::item::TokenDebug,
    lex::lex::{Lex, Token},
    parser::Grammar,
};

use self::{
    item::{ItemSet, TokenIdx},
    table::{Action, ActionTable, GotoTable},
};

pub mod first;
pub mod item;
pub mod table;

pub struct Lalr<'ast> {
    pub(crate) grammar: Grammar<'ast>,
    pub(crate) lexer: Lex,
    pub(crate) action: ActionTable,
    pub(crate) goto: GotoTable<'ast>,
    states: IndexSet<ItemSet>,
}

impl<'ast> Lalr<'ast> {
    pub fn new(grammar: Grammar<'ast>) -> Self {
        let lexer = Lex::from_grammar(&grammar);

        let (action, goto, states) = table::make_tables(&grammar);

        Self {
            grammar,
            lexer,
            action,
            goto,
            states,
        }
    }

    pub fn parse(&self, input: &str) -> String {
        let tokens = self.lexer.lex(input);
        let tokens_with_idx = self.lexer.convert_tokens(tokens);

        self.parse_impl(&tokens_with_idx)
    }

    fn parse_impl(&self, tokens: &[(Token, TokenIdx)]) -> String {
        let ret = String::new();
        let mut stack = vec![0u32];
        let mut token_stack = Vec::<&Token>::new();

        let mut i = 0;
        let tok = &tokens[i];
        let mut a = &tok.0;
        let mut a_idx = tok.1;

        #[inline]
        fn next_token<'a>(
            a: &mut &'a Token,
            a_idx: &mut TokenIdx,
            i: &mut usize,
            tokens: &'a [(Token, TokenIdx)],
        ) {
            *i += 1;
            if *i < tokens.len() {
                *a = &tokens[*i].0;
                *a_idx = tokens[*i].1;
            }
        }

        println!(
            "PRODUCTIONS: {:#?}",
            self.grammar
                .iter_productions()
                .enumerate()
                .collect::<Vec<_>>()
        );
        println!(
            "TOKENS: {:#?}",
            self.grammar.tokens().enumerate().collect::<Vec<_>>()
        );
        println!("ACTION: {:#?}", self.action);
        println!("GOTO: {:#?}", self.goto);

        let mut loop_count = 0;
        loop {
            loop_count += 1;
            let s = *stack.last().expect("Stack is empty");
            println!("STACK: {:?}", stack);
            println!("S={:?} TOKEN {:?}: {:?} i={}", s, a_idx, a, i);

            match self.action.try_index((s, a_idx)) {
                Some(Action::Shift(new_state_idx)) => {
                    stack.push(*new_state_idx);

                    println!("  shifting {}", *new_state_idx);

                    token_stack.push(a);
                    next_token(&mut a, &mut a_idx, &mut i, tokens);
                }
                Some(Action::Reduce(prod_idx)) => {
                    let production = self
                        .grammar
                        .get_production(*prod_idx)
                        .expect("Production is undefined");
                    let new_len = stack.len() - production.input_tokens.len();

                    // println!(
                    //     "  STACK LEN {} PROD LEN {} NEW LEN {}",
                    //     stack.len(),
                    //     production.input_tokens.len(),
                    //     new_len
                    // );

                    stack.truncate(new_len);
                    println!("  reducing by {:?}", production.debug(&self.grammar));

                    // let (left, right) = token_stack.split_at(new_len);
                    // println!("RIGHT: {:#?}\nLEFT: {:#?}", right, left);

                    // println!("  truncated STACK: {:?}", stack);

                    // println!("  new top: {:?}", stack.last().expect("Stack is empty"));

                    let goto_ta = *self
                        .goto
                        .try_index((*stack.last().expect("Stack is empty"), production.name))
                        .expect("Goto is empty");

                    stack.push(goto_ta as u32);
                    println!("  pushing ({}) => STACK: {:?}", goto_ta, stack);

                    // println!(
                    //     "Outputting production: {} ({:?})",
                    //     production.name.as_ref(),
                    //     prod_idx
                    // );
                }
                Some(Action::Accept) => {
                    println!("Accepting!");
                    break;
                }
                None => panic!(
                    "Invalid token: {:?}\nValid: {:?}",
                    a,
                    self.action.valid(s).map(|valid_keys| valid_keys
                        .map(|token_idx| TokenDebug(*token_idx, &self.grammar))
                        .collect::<Vec<_>>())
                ),
            }
        }

        println!("LOOP COUNT: {}", loop_count);
        ret
    }
}

#[cfg(test)]
mod test {
    use bumpalo::Bump;

    use crate::{parse_grammar, parser::Grammar};

    use super::Lalr;

    #[test]
    fn basic() {
        let grammar_str = r#"
            export start S: ({ kind: "S", left: C, right: C }) = [ 
                <c1: C> <c2: C> => ({ kind: "S", left: c1, right: c2 })
            ]

            export C: ({ kind: "C", values: string[] }) = [
                <c1: "c"> <c2: C> => ({ kind: "C", values: [...c2['values']] }),
                <d: "d"> => ({ kind: "C", values: [d]})
            ]
        "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        // println!(
        //     "TOKENS: {:#?}",
        //     grammar.tokens().enumerate().collect::<Vec<_>>()
        // );

        let lalr = Lalr::new(grammar);

        let _noob = lalr.parse("cdd");
    }

    #[test]
    fn basic_ops() {
        let grammar_str = r#"
            export start Exprs: () = [
                Expr Expr => ("nice"),
                Expr => ("also nice")
            ]

            export Expr: () = [ 
                <int: Int> => ({ kind: "int", int })
            ]

            export Int: () = [
                r"[1-9][0-9]*" => ("lol")
            ]
        "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        // println!(
        //     "PRODUCTION NAMES: {:#?}",
        //     grammar
        //         .iter_productions()
        //         .map(|production| production)
        //         .collect::<Vec<_>>()
        // );

        // println!(
        //     "TOKENS: {:#?}",
        //     grammar.tokens().enumerate().collect::<Vec<_>>()
        // );

        let lalr = Lalr::new(grammar);

        let _noob = lalr.parse("42 420");
    }

    #[test]
    fn basic_ops2() {
        let grammar_str = r#"
            export start Exprs: () = [
                Expr Exprs => ("nice"),
                Expr => ("also nice")
            ]

            export Expr: () = [ 
                <int: Int> => ({ kind: "int", int }),
                <str: String> => ({ kind: "str", str }),
            ]

            export Int: () = [
                r"[1-9][0-9]*" => ("lol")
            ]

            export String: () = [
                r"'[^']*'" => ("lol")
            ]
        "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        // println!(
        //     "TOKENS: {:#?}",
        //     grammar.tokens().enumerate().collect::<Vec<_>>()
        // );

        let lalr = Lalr::new(grammar);

        let _noob = lalr.parse("42 420 69 9001");
    }

    #[test]
    fn lisp() {
        let grammar_str = r#"
            export start SExpr: () = [
                "(" <exprs: Exprs> ")" => ("LMAO")
            ]

            export Exprs: () = [
                Expr Exprs => ("nice"),
                Expr => ("also nice")
            ]

            export Expr: () = [ 
                <int: Int> => ({ kind: "int", int }),
                <str: Symbol> => ({ kind: "str", str }),
            ]

            export Int: () = [
                r"[1-9][0-9]*" => ("lol")
            ]

            export Symbol: () = [
                r"[^() ]*" => ("lol")
            ]
        "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        // println!(
        //     "TOKENS: {:#?}",
        //     grammar.tokens().enumerate().collect::<Vec<_>>()
        // );

        let lalr = Lalr::new(grammar);

        let _noob = lalr.parse("(add 35 34)");
    }
}
