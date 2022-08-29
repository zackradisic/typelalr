use indexmap::IndexSet;

use crate::{
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
    grammar: Grammar<'ast>,
    lexer: Lex,
    action: ActionTable,
    goto: GotoTable<'ast>,
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

        // println!("ACTION: {:#?}", self.action);
        println!("GOTO: {:#?}", self.goto);

        loop {
            let s = stack.last().unwrap().clone();
            println!("S {:?} TOKEN {:?}: {:?}", s, a_idx, a);

            match self.action.try_index((s, a_idx)) {
                Some(Action::Shift(new_state_idx)) => {
                    stack.push(*new_state_idx);
                    next_token(&mut a, &mut a_idx, &mut i, tokens);
                }
                Some(Action::Reduce(prod_idx)) => {
                    let production = self.grammar.get_production(*prod_idx).unwrap();
                    let new_len = stack.len() - production.input_tokens.len();
                    println!("STACK: {:?}", stack);
                    stack.truncate(new_len);
                    println!("STACK: {:?}", stack);

                    let goto_ta = *self
                        .goto
                        .try_index((*stack.last().expect("TOP OF STACK"), production.name))
                        .expect("GOTO");

                    stack.push(goto_ta as u32);

                    println!(
                        "Outputting production: {} {:?}",
                        production.name.as_ref(),
                        prod_idx
                    );
                }
                Some(Action::Accept) => break,
                None => todo!(),
            }
        }

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
            export start S = [ 
                <c1: C> <c2: C> => ("nothing to see here")
            ]

            export C = [
                "c" <c: C> => ("OK"),
                "d" => ("noob")
            ]
        "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);
        println!(
            "TOKENS: {:#?}",
            grammar.tokens().enumerate().collect::<Vec<_>>()
        );
        let lalr = Lalr::new(grammar);

        let noob = lalr.parse("cccccd");
    }
}
