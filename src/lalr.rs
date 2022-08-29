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
    goto: GotoTable,
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

        // self.parse_impl(&tokens)
        todo!()
    }

    fn parse_impl(&self, tokens: &[(Token, TokenIdx)]) -> String {
        let ret = String::new();
        let mut stack = vec![0u32];

        let mut i = 0;
        let tok = &tokens[i];
        let mut a = &tok.0;
        let mut a_idx = tok.1;

        loop {
            let s = stack.last().unwrap().clone();
            match self.action.try_index((s, a_idx)) {
                Some(Action::Shift(new_state_idx)) => {
                    stack.push(*new_state_idx);
                    i += 1;
                    a = &tokens[i].0;
                    a_idx = tokens[i].1;
                }
                Some(Action::Reduce(prod_idx)) => {
                    let production = self.grammar.get_production(*prod_idx).unwrap();
                    let new_len = stack.len() - production.input_tokens.len();
                    stack.truncate(new_len);
                }
                Some(Action::Accept) => break,
                None => todo!(),
            }
        }

        ret
    }
}
