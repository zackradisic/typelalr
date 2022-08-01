use regex_syntax::hir::Hir as RegexHir;
use std::collections::{btree_map::Entry, BTreeMap};

use crate::{
    dfa::DFA,
    nfa::{StateIdx, NFA},
    option_usize::OptionUsize,
};

#[derive(Debug, Clone)]
pub struct Token {
    name: String,
}

#[derive(Debug, Clone)]
pub struct Lex {
    dfa: DFA,
    tokens: BTreeMap<StateIdx, Vec<Token>>,
}

impl Lex {
    pub fn from_tokens(tokens: Vec<(Token, RegexHir)>) -> Self {
        let mut tokens_with_nfa: Vec<_> = tokens
            .into_iter()
            .map(|(tok, regex)| (tok, NFA::from_regex(&regex)))
            .collect();

        let mut lex_nfa = NFA::lex_nfa_template(
            &tokens_with_nfa
                .iter()
                .map(|(_, nfa)| (nfa.states.len(), nfa.edges.len()))
                .collect::<Vec<_>>(),
        );

        let mut tokens_map = BTreeMap::new();

        let mut state_offset = 1;
        let mut edge_offset = tokens_with_nfa.len();

        for (tok, nfa) in tokens_with_nfa.iter_mut() {
            for accepting_state_idx in nfa.accepting_states() {
                println!("LOL: {:?} {:?}", accepting_state_idx, state_offset);
                // TODO: This clone seems costly
                tokens_map.insert(StateIdx(accepting_state_idx.0 + state_offset), tok.clone());
            }
            nfa.shift_state_and_edges(state_offset, edge_offset);
            state_offset += nfa.states.len();
            edge_offset += nfa.edges.len();
        }

        for (_, nfa) in tokens_with_nfa.iter() {
            for state in &nfa.states {
                lex_nfa.states.push(state.clone());
            }
            for edges in &nfa.edges {
                lex_nfa.edges.push(edges.clone());
            }
            // lex_nfa.states.extend_from_slice(&nfa.states);
            // lex_nfa.edges.extend_from_slice(&nfa.edges);
        }
        // for i in 0..tokens_with_nfa.len() {
        //     lex_nfa
        // }
        // println!("NFA: {:#?}\n", lex_nfa);

        let (dfa, nfa_to_dfa_map) = DFA::from_nfa(lex_nfa);
        let mut final_tokens = BTreeMap::new();
        for (idx, tok) in tokens_map.iter() {
            let dfa_idx = nfa_to_dfa_map.get(&idx).unwrap();
            println!("IDX {:?} DFA IDX {:?} TOK {:?}", idx, dfa_idx, tok);
            match final_tokens.entry(*dfa_idx) {
                Entry::Vacant(entry) => {
                    entry.insert(vec![tok.clone()]);
                }
                Entry::Occupied(mut entry) => {
                    entry.get_mut().push(tok.clone());
                }
            }
            // final_tokens.insert(*dfa_idx, tok.clone());
        }

        Self {
            dfa,
            tokens: final_tokens,
        }
    }

    pub fn new(tokens: Vec<(StateIdx, Token)>, dfa: DFA) -> Self {
        Self {
            dfa,
            // tokens: tokens.into_iter().collect(),
            tokens: tokens.into_iter().map(|(k, tok)| (k, vec![tok])).collect(),
        }
    }

    #[inline]
    pub fn is_whitespace(c: char) -> bool {
        match c {
            '\n' | ' ' => true,
            _ => false,
        }
    }

    pub fn lex(&self, input: &str) -> Vec<Token> {
        let chars: Vec<char> = input.chars().collect();
        let mut tokens = vec![];
        let mut state_idx = StateIdx(0);

        let mut lexeme_begin: usize = 0;
        let mut forward: usize = 0;

        let mut last_accepting_state: Option<(usize, StateIdx, StateIdx)> = None;

        while forward < chars.len() {
            let c = chars[forward];
            println!(
                "C {} F {} S {:?} B {:?}",
                c, forward, state_idx, last_accepting_state
            );

            let matching_edge = match self.dfa.edge_iter(state_idx) {
                Some(mut edge_iter) => edge_iter.find(|edge| edge.label.matches(c)),
                None => None,
            };

            match matching_edge {
                Some(edge) => {
                    if self.dfa.states[edge.to.0].is_accept {
                        last_accepting_state = Some((forward, edge.to, edge.from));
                    }
                    state_idx = edge.to;
                    forward += 1;
                }
                None => {
                    if let Some((last_accepting_idx, last_accepting_state_idx, back)) =
                        last_accepting_state
                    {
                        lexeme_begin = last_accepting_idx + 1;
                        forward = last_accepting_idx + 1;
                        // state_idx = last_accepting_state_idx;
                        state_idx = StateIdx(0);
                        tokens.push(
                            self.tokens
                                .get(&last_accepting_state_idx)
                                .unwrap()
                                .first()
                                .unwrap()
                                .clone(),
                        );
                        last_accepting_state = None;
                    } else if Self::is_whitespace(c) && forward == lexeme_begin {
                        forward += 1;
                        lexeme_begin += 1;
                    } else {
                        break;
                    }
                }
            }
        }

        if let Some((last_accepting_idx, last_accepting_state_idx, back)) = last_accepting_state {
            // tokens.push(self.tokens.get(&last_accepting_state_idx).unwrap().clone());
            println!("LAST: {:?} {:?}", last_accepting_state_idx, self.tokens);

            tokens.push(
                self.tokens
                    .get(&last_accepting_state_idx)
                    .unwrap()
                    .first()
                    .unwrap()
                    .clone(),
            );
        }

        tokens
    }

    fn matches(&self, state_idx: StateIdx, c: char) -> Result<OptionUsize, ()> {
        if let Some(mut edge_iter) = self.dfa.edge_iter(state_idx) {
            match edge_iter.find(|edge| edge.label.matches(c)) {
                Some(edge) => return Ok(OptionUsize::some(edge.to.0)),
                None => {
                    if Self::is_whitespace(c) {
                        return Ok(OptionUsize::none());
                    }
                    return Err(());
                }
            }
        }
        Ok(OptionUsize::none())
    }
}

#[cfg(test)]
mod test {
    use super::{Lex, Token};
    use regex_syntax::{hir::Hir as RegexHir, Parser};

    fn new_token(name: &str, regex: &str) -> (Token, RegexHir) {
        (
            Token { name: name.into() },
            Parser::new().parse(regex).unwrap(),
        )
    }

    #[test]
    fn lex() {
        let tokens = vec![new_token("number", "[0-9]+"), new_token("+", "\\+")];
        let lexer = Lex::from_tokens(tokens);

        println!("LEXER {:#?}", lexer);
        println!("\n\nDFA {:#?}", lexer.dfa);
        let toks = lexer.lex("420 + 420");
        println!("toks {:?}", toks);
    }

    #[test]
    fn lex2() {
        let tokens = vec![
            new_token("a", "a"),
            new_token("abb", "abb"),
            new_token("a*b+", "a*b+"),
        ];
        let lexer = Lex::from_tokens(tokens);

        println!("LEXER {:#?}", lexer);
        println!("\n\nDFA {:#?}", lexer.dfa);
        let toks = lexer.lex("abbb");
        println!("toks {:?}", toks);
    }
}
