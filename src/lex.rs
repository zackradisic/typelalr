use regex_syntax::hir::Hir as RegexHir;
use std::collections::{btree_map::Entry, BTreeMap};

use crate::{
    dfa::DFA,
    nfa::{StateIdx, NFA},
    option_usize::OptionUsize,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    name: String,
}

#[derive(Debug, Clone)]
pub struct Lex {
    dfa: DFA,
    tokens: BTreeMap<StateIdx, Token>,
    accepting_dfa_to_nfa: BTreeMap<StateIdx, Vec<StateIdx>>,
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
        }

        let dfa = DFA::from_nfa(lex_nfa);

        Self {
            accepting_dfa_to_nfa: dfa.accepting_states(),
            dfa,
            tokens: tokens_map,
        }
    }

    pub fn new(tokens: Vec<(StateIdx, Token)>, dfa: DFA) -> Self {
        Self {
            accepting_dfa_to_nfa: dfa.accepting_states(),
            dfa,
            // tokens: tokens.into_iter().collect(),
            tokens: tokens.into_iter().map(|(k, tok)| (k, tok)).collect(),
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

        let mut last_accepting_state: Option<(usize, StateIdx)> = None;

        while forward < chars.len() {
            let c = chars[forward];

            let matching_edge = match self.dfa.edge_iter(state_idx) {
                Some(mut edge_iter) => edge_iter.find(|edge| edge.label.matches(c)),
                None => None,
            };

            match matching_edge {
                Some(edge) => {
                    if let Some(nfa_set) = self.accepting_dfa_to_nfa.get(&edge.to) {
                        last_accepting_state = Some((forward, *nfa_set.first().unwrap()));
                    }
                    state_idx = edge.to;
                    forward += 1;
                }
                None => {
                    if let Some((last_accepting_idx, last_accepting_state_idx)) =
                        last_accepting_state
                    {
                        lexeme_begin = last_accepting_idx + 1;
                        forward = last_accepting_idx + 1;
                        // state_idx = last_accepting_state_idx;
                        state_idx = StateIdx(0);
                        tokens.push(self.tokens.get(&last_accepting_state_idx).unwrap().clone());
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

        if let Some((_last_accepting_idx, last_accepting_state_idx)) = last_accepting_state {
            tokens.push(self.tokens.get(&last_accepting_state_idx).unwrap().clone());
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

    fn tok<S: Into<String>>(s: S) -> Token {
        Token { name: s.into() }
    }

    #[test]
    fn lex() {
        let tokens = vec![new_token("number", "[0-9]+"), new_token("+", "\\+")];
        let lexer = Lex::from_tokens(tokens);

        let toks = lexer.lex("420 + 420");
        let [num1, plus, num2]: &[Token; 3] = toks.as_slice().try_into().unwrap();

        assert_eq!(num1, &tok("number"));
        assert_eq!(plus, &tok("+"));
        assert_eq!(num2, &tok("number"));
    }

    #[test]
    fn precedence_longest() {
        let tokens = vec![new_token("abbb", "abbb"), new_token("a*b+", "a*b+")];
        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("abbbbbbb");

        assert_eq!(toks[0], tok("a*b+"));
    }

    #[test]
    fn precedence_tie() {
        let mut tokens = vec![
            new_token("a*b+", "a*b+"),
            new_token("a*b*", "a*b*"),
            new_token("abbb", "abbb"),
        ];
        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("abbb");
        assert_eq!(toks[0], tok("a*b+"));

        // "a*b*" is first now
        tokens.swap(0, 1);
        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("abbb");
        assert_eq!(toks[0], tok("a*b*"));

        // "abbb" is first now
        tokens.swap(0, 2);
        let lexer = Lex::from_tokens(tokens);
        let toks = lexer.lex("abbb");
        assert_eq!(toks[0], tok("abbb"))
    }
}
