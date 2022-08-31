use regex_syntax::hir::Hir as RegexHir;
use std::collections::BTreeMap;

use crate::{lalr::item::TokenIdx, option_usize::OptionUsize, parser::Grammar};

use super::{
    dfa::DFA,
    nfa::{StateIdx, NFA},
};

#[derive(Debug, Clone, PartialEq)]
pub struct TokenDef {
    pub name: String,
    pub with_val: bool,
    pub token_idx: Option<u32>,
}

impl TokenDef {
    pub fn to_token(&self) -> Token {
        Token {
            name: self.name.clone(),
            val: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    name: String,
    val: Option<Box<String>>,
}

impl Token {
    pub fn new(name: String) -> Self {
        Token { name, val: None }
    }

    pub fn new_with_val<S: Into<String>>(name: String, val: S) -> Self {
        Token {
            name,
            val: Some(Box::new(val.into())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lex {
    pub(crate) dfa: DFA,
    pub(crate) tokens: BTreeMap<StateIdx, TokenDef>,
    pub(crate) accepting_dfa_to_nfa: BTreeMap<StateIdx, Vec<StateIdx>>,
}

impl Lex {
    pub fn from_grammar<'ast>(grammar: &Grammar<'ast>) -> Self {
        let mut tokens = Vec::new();

        for (idx, tok) in grammar.tokens().enumerate() {
            let token_def_with_regex = match tok.to_token_def_with_regex(TokenIdx(idx as u32)) {
                Some(t) => t,
                None => continue,
            };

            tokens.push(token_def_with_regex);
        }

        Self::from_tokens(tokens)
    }

    pub fn from_tokens(tokens: Vec<(TokenDef, RegexHir)>) -> Self {
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

    pub fn new(tokens: Vec<(StateIdx, TokenDef)>, dfa: DFA) -> Self {
        Self {
            accepting_dfa_to_nfa: dfa.accepting_states(),
            dfa,
            // tokens: tokens.into_iter().collect(),
            tokens: tokens.into_iter().map(|(k, tok)| (k, tok)).collect(),
        }
    }

    pub fn convert_tokens(&self, tokens: Vec<Token>) -> Vec<(Token, TokenIdx)> {
        let def_map = self
            .tokens
            .iter()
            .fold(BTreeMap::new(), |mut map, (_, def)| {
                map.insert(def.name.as_str(), def);
                map
            });
        tokens
            .into_iter()
            .map(|token| self.token_with_idx(token, &def_map))
            .chain(std::iter::once((Token{ name: "EOF".into(), val: None}, Grammar::EOF_TOKEN_IDX)))
            .collect()
    }

    fn token_with_idx(
        &self,
        token: Token,
        def_map: &BTreeMap<&str, &TokenDef>,
    ) -> (Token, TokenIdx) {
        let def = def_map.get(token.name.as_str()).unwrap();
        (token, TokenIdx(def.token_idx.unwrap()))
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
            println!("LEXEME_BEGIN: {} FORWARD: {}", lexeme_begin, last_accepting_idx);
                        let tok = self.make_tok(
                            input,
                            lexeme_begin,
                            last_accepting_idx,
                            last_accepting_state_idx,
                        );
                        tokens.push(tok);

                        lexeme_begin = last_accepting_idx + 1;
                        forward = last_accepting_idx + 1;
                        state_idx = StateIdx(0);

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
            println!("LEXEME_BEGIN: {} FORWARD: {}", lexeme_begin, _last_accepting_idx);
            tokens.push(self.make_tok(
                input,
                lexeme_begin,
                _last_accepting_idx,
                last_accepting_state_idx,
            ));
        }

        tokens
    }

    fn make_tok(
        &self,
        input: &str,
        start: usize,
        end: usize,
        last_accepting_state_idx: StateIdx,
    ) -> Token {
        let ret = match self.tokens.get(&last_accepting_state_idx) {
            Some(TokenDef {
                with_val: true,
                name,
                ..
            }) => {
                // let val = char_vec.iter().collect::<String>();
                let val = input[start..(end + 1)].to_string();
                Token::new_with_val(name.clone(), val)
            }
            Some(TokenDef {
                with_val: false,
                name,
                ..
            }) => Token::new(name.clone()),
            None => panic!(),
        };
        // char_vec.clear();
        ret
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
    use super::{Lex, Token, TokenDef};
    use regex_syntax::{hir::Hir as RegexHir, Parser};

    fn new_token(name: &str, regex: &str) -> (TokenDef, RegexHir) {
        (
            TokenDef {
                name: name.into(),
                with_val: false,
                token_idx: None,
            },
            Parser::new().parse(regex).unwrap(),
        )
    }

    fn new_token_val(name: &str, regex: &str) -> (TokenDef, RegexHir) {
        (
            TokenDef {
                name: name.into(),
                with_val: true,
                token_idx: None,
            },
            Parser::new().parse(regex).unwrap(),
        )
    }

    fn tok<S: Into<String>>(s: S) -> Token {
        Token {
            name: s.into(),
            val: None,
        }
    }

    fn tok_val<S: Into<String>>(s: S, val: impl Into<String>) -> Token {
        Token {
            name: s.into(),
            val: Some(Box::new(val.into())),
        }
    }

    #[test]
    fn sanity() {
        let tokens = vec![new_token_val("c", "c"), new_token_val("d", "d")];
        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("cdd");
        println!("TOKS: {:#?}", toks);
    }

    #[test]
    fn string() {
        let tokens = vec![new_token_val("string", "\".*\"")];

        let lexer = Lex::from_tokens(tokens.clone());
        println!("LEX: {:#?}", lexer);
        let toks = lexer.lex("\"hello\" noob");

        println!("TOKS: {:#?}", toks);
        // assert!(dfa.test("\"noice\""));
        // assert!(dfa.test("\"\""));

        // assert!(!dfa.test("dfslkdjf"));
        // assert!(!dfa.test("\""));
        // assert!(!dfa.test("\"dslkfjlsdkjf"));
    }

    #[test]
    fn lex() {
        let tokens = vec![new_token("number", "[0-9]+"), new_token("+", "\\+")];
        let lexer = Lex::from_tokens(tokens);

        let toks = lexer.lex("420 + 69");
        let [num1, plus, num2]: &[Token; 3] = toks.as_slice().try_into().unwrap();

        assert_eq!(num1, &tok("number"));
        assert_eq!(plus, &tok("+"));
        assert_eq!(num2, &tok("number"));
    }

    #[test]
    fn precedence_longest2() {
        let tokens = vec![new_token("bb", "bb"), new_token("bbb", "bbb")];
        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("bbbbb");

        assert_eq!(toks[0], tok("bbb"));
        assert_eq!(toks[1], tok("bb"));
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

    #[test]
    fn capture() {
        let tokens = vec![
            new_token_val("number", "[0-9]*"),
            new_token_val("op", "\\+|-"),
        ];

        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("420 + 69");

        let [_420, plus, _69]: &[Token; 3] = toks.as_slice().try_into().unwrap();

        assert_eq!(_420, &tok_val("number", "420"));
        assert_eq!(plus, &tok_val("op", "+"));
        assert_eq!(_69, &tok_val("number", "69"));
    }

    #[test]
    fn capture2() {
        let tokens = vec![new_token_val("number", "[0-9]*"), new_token("+", "\\+")];

        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("420 + 69");

        let [_420, plus, _69]: &[Token; 3] = toks.as_slice().try_into().unwrap();

        assert_eq!(_420, &tok_val("number", "420"));
        assert_eq!(plus, &tok("+"));
        assert_eq!(_69, &tok_val("number", "69"));
    }

    #[test]
    fn simple_lang() {
        let tokens = vec![
            new_token("let", "let"),
            new_token_val("number", "[0-9]*"),
            new_token("+", "\\+"),
            new_token_val("identifier", "[A-Za-z_][A-Za-z_0-9]*"),
            new_token("=", "="),
            new_token(";", ";"),
        ];

        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("let foo = 420;");

        let [_let, foo, eq, _420, semi]: &[Token; 5] = toks.as_slice().try_into().unwrap();

        assert_eq!(_let, &tok("let"));
        assert_eq!(foo, &tok_val("identifier", "foo"));
        assert_eq!(eq, &tok("="));
        assert_eq!(_420, &tok_val("number", "420"));
        assert_eq!(semi, &tok(";"));
    }

    #[test]
    fn lisp() {
        let tokens = vec![
            new_token("(", "\\("),
            new_token(")", "\\)"),
            new_token_val("symbol", "[\\+\\-\\*A-Za-z_\\.-][A-Za-z_\\.-0-9]*"),
            new_token_val("string", "\"[^\"]*\""),
            new_token_val("int", "[1-9][0-9]*"),
            new_token_val("float", "[+-]?([0-9]*[.])?[0-9]+"),
        ];

        let lexer = Lex::from_tokens(tokens.clone());
        let toks = lexer.lex("(print \"Stuff\" \"HI\" (+ 400 20) (- 70.0 1.0))");

        let [
            lparen1, print, stuff, hi, lparen2, plus, n400, n20, rparen2, 
            lparen3, minus, f70, f1, rparen3, rparen1
        ]: &[Token; 15] = toks.as_slice().try_into().unwrap();

        for paren in [lparen1, lparen2, lparen3] {
            assert_eq!(paren, &tok("("));
        }
        for paren in [rparen1, rparen2, rparen3] {
            assert_eq!(paren, &tok(")"));
        }

        assert_eq!(print, &tok_val("symbol", "print"));
        assert_eq!(stuff, &tok_val("string", "\"Stuff\""));
        assert_eq!(hi, &tok_val("string", "\"HI\""));
        assert_eq!(plus, &tok_val("symbol", "+"));
        assert_eq!(n400, &tok_val("int", "400"));
        assert_eq!(n20, &tok_val("int", "20"));
        assert_eq!(minus, &tok_val("symbol", "-"));
        assert_eq!(f70, &tok_val("float", "70.0"));
        assert_eq!(f1, &tok_val("float", "1.0"));
    }
}
