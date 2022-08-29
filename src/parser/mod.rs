pub mod ast;
pub mod grammar;

mod support;

use regex_syntax::hir::Hir as RegexHir;
use regex_syntax::Parser as RegexParser;
use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

use bumpalo::Bump;
pub use support::*;

use crate::{
    lalr::item::{ProductionIdx, TokenIdx},
    lex::lex::TokenDef,
};

use self::ast::Ident;

/// Augmented grammar
#[derive(Debug)]
pub struct Grammar<'ast> {
    production_name_map: BTreeMap<Ident<'ast>, Vec<ProductionIdx>>,
    productions: Vec<Production<'ast>>,
    tokens: Vec<Symbol<'ast>>,
    augmented_start_name: Ident<'ast>,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
pub enum Symbol<'ast> {
    StrLit(&'ast str),
    NonTerminal(Ident<'ast>),
    Regex(&'ast str),
    Epsilon,
    Eof,
}

#[derive(Debug)]
pub struct Production<'ast> {
    pub name: Ident<'ast>,
    pub input_tokens: Vec<Symbol<'ast>>,
    pub mapping_fn: Option<&'ast str>,
}

pub struct ProductionIter<'ast> {
    production_indices: &'ast [ProductionIdx],
    productions: &'ast [Production<'ast>],
    idx: usize,
}

impl<'ast> Grammar<'ast> {
    // S' -> S
    pub const AUGMENTED_START_PRODUCTION_IDX: ProductionIdx = ProductionIdx(0);
    pub const EOF_TOKEN_IDX: TokenIdx = TokenIdx(0);

    pub fn grammar_from_ast_productions(
        bump: &'ast Bump,
        ast_productions: &'ast [ast::Production<'ast>],
    ) -> Self {
        let mut production_name_map: BTreeMap<Ident, Vec<ProductionIdx>> = BTreeMap::new();
        let mut productions = Vec::new();

        let start_symbol = ast_productions.iter().find(|prod| prod.is_start).unwrap();
        let augmented_start_name = Ident(bump.alloc(format!("{}'", start_symbol.name)));

        production_name_map.insert(
            augmented_start_name,
            vec![Self::AUGMENTED_START_PRODUCTION_IDX],
        );
        productions.push(Production {
            name: augmented_start_name,
            input_tokens: vec![Symbol::NonTerminal(start_symbol.name)],
            mapping_fn: None,
        });

        for ast_prod in ast_productions {
            for body in &ast_prod.bodies {
                let production_idx = ProductionIdx(productions.len() as u32);
                productions.push(Production::from_ast(ast_prod, body));

                match production_name_map.entry(ast_prod.name) {
                    Entry::Vacant(entry) => {
                        entry.insert(vec![production_idx]);
                    }
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().push(production_idx);
                    }
                }
            }
        }

        let mut token_set: BTreeSet<Symbol> = BTreeSet::from_iter([Symbol::Eof]);
        for prod in productions.iter() {
            for input_token in prod.input_tokens.iter() {
                token_set.insert(input_token.clone());
            }
        }
        let mut tokens = Vec::with_capacity(1 + token_set.len());
        tokens.push(Symbol::Eof);
        tokens.extend(token_set);

        Self {
            production_name_map,
            productions,
            tokens,
            augmented_start_name,
        }
    }

    pub fn iter_productions(&self) -> std::slice::Iter<Production> {
        self.productions.iter()
    }

    pub fn production_iter(&'ast self, name: Ident<'ast>) -> ProductionIter<'ast> {
        ProductionIter {
            production_indices: self.production_name_map.get(&name).unwrap(),
            productions: &self.productions,
            idx: 0,
        }
    }

    pub fn tokens(&self) -> std::slice::Iter<Symbol<'ast>> {
        self.tokens.iter()
    }

    // pub fn non_terminals(
    //     &self,
    // ) -> std::iter::FilterMap<
    //     std::iter::Enumerate<std::slice::Iter<Symbol<'ast>>>,
    //     for<'a> fn((usize, &'a Symbol<'ast>)) -> Option<(TokenIdx, &'a Ident<'ast>)>,
    // > {
    //     self.tokens.iter().enumerate().filter_map(|(i, tok)| {
    //         tok.as_non_terminal()
    //             .map(|non_terminal_name| (TokenIdx(i as u32), non_terminal_name))
    //     })
    // }

    pub fn non_terminals(
        &self,
    ) -> std::iter::FilterMap<
        std::iter::Enumerate<std::slice::Iter<Symbol<'ast>>>,
        for<'a> fn((usize, &'a Symbol<'ast>)) -> Option<(TokenIdx, &'a Symbol<'ast>)>,
    > {
        self.tokens.iter().enumerate().filter_map(|(i, tok)| {
            if !tok.is_terminal() {
                Some((TokenIdx(i as u32), tok))
            } else {
                None
            }
        })
    }

    pub fn get_production_indices_by_name(&self, name: Ident<'ast>) -> Option<&Vec<ProductionIdx>> {
        self.production_name_map.get(&name)
    }

    pub fn get_production(&self, idx: ProductionIdx) -> Option<&Production> {
        self.productions.get(idx.0 as usize)
    }

    pub fn get_token(&self, idx: TokenIdx) -> Option<&Symbol> {
        self.tokens.get(idx.0 as usize)
    }

    pub fn get_token_idx(&self, input_token: &Symbol) -> Option<TokenIdx> {
        self.tokens.iter().enumerate().find_map(|(idx, tok)| {
            if tok.eq(input_token) {
                Some(TokenIdx(idx as u32))
            } else {
                None
            }
        })
    }
}

impl<'ast> Symbol<'ast> {
    pub fn to_token_def_with_regex(&self, token_idx: TokenIdx) -> Option<(TokenDef, RegexHir)> {
        let token_idx = token_idx.0;
        match self {
            Symbol::StrLit(str_lit) => Some((
                TokenDef {
                    name: str_lit.to_string(),
                    with_val: false,
                    token_idx: Some(token_idx),
                },
                RegexParser::new().parse(str_lit).unwrap(),
            )),
            Symbol::Regex(regex) => Some((
                TokenDef {
                    name: regex.to_string(),
                    with_val: true,
                    token_idx: Some(token_idx),
                },
                RegexParser::new().parse(regex).unwrap(),
            )),
            Symbol::Epsilon => None,
            Symbol::Eof => None,
            Symbol::NonTerminal(_) => None,
        }
    }

    pub fn as_non_terminal(&self) -> Option<&Ident<'ast>> {
        match self {
            Symbol::NonTerminal(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            Symbol::Eof | Symbol::Epsilon | Symbol::Regex(_) | Symbol::StrLit(_) => true,
            Symbol::NonTerminal(_) => false,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            Symbol::Epsilon => 0,
            Symbol::StrLit(_) => 1,
            Symbol::NonTerminal(_) => 2,
            Symbol::Regex(_) => 3,
            Symbol::Eof => 4,
        }
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        match (self, other) {
            (Self::Epsilon, _) => Less,
            (Self::StrLit(a), Self::StrLit(b)) => a.cmp(b),
            (Self::Regex(a), Self::Regex(b)) => a.cmp(b),
            (Self::NonTerminal(a), Self::NonTerminal(b)) => a.cmp(b),
            _ => self.precedence().cmp(&other.precedence()),
        }
    }
}

impl<'ast> std::fmt::Display for Symbol<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::StrLit(str_lit) => f.write_str(str_lit),
            Symbol::NonTerminal(non_terminal) => f.write_str(non_terminal.as_ref()),
            Symbol::Regex(regex) => f.write_str(regex),
            Symbol::Epsilon => f.write_str("ε"),
            Symbol::Eof => f.write_str("﹩"),
        }
    }
}

impl<'ast> Production<'ast> {
    pub fn from_ast(
        prod: &'ast ast::Production<'ast>,
        prod_body: &'ast ast::ProductionBody<'ast>,
    ) -> Self {
        Self {
            name: prod.name,
            input_tokens: prod_body
                .input_tokens
                .iter()
                .map(|sym| sym.lower())
                .collect(),
            mapping_fn: Some(prod_body.mapping_fn),
        }
    }
}

impl<'ast> Iterator for ProductionIter<'ast> {
    type Item = (ProductionIdx, &'ast Production<'ast>);

    fn next(&mut self) -> Option<Self::Item> {
        let next_production_idx = self.production_indices.get(self.idx)?;
        self.idx += 1;

        self.productions
            .get(next_production_idx.0 as usize)
            .map(|production| (*next_production_idx, production))
    }
}
