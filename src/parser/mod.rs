pub mod ast;
pub mod grammar;

mod support;

use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

use bumpalo::Bump;
pub use support::*;

use crate::lalr::item::{ProductionIdx, TokenIdx};

use self::ast::{Ident, InputSymbol, NamedSymbol};

/// Augmented grammar
#[derive(Debug)]
pub struct Grammar<'ast> {
    production_name_map: BTreeMap<Ident<'ast>, Vec<ProductionIdx>>,
    productions: Vec<Production<'ast>>,
    tokens: Vec<InputSymbol<'ast>>,
    augmented_start_name: Ident<'ast>,
}

#[derive(Debug)]
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
    pub input_tokens: &'ast [InputSymbol<'ast>],
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
            input_tokens: bump
                .alloc(vec![InputSymbol::Named(bump.alloc(NamedSymbol {
                    name: Some(augmented_start_name),
                    ty: bump.alloc(InputSymbol::NonTerminal(start_symbol.name)),
                }))])
                .as_slice(),
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

        let mut token_set: BTreeSet<InputSymbol> = BTreeSet::from_iter([InputSymbol::Eof]);
        for prod in productions.iter() {
            for input_token in prod.input_tokens.iter() {
                token_set.insert(input_token.clone());
            }
        }
        let mut tokens = Vec::with_capacity(1 + token_set.len());
        tokens.push(InputSymbol::Eof);
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

    pub fn tokens(&self) -> std::slice::Iter<InputSymbol<'ast>> {
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
        std::iter::Enumerate<std::slice::Iter<InputSymbol<'ast>>>,
        for<'a> fn((usize, &'a InputSymbol<'ast>)) -> Option<(TokenIdx, &'a InputSymbol<'ast>)>,
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

    pub fn get_token(&self, idx: TokenIdx) -> Option<&InputSymbol> {
        self.tokens.get(idx.0 as usize)
    }

    pub fn get_token_idx(&self, input_token: &InputSymbol) -> Option<TokenIdx> {
        self.tokens.iter().enumerate().find_map(|(idx, tok)| {
            if tok.eq(input_token) {
                Some(TokenIdx(idx as u32))
            } else {
                None
            }
        })
    }
}

impl<'ast> Production<'ast> {
    pub fn from_ast(
        prod: &'ast ast::Production<'ast>,
        prod_body: &'ast ast::ProductionBody<'ast>,
    ) -> Self {
        Self {
            name: prod.name,
            input_tokens: &prod_body.input_tokens,
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
