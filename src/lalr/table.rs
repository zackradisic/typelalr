use std::{
    collections::BTreeMap,
    mem::ManuallyDrop,
    ops::{Deref, Index},
};

use bit_vec::BitVec;
use indexmap::{IndexMap, IndexSet};

use crate::parser::{ast::InputToken, Grammar};

use super::item::{ItemSet, ProductionIdx, TokenIdx};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    /// Shift and stack the next state
    Shift(u32),
    Reduce(ProductionIdx),
    Accept,
}

type TableInner<T> = BTreeMap<usize, BTreeMap<TokenIdx, T>>;

pub struct Table<T>(TableInner<T>);
pub type ActionTable = Table<Action>;
pub type GotoTable = Table<usize>;

impl<T> Table<T> {
    pub fn try_index(&self, (outer_idx, inner_idx): (u32, TokenIdx)) -> Option<&T> {
        match self.0.get(&(outer_idx as usize)) {
            Some(inner) => inner.get(&inner_idx),
            None => None,
        }
    }
}

impl<T> Deref for Table<T> {
    type Target = TableInner<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn make_tables<'ast>(grammar: &Grammar<'ast>) -> (ActionTable, GotoTable, IndexSet<ItemSet>) {
    let lr_items = ItemSet::items(&grammar);

    let (lalr_set, lr_to_lalr) = merge_cores(&lr_items);

    let (action, goto) = make_lalr_tables(&grammar, &lalr_set, &lr_items, &lr_to_lalr);

    (action, goto, lalr_set)
}

pub fn make_lalr_tables<'ast>(
    grammar: &Grammar<'ast>,
    lalr_sets: &IndexSet<ItemSet>,
    lr_sets: &IndexSet<ItemSet>,
    lr_to_lalr: &BTreeMap<usize, usize>,
) -> (ActionTable, GotoTable) {
    let mut action: TableInner<Action> = Default::default();
    let mut goto: TableInner<usize> = Default::default();

    #[inline]
    fn compute_goto<'ast>(
        item_set: &ItemSet,
        a: &InputToken<'ast>,
        grammar: &Grammar<'ast>,
        lr_sets: &IndexSet<ItemSet>,
        lr_to_lalr: &BTreeMap<usize, usize>,
        i: usize,
        lalr_sets: &IndexSet<ItemSet>,
    ) -> Option<usize> {
        let goto_ia = item_set.goto(a, grammar);
        let lr_idx = match lr_sets.get_index_of(&goto_ia) {
            Some(i) => i,
            None => {
                // println!(
                //     "({}) - LR(1) => {:?}: Didn't work: {:#?}",
                //     i,
                //     lalr_sets.get_index_of(&goto_ia),
                //     goto_ia.debug(grammar)
                // );
                // return None;
                // TODO: This might not be correct
                return lalr_sets.get_index_of(&goto_ia);
            }
        };
        lr_to_lalr.get(&lr_idx).cloned()
    }

    for (i, item_set) in lalr_sets.iter().enumerate() {
        for (item, lookaheads) in item_set.iter() {
            let dot_value = item.dot_token(grammar);

            if let Some(dot_value) = dot_value {
                if dot_value.is_terminal() {
                    let token_idx = grammar.get_token_idx(dot_value).unwrap();
                    let goto_idx = compute_goto(
                        item_set, dot_value, grammar, lr_sets, lr_to_lalr, i, lalr_sets,
                    );

                    let j = match goto_idx {
                        Some(j) => j,
                        None => continue,
                    };

                    action
                        .entry(i)
                        .or_insert_with(BTreeMap::new)
                        .insert(token_idx, Action::Shift(j as u32));
                }
                continue;
            }

            // Otherwise we are at the end of the production, ex/ S ⇒  C C⋅
            if item.production_idx() == Grammar::AUGMENTED_START_PRODUCTION_IDX {
                action
                    .entry(i)
                    .or_insert_with(BTreeMap::new)
                    .insert(Grammar::EOF_TOKEN_IDX, Action::Accept);
                continue;
            }

            for lookahead in lookaheads {
                action
                    .entry(i)
                    .or_insert_with(BTreeMap::new)
                    .insert(*lookahead, Action::Reduce(item.production_idx()));
            }
        }
    }

    for (i, state) in lalr_sets.iter().enumerate() {
        for (token_idx, a) in grammar.non_terminals() {
            let goto_ia = state.goto(a, grammar);
            match lalr_sets.get_index_of(&goto_ia) {
                Some(j) => {
                    goto.entry(i)
                        .or_insert_with(BTreeMap::new)
                        .insert(token_idx, j);
                }
                None => (),
            }
        }
    }

    (Table(action), Table(goto))
}

pub fn merge_cores(lalr_sets: &IndexSet<ItemSet>) -> (IndexSet<ItemSet>, BTreeMap<usize, usize>) {
    let mut presence_vec = BitVec::from_elem(lalr_sets.len(), true);
    let mut core_hash_to_index = IndexMap::<u64, Vec<usize>>::new();

    for (i, set) in lalr_sets.iter().enumerate() {
        core_hash_to_index
            .entry(set.core_hash())
            .or_insert_with(Vec::new)
            .push(i);
    }

    let mut lalr_sets = lalr_sets
        .into_iter()
        .map(|set| ManuallyDrop::new(set.clone()))
        .collect();

    let mut lr_to_lalr: BTreeMap<usize, usize> = BTreeMap::new();
    let mut resulting = IndexSet::new();

    for core_hash in core_hash_to_index.keys() {
        let indices = core_hash_to_index.get(core_hash).unwrap();

        for idx in indices {
            lr_to_lalr.insert(*idx, resulting.len());
        }

        let merged_set = merge_sets_with_same_core(indices, &mut lalr_sets, &mut presence_vec);
        resulting.insert(merged_set);
    }

    (resulting, lr_to_lalr)
}

fn merge_sets_with_same_core(
    indices: &Vec<usize>,
    lalr_sets: &mut Vec<ManuallyDrop<ItemSet>>,
    presence_vec: &mut BitVec,
) -> ItemSet {
    let first = indices[0];

    if !presence_vec[first] {
        panic!(
            "Attempting to read item set that has been dropped, index={}",
            first
        );
    }
    presence_vec.set(first, false);

    // SAFETY:
    // We use the presence vec to check if the set in this index is valid
    let mut item_set = unsafe { ManuallyDrop::take(&mut lalr_sets[first]) };

    for i in indices.iter().skip(1) {
        if !presence_vec[*i] {
            panic!(
                "Attempting to read item set that has been dropped, index={}",
                i
            );
        }
        presence_vec.set(*i, false);

        // SAFETY:
        // It's safe to read this
        let other = unsafe { ManuallyDrop::take(&mut lalr_sets[*i]) };
        item_set.union(other);
    }

    item_set
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use bumpalo::Bump;
    use indexmap::IndexMap;

    use crate::{
        lalr::item::{ItemSet, ItemSetDebug, TokenDebug},
        parse_grammar,
        parser::Grammar,
    };

    use super::{make_lalr_tables, merge_cores};

    #[test]
    fn merging() {
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

        let lr_items = ItemSet::items(&grammar);

        let (merged, _) = merge_cores(&lr_items);

        for (i, item_set) in merged.iter().enumerate() {
            println!("{}: {:#?}", i, ItemSetDebug(item_set, &grammar));
        }
    }

    #[test]
    fn tables() {
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

        let lr_items = ItemSet::items(&grammar);

        let (lalr_set, lr_to_lalr) = merge_cores(&lr_items);

        println!("LR ITEMMMSSSS");
        for (i, item_set) in lr_items.iter().enumerate() {
            println!("{} {:#?}", i, ItemSetDebug(item_set, &grammar));
        }
        println!("\n--------\n");
        println!("LALR ITEMMMSSSS");
        for (i, item_set) in lr_items.iter().enumerate() {
            println!("{} {:#?}", i, ItemSetDebug(item_set, &grammar));
        }
        println!("\n--------\n");

        let (action, goto) = make_lalr_tables(&grammar, &lalr_set, &lr_items, &lr_to_lalr);

        for (i, item_set) in action.iter() {
            let item_set = item_set.iter().fold(BTreeMap::new(), |mut acc, (k, v)| {
                acc.insert(TokenDebug(*k, &grammar), *v);
                acc
            });
            println!("{}: {:#?}", i, item_set)
        }

        println!("\n--------\n");

        for (i, item_set) in goto.iter() {
            let item_set = item_set.iter().fold(BTreeMap::new(), |mut acc, (k, v)| {
                acc.insert(TokenDebug(*k, &grammar), *v);
                acc
            });
            println!("{}: {:#?}", i, item_set)
        }
    }
}