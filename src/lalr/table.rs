use std::{
    collections::{btree_map::Entry, BTreeMap},
    mem::ManuallyDrop,
    ops::Deref,
};

use bit_vec::BitVec;
use indexmap::{IndexMap, IndexSet};

use crate::parser::{ast::Ident, Grammar, Symbol};

use super::item::{ItemSet, ProductionIdx, TokenIdx};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    /// Shift and stack the next state
    Shift(u32),
    Reduce(ProductionIdx),
    Accept,
}

type TableInner<K, V> = BTreeMap<usize, BTreeMap<K, V>>;

#[derive(Debug)]
pub struct Table<K, V>(TableInner<K, V>);

pub type ActionTable = Table<TokenIdx, Action>;
pub type GotoTable<'ast> = Table<Ident<'ast>, usize>;

impl<K: Ord, V> Table<K, V> {
    pub fn empty() -> Self {
        Self(BTreeMap::new())
    }
    pub fn try_index(&self, (outer_idx, inner_idx): (u32, K)) -> Option<&V> {
        match self.0.get(&(outer_idx as usize)) {
            Some(inner) => inner.get(&inner_idx),
            None => None,
        }
    }

    pub fn valid(&self, outer_idx: u32) -> Option<std::collections::btree_map::Keys<K, V>> {
        self.0.get(&(outer_idx as usize)).map(BTreeMap::keys)
    }

    pub fn entry(&mut self, (outer_idx, inner_idx): (u32, K)) -> Entry<K, V> {
        self.0
            .entry(outer_idx as usize)
            .or_insert_with(BTreeMap::new)
            .entry(inner_idx)
    }

    pub fn insert(&mut self, (outer_idx, inner_idx): (u32, K), val: V) -> Option<V> {
        match self.entry((outer_idx, inner_idx)) {
            Entry::Vacant(entry) => {
                entry.insert(val);
                None
            }
            Entry::Occupied(mut entry) => Some(entry.insert(val)),
        }
    }

    pub fn iter(&self) -> std::collections::btree_map::Iter<usize, BTreeMap<K, V>> {
        self.0.iter()
    }
}

impl<K, V> Deref for Table<K, V> {
    type Target = TableInner<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn make_tables<'ast>(
    grammar: &Grammar<'ast>,
) -> (ActionTable, GotoTable<'ast>, IndexSet<ItemSet>) {
    // Create the LR(1) set of items so we can create our parsing tables
    let lr_items = ItemSet::items(grammar);

    // Merge LR(1) sets with same cores
    let (lalr_set, lr_to_lalr) = merge_cores(&lr_items);

    let (action, goto) = make_lalr_tables(grammar, &lalr_set, &lr_items, &lr_to_lalr);

    (action, goto, lalr_set)
}

pub fn make_lalr_tables<'ast>(
    grammar: &Grammar<'ast>,
    lalr_sets: &IndexSet<ItemSet>,
    lr_sets: &IndexSet<ItemSet>,
    lr_to_lalr: &BTreeMap<usize, usize>,
) -> (ActionTable, GotoTable<'ast>) {
    let mut action: ActionTable = ActionTable::empty();
    let mut goto: GotoTable = GotoTable::empty();

    #[inline]
    fn compute_goto<'ast>(
        item_set: &ItemSet,
        a: &Symbol<'ast>,
        grammar: &Grammar<'ast>,
        lr_sets: &IndexSet<ItemSet>,
        lr_to_lalr: &BTreeMap<usize, usize>,
        _i: usize,
        lalr_sets: &IndexSet<ItemSet>,
    ) -> Option<usize> {
        let goto_ia = item_set.goto(a, grammar);
        let lr_idx = match lr_sets.get_index_of(&goto_ia) {
            Some(i) => i,
            None => {
                return lalr_sets.get_index_of(&goto_ia);
            }
        };
        lr_to_lalr.get(&lr_idx).cloned()
    }

    for (i, item_set) in lalr_sets.iter().enumerate() {
        for (item, lookaheads) in item_set.iter() {
            let dot_value = item.dot_token(grammar);

            // A dot value of Some(Symbol) means the dot is not at the end of the production
            if let Some(dot_value) = dot_value {
                // If the terminal brings us to a new state then add action to shift to the new state:
                // If [A -> α.𝑎β, b] and GOTO(item_set, 𝑎) = new_item_set_idx
                // then set ACTION[item_set_idx, 𝑎] = Shift(new_item_set_idx)
                if dot_value.is_terminal() {
                    let token_idx = grammar.get_token_idx(dot_value).unwrap();
                    let goto_idx = compute_goto(
                        item_set, dot_value, grammar, lr_sets, lr_to_lalr, i, lalr_sets,
                    );

                    let j = match goto_idx {
                        Some(j) => j,
                        None => continue,
                    };

                    match action.entry((i as u32, token_idx)) {
                        Entry::Vacant(entry) => entry.insert(Action::Shift(j as u32)),
                        Entry::Occupied(_) => panic!("Conflict ({}, {:?})", i, token_idx),
                    };
                }
                continue;
            }
            // At this point we are at the end of the production, ex/ S ⇒  C C⋅

            for lookahead in lookaheads {
                // We have reached the end, set action to accept
                if *lookahead == Grammar::EOF_TOKEN_IDX
                    && item.production_idx() == Grammar::AUGMENTED_START_PRODUCTION_IDX
                {
                    match action.entry((i as u32, Grammar::EOF_TOKEN_IDX)) {
                        Entry::Vacant(entry) => entry.insert(Action::Accept),
                        Entry::Occupied(entry) => {
                            panic!(
                                "Conflict ({}, {:?}), Current: {:?}",
                                i,
                                Grammar::EOF_TOKEN_IDX,
                                entry.get()
                            )
                        }
                    };
                    continue;
                }

                // Otherwise we reduce the production
                match action.entry((i as u32, *lookahead)) {
                    Entry::Vacant(entry) => entry.insert(Action::Reduce(item.production_idx())),
                    Entry::Occupied(_) => panic!("Conflict ({}, {:?})", i, lookahead),
                };
            }
        }
    }

    for (i, state) in lalr_sets.iter().enumerate() {
        for (_token_idx, production_name) in grammar.non_terminals() {
            let goto_ia = state.goto(&Symbol::NonTerminal(*production_name), grammar);

            // lalr state = I1 U I2 U ... etc where the I's are lr(1) states with same core merged
            // so we much check if goto_ia == lalr state, or
            match lr_sets.get_index_of(&goto_ia) {
                Some(j) => {
                    goto.insert(
                        (i as u32, *production_name),
                        *lr_to_lalr.get(&j).expect("LR -> LALR"),
                    );
                }
                None => match lalr_sets.get_index_of(&goto_ia) {
                    Some(j) => {
                        goto.insert((i as u32, *production_name), j);
                    }
                    None => (),
                },
            }
        }
    }

    (action, goto)
}

/// A "core" of an LR(1) item set is the set of all the first components of all LR(1) items in the set:
/// I4: [C ->  d., c/d] the core is { C -> d. }

/// I3: [C -> c.C, c/d] the core is { C -> c. C, C -> .cC, C -> .d }
///     [C -> .cC, c/d]
///     [C ->  .d, c/d]
///
/// I7: [C ->  d.,   $] the core is { C -> d. }
///
/// I4 and I7 have the same core so we merge them
pub fn merge_cores(lr_sets: &IndexSet<ItemSet>) -> (IndexSet<ItemSet>, BTreeMap<usize, usize>) {
    let mut presence_vec = BitVec::from_elem(lr_sets.len(), true);
    let mut core_hash_to_index = IndexMap::<u64, Vec<usize>>::new();

    for (i, set) in lr_sets.iter().enumerate() {
        core_hash_to_index
            .entry(set.core_hash())
            .or_insert_with(Vec::new)
            .push(i);
    }

    let mut lalr_sets = lr_sets
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

    use crate::{
        lalr::item::{ItemSet, ItemSetDebug, TokenDebug},
        parse_grammar,
        parser::Grammar,
    };

    use super::{make_lalr_tables, merge_cores};

    #[test]
    fn merging() {
        let grammar_str = r#"
            export start S: () = [ 
                <c1: C> <c2: C> => ("nothing to see here")
            ]

            export C: () = [
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
            export start S: () = [ 
                <c1: C> <c2: C> => ("nothing to see here")
            ]

            export C: () = [
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

        let (action, _goto) = make_lalr_tables(&grammar, &lalr_set, &lr_items, &lr_to_lalr);

        for (i, item_set) in action.iter() {
            let item_set = item_set.iter().fold(BTreeMap::new(), |mut acc, (k, v)| {
                acc.insert(TokenDebug(*k, &grammar), *v);
                acc
            });
            println!("{}: {:#?}", i, item_set)
        }

        println!("\n--------\n");

        // for (i, item_set) in goto.iter() {
        //     let item_set = item_set.iter().fold(BTreeMap::new(), |mut acc, (k, v)| {
        //         acc.insert(TokenDebug(*k, &grammar), *v);
        //         acc
        //     });
        //     println!("{}: {:#?}", i, item_set)
        // }
    }
}
