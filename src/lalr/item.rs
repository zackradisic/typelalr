//! This is mostly adapted from LR parsing sections in the Dragon Book
use std::{
    collections::{btree_map::Entry, hash_map::DefaultHasher, BTreeMap, BTreeSet},
    hash::{Hash, Hasher},
};

use indexmap::IndexSet;

use crate::parser::{
    ast::{Ident, InputSymbol, NamedSymbol},
    Grammar, Production, Symbol,
};

use super::first::first;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProductionIdx(pub u32);

/// An index to one of the possible tokens in the grammar
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenIdx(pub u32);

/// An LR(1) item.
///
/// First value represents the grammar production, second represents
/// the position of the dot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Item(ProductionIdx, u32);

/// LR(1) set of items
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ItemSet(BTreeMap<Item, BTreeSet<TokenIdx>>);

pub struct ItemIter<'ast> {
    item: Item,
    grammar: &'ast Grammar<'ast>,
}

impl TokenIdx {
    fn debug_grammar<'ast>(
        self,
        f: &mut std::fmt::Formatter<'_>,
        grammar: &'ast Grammar<'ast>,
    ) -> std::fmt::Result {
        match grammar.get_token(self) {
            Some(token) => write!(f, "{:?}", token),
            None => write!(f, "{:?}", self),
        }
    }
}

impl Item {
    #[inline]
    pub fn production_idx(self) -> ProductionIdx {
        self.0
    }

    #[inline]
    pub fn dot(self) -> u32 {
        self.1
    }

    /// Increment the position of the dot
    #[inline]
    fn increment<'ast>(mut self) -> Self {
        self.1 += 1;
        self
    }

    pub fn production<'ast>(self, grammar: &'ast Grammar<'ast>) -> &'ast Production<'ast> {
        grammar.get_production(self.production_idx()).unwrap()
    }

    pub fn dot_token<'ast>(self, grammar: &'ast Grammar<'ast>) -> Option<&'ast Symbol<'ast>> {
        let production = grammar.get_production(self.production_idx()).unwrap();
        production.input_tokens.get(self.dot() as usize)
    }

    pub fn last_token<'ast>(self, grammar: &'ast Grammar<'ast>) -> &'ast Symbol<'ast> {
        let production = grammar.get_production(self.production_idx()).unwrap();
        production.input_tokens.last().unwrap()
    }

    fn dot_production_name<'ast>(self, grammar: &'ast Grammar<'ast>) -> Option<Ident<'ast>> {
        let this_production: &Production = grammar.get_production(self.production_idx()).unwrap();

        match this_production
            .input_tokens
            .get(self.dot() as usize)
            .and_then(|symbol| symbol.as_non_terminal())
        {
            Some(non_terminal) => Some(*non_terminal),
            _ => None,
        }
    }

    /// Iterate over the symbols in this item (after the dot)
    fn iter<'ast>(self, grammar: &'ast Grammar<'ast>) -> ItemIter<'ast> {
        ItemIter {
            item: self,
            grammar,
        }
    }

    fn debug_grammar<'ast>(
        self,
        f: &mut std::fmt::Formatter<'_>,
        grammar: &'ast Grammar<'ast>,
    ) -> std::fmt::Result {
        match grammar.get_production(self.production_idx()) {
            Some(production) => {
                f.write_str(production.name.as_ref())?;
                f.write_str(" ⇒  ")?;
                let last = if production.input_tokens.is_empty() {
                    0
                } else {
                    production.input_tokens.len() - 1
                };
                for (i, token) in production.input_tokens.iter().enumerate() {
                    if i == self.dot() as usize {
                        f.write_str("⋅")?;
                    }
                    write!(f, "{}{}", token, if last == i { "" } else { " " })?;
                }
                if self.dot() as usize == production.input_tokens.len() {
                    f.write_str("⋅")?;
                }
                Ok(())
            }
            None => write!(f, "{:?}", self),
        }
    }
}

impl ItemSet {
    pub fn debug<'ast>(&'ast self, grammar: &'ast Grammar<'ast>) -> ItemSetDebug<'ast> {
        ItemSetDebug(self, grammar)
    }
    pub fn iter(&self) -> std::collections::btree_map::Iter<Item, BTreeSet<TokenIdx>> {
        self.0.iter()
    }

    pub fn default_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }

    pub fn core_hash(&self) -> u64 {
        let hasher = DefaultHasher::new();
        self.core_hash_with_hasher(hasher)
    }

    pub fn core_hash_with_hasher<H: Hasher>(&self, mut h: H) -> u64 {
        for item in self.0.keys() {
            item.hash(&mut h);
        }
        h.finish()
    }

    /// Form a union of two item sets.
    ///
    /// It is incorrect to call this with two item sets that don't share the same core
    pub fn union(&mut self, other: Self) {
        debug_assert_eq!(self.0.keys().len(), other.0.keys().len());

        for (item, lookaheads) in self.0.iter_mut() {
            lookaheads.extend(other.0.get(&item).unwrap());
        }
    }

    fn initial_items<'ast>() -> Self {
        let augmented_start_item = Item(Grammar::AUGMENTED_START_PRODUCTION_IDX, 0);
        Self(BTreeMap::from_iter([(
            augmented_start_item,
            BTreeSet::from_iter([Grammar::EOF_TOKEN_IDX]),
        )]))
    }

    pub fn items<'ast>(grammar: &'ast Grammar<'ast>) -> IndexSet<Self> {
        fn compute_hash(item_set: &ItemSet) -> u64 {
            let mut hasher = DefaultHasher::new();
            item_set.hash(&mut hasher);
            hasher.finish()
        }

        let mut initial_items = Self::initial_items();
        initial_items.closure(grammar);

        let mut hash_stack = vec![compute_hash(&initial_items)];
        let mut c = IndexSet::from_iter([initial_items]);
        let mut temp = Vec::new();

        loop {
            for item_set in c.iter() {
                for grammar_symbol in grammar.tokens() {
                    let goto_i_x = item_set.goto(grammar_symbol, grammar);
                    if !goto_i_x.0.is_empty() && !c.contains(&goto_i_x) {
                        temp.push(goto_i_x);
                    }
                }
            }

            if temp.is_empty() {
                break;
            }

            while !temp.is_empty() {
                let item_set = temp.pop().unwrap();
                hash_stack.push(compute_hash(&item_set));
                c.insert(item_set);
            }
        }

        c
    }

    fn closure<'ast>(&mut self, grammar: &'ast Grammar<'ast>) {
        let mut to_be_added: Vec<(Item, TokenIdx)> = Vec::new();
        loop {
            for (item, lookahead_set) in self.0.iter_mut() {
                let production_name = match item.dot_production_name(grammar) {
                    Some(prod_name) => prod_name,
                    None => continue,
                };

                for (prod_idx, _prod) in grammar.production_iter(production_name) {
                    for lookahead in lookahead_set.iter() {
                        let β𝑎 = item
                            .increment()
                            .iter(grammar)
                            .chain(std::iter::once(grammar.get_token(*lookahead).unwrap()));

                        for terminal in first(grammar, β𝑎) {
                            let item = Item(prod_idx, 0);
                            let token_idx = grammar.get_token_idx(terminal).unwrap();

                            to_be_added.push((item, token_idx));
                        }
                    }
                }
            }

            let mut added = false;
            for (item, token_idx) in to_be_added.iter() {
                match self.0.entry(*item) {
                    Entry::Vacant(entry) => {
                        entry.insert(BTreeSet::from_iter([*token_idx]));
                        added = true;
                    }
                    Entry::Occupied(mut entry) => {
                        if entry.get_mut().insert(*token_idx) {
                            added = true;
                        }
                    }
                }
            }
            to_be_added.clear();

            if !added {
                break;
            }
        }
    }

    pub fn goto<'ast>(&self, x: &Symbol, grammar: &'ast Grammar<'ast>) -> Self {
        let mut j = BTreeMap::<Item, BTreeSet<TokenIdx>>::default();

        for (item, lookahead_set) in self.0.iter() {
            let input_token = match item.dot_token(grammar) {
                Some(input_token) => input_token,
                None => continue,
            };

            if input_token != x {
                continue;
            }

            let new_item = item.increment();
            for lookahead in lookahead_set.iter() {
                match j.entry(new_item) {
                    Entry::Vacant(entry) => {
                        entry.insert(BTreeSet::from_iter([*lookahead]));
                    }
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(*lookahead);
                    }
                }
            }
        }

        let mut ret = Self(j);
        ret.closure(grammar);

        ret
    }

    fn display<'ast>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        grammar: &'ast Grammar<'ast>,
    ) -> std::fmt::Result {
        f.debug_set()
            .entries(self.0.iter().map(|(item, lookahead)| {
                // TODO: FIX
                ItemLookaheadDebug(ItemDebug(*item, grammar), TokenSetDebug(lookahead, grammar))
            }))
            .finish()

        // f.debug_tuple("ItemSet")
        //     .field(
        //         f.debug_set()
        //             .entries(
        //                 self.0
        //                     .iter()
        //                     .map(|(item, lookahead)| f.debug_list().entry(item).entry(lookahead)),
        //             )
        //             .into(),
        //     )
        //     .finish()
    }
}

impl std::hash::Hash for ItemSet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (item, lookaheads) in self.0.iter() {
            item.hash(state);
            for lookahead in lookaheads {
                lookahead.hash(state);
            }
        }
    }
}

impl<'ast> Iterator for ItemIter<'ast> {
    type Item = &'ast Symbol<'ast>;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_idx = self.item.dot();
        self.item.1 += 1;

        let production = self.grammar.get_production(self.item.production_idx())?;
        production.input_tokens.get(cur_idx as usize)
    }
}

pub struct ItemSetDebug<'ast>(pub &'ast ItemSet, pub &'ast Grammar<'ast>);
struct ItemLookaheadDebug<'ast>(ItemDebug<'ast>, TokenSetDebug<'ast>);
struct TokenSetDebug<'ast>(&'ast BTreeSet<TokenIdx>, &'ast Grammar<'ast>);
pub struct TokenDebug<'ast>(pub TokenIdx, pub &'ast Grammar<'ast>);
struct ItemDebug<'ast>(Item, &'ast Grammar<'ast>);

impl<'ast> std::fmt::Debug for ItemSetDebug<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display(f, self.1)
    }
}
impl<'ast> std::fmt::Debug for ItemLookaheadDebug<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Item")
            .field("item", &self.0)
            .field("lookahead_tokens", &self.1)
            .finish()
    }
}
impl<'ast> std::fmt::Debug for TokenSetDebug<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set()
            .entries(self.0.iter().map(|token| TokenDebug(*token, self.1)))
            .finish()
    }
}
impl<'ast> std::fmt::Debug for TokenDebug<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.debug_grammar(f, self.1)
    }
}
impl<'ast> Eq for TokenDebug<'ast> {
    fn assert_receiver_is_total_eq(&self) {}
}
impl<'ast> Ord for TokenDebug<'ast> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<'ast> PartialEq for TokenDebug<'ast> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<'ast> PartialOrd for TokenDebug<'ast> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}
impl<'ast> std::fmt::Debug for ItemDebug<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.debug_grammar(f, self.1)
    }
}

#[cfg(test)]
mod test {
    use bumpalo::Bump;

    use crate::{lalr::item::ItemSetDebug, parse_grammar, parser::Grammar};

    use super::{first, ItemSet, ProductionIdx};

    #[test]
    fn test() {
        let grammar_str = r#"
            export start Foo = [ 
                "hello" "how" "are" "you" => ({ hello: "sir" }),
                r"[0-9]+" => ({ hello: "sir" }),
                <bruh: Bruh> => ("noice")
            ]

            export Bruh = [
                "noice" => ("lmao")
            ]
        "#;

        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        let first_set = first(
            &grammar,
            grammar
                .get_production(ProductionIdx(0))
                .unwrap()
                .input_tokens
                .iter(),
        );

        println!("FIRST: {:#?}", first_set);
    }

    #[test]
    fn closure() {
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

        let mut initial_items = ItemSet::initial_items();
        initial_items.closure(&grammar);

        // for (item, lookaheads) in &initial_items.0.iter() {}

        assert_eq!(
            format!("{:#?}", ItemSetDebug(&initial_items, &grammar)),
            r#"{
    Item {
        item: S' ⇒  ⋅S,
        lookahead_tokens: {
            Eof,
        },
    },
    Item {
        item: S ⇒  ⋅C C,
        lookahead_tokens: {
            Eof,
        },
    },
    Item {
        item: C ⇒  ⋅c C,
        lookahead_tokens: {
            StrLit("c"),
            StrLit("d"),
        },
    },
    Item {
        item: C ⇒  ⋅d,
        lookahead_tokens: {
            StrLit("c"),
            StrLit("d"),
        },
    },
}"#
        );
        println!("{:#?}", ItemSetDebug(&initial_items, &grammar));
    }

    #[test]
    fn items() {
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

        let items = ItemSet::items(&grammar);

        for (i, item_set) in items.iter().enumerate() {
            println!("{}: {:#?}", i, ItemSetDebug(item_set, &grammar));
        }
        // println!("{:#?}", items);
    }
}
