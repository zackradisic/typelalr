use std::{
    cmp::{self},
    collections::{BTreeMap, BTreeSet, HashSet},
};

use bit_vec::BitVec;

use crate::{
    nfa::{Match, StateIdx, NFA},
    option_usize::OptionUsize,
};

#[derive(Clone, Debug)]
pub struct DFA {
    pub edges: Vec<Edge>,
    pub states: Vec<State>,
    pub accepting_states: Vec<StateIdx>,
}

#[derive(Clone, Debug)]
pub struct DFABuilder {
    edges: Vec<Edge>,
    states: Vec<State>,
}

#[derive(Clone, Debug)]
pub struct Edge {
    pub from: StateIdx,
    pub label: Match,
    pub to: StateIdx,
}

#[derive(Clone, Debug)]
pub struct State {
    first_edge: OptionUsize<usize>,
    /// `true` when this is a state that accepts the entire NFA
    pub is_accept: bool,
}

impl DFA {
    pub const START_IDX: StateIdx = StateIdx(0);

    pub fn from_nfa(nfa: NFA) -> (Self, BTreeMap<StateIdx, StateIdx>) {
        let alphabet = nfa.labels();
        let mut set = BTreeSet::new();
        for label in alphabet {
            set.insert(label.clone());
        }
        DFABuilder::new(&nfa).build(&nfa, set.into_iter().collect())
    }

    pub fn test(&self, s: &str) -> bool {
        match self.simulate(s) {
            Some(eaten) => eaten == s.len(),
            _ => false,
        }
    }

    pub fn simulate(&self, s: &str) -> Option<usize> {
        let mut state_idx = Self::START_IDX;

        let mut eaten = 0;
        for c in s.chars() {
            if let Some(mut edge_iter) = self.edge_iter(state_idx) {
                match edge_iter.find(|edge| edge.label.matches(c)) {
                    Some(edge) => {
                        eaten += 1;
                        state_idx = edge.to;
                    }
                    None => break,
                }
            }
        }

        if self.accepting_states.contains(&state_idx) {
            Some(eaten)
        } else {
            None
        }
    }

    pub fn edge_iter(&self, state_idx: StateIdx) -> Option<EdgeIter> {
        let edge_idx = self.states[state_idx.0].first_edge.unwrap_or(usize::MAX);
        if edge_idx == usize::MAX {
            return None;
        }

        Some(EdgeIter {
            dfa: self,
            state: state_idx,
            edge_idx,
        })
    }

    pub fn shift_state_and_edges(&mut self, state_offset: usize, edge_offset: usize) {
        for state in &mut self.states {
            let first_edge = &mut state.first_edge;
            if first_edge.is_some() {
                *first_edge = OptionUsize::some(first_edge.unwrap() + edge_offset);
            }
        }

        for edge in &mut self.edges {
            edge.from = StateIdx(edge.from.0 + state_offset);
            edge.to = StateIdx(edge.to.0 + state_offset);
        }

        for state in &mut self.accepting_states {
            *state = StateIdx(state.0 + state_offset);
        }
    }
}

impl DFABuilder {
    pub fn new(nfa: &NFA) -> Self {
        Self {
            edges: vec![],
            states: vec![],
        }
    }

    fn hashset_to_btreeset(f: HashSet<StateIdx>) -> BTreeSet<StateIdx> {
        let mut ret = BTreeSet::new();
        for s in f {
            ret.insert(s);
        }
        ret
    }

    // This is the algorithm described in chapter 3.7.1 of the dragon book
    pub fn build(mut self, nfa: &NFA, alphabet: Vec<Match>) -> (DFA, BTreeMap<StateIdx, StateIdx>) {
        // let mut d_states = vec![nfa.epsilon_closure([NFA::START_IDX].into())];
        let mut d_states = vec![Self::hashset_to_btreeset(
            nfa.epsilon_closure([NFA::START_IDX].into()),
        )];
        // let mut d_states: BTreeMap<usize, BTreeSet<StateIdx>> = BTreeMap::from([(
        //     0,
        //     Self::hashset_to_btreeset(nfa.epsilon_closure([NFA::START_IDX].into())),
        // )]);
        let mut first_edges: Vec<OptionUsize<usize>> = vec![];
        let mut marks = BitVec::from_elem(1, false);

        loop {
            // Loop while there is an unmarked state set
            let t_idx = match (0..d_states.len()).find(|idx| !marks[*idx]) {
                Some(idx) => idx,
                None => break,
            };

            // Mark `t`
            marks.set(t_idx, true);

            first_edges.push(OptionUsize::none());

            for label in &alphabet {
                let u = Self::epsilon_closure(nfa, Self::move_set(nfa, &d_states[t_idx], label));
                if u.is_empty() {
                    continue;
                }
                let idx = match d_states.iter().enumerate().find(|(_, s)| *s == &u) {
                    Some((i, _)) => i,
                    None => {
                        marks.push(false);
                        let new_idx = d_states.len();
                        d_states.push(u);
                        new_idx
                    }
                };

                let first_edge = &mut first_edges[t_idx];
                if first_edge.is_none() {
                    // *first_edge = OptionUsize::some(idx);
                    *first_edge = OptionUsize::some(self.edges.len());
                }
                self.edges.push(Edge {
                    from: StateIdx(t_idx),
                    label: label.clone(),
                    to: StateIdx(idx),
                });
            }
        }

        let accepting_states = nfa.accepting_states();
        let mut final_states = Vec::with_capacity(d_states.capacity());
        let mut nfa_to_dfa_mapping = BTreeMap::new();
        for (i, states) in d_states.iter().enumerate() {
            for s in states.iter() {
                if accepting_states.contains(s) {
                    nfa_to_dfa_mapping.insert(*s, StateIdx(i));
                }
            }
        }

        for (i, states) in d_states.into_iter().enumerate() {
            final_states.push(State {
                first_edge: first_edges[i],
                is_accept: states.iter().any(|s| accepting_states.contains(s)),
            })
        }

        (
            DFA {
                edges: self.edges,
                accepting_states: final_states
                    .iter()
                    .enumerate()
                    .filter_map(|(i, s)| if s.is_accept { Some(StateIdx(i)) } else { None })
                    .collect(),
                states: final_states,
            },
            nfa_to_dfa_mapping,
        )
    }

    fn epsilon_closure(nfa: &NFA, mut set: BTreeSet<StateIdx>) -> BTreeSet<StateIdx> {
        let mut stack: Vec<StateIdx> = Vec::with_capacity(set.len());
        for s in set.iter() {
            stack.push(*s);
        }

        while let Some(state) = stack.pop() {
            if let Some(edge_iter) = nfa.edge_iter(state) {
                // Iterate epsilon edges
                for edge in edge_iter.filter(|edge| edge.label.is_none()) {
                    let u = edge.to;
                    if set.insert(u) {
                        stack.push(u);
                    }
                }
            }
        }

        set
    }

    fn move_set(nfa: &NFA, set: &BTreeSet<StateIdx>, label: &Match) -> BTreeSet<StateIdx> {
        let mut ret = BTreeSet::new();

        for state in set.iter() {
            if let Some(edge_iter) = nfa.edge_iter(*state) {
                for edge in edge_iter {
                    if edge.label.as_ref().map_or(false, |l| {
                        l.range.contains(&label.range.start)
                            && l.range.contains(&(label.range.end - 1))
                    }) {
                        ret.insert(edge.to);
                    }
                }
            }
        }

        ret
    }
}

// From: https://github.com/lalrpop/lalrpop/blob/master/lalrpop/src/lexer/dfa/overlap.rs
pub fn remove_overlap(ranges: &BTreeSet<Match>) -> Vec<Match> {
    // We will do this in the dumbest possible way to start. :)
    // Maintain a result vector that contains disjoint ranges.  To
    // insert a new range, we walk over this vector and split things
    // up as we go. This algorithm is so naive as to be exponential, I
    // think. Sue me.

    let mut disjoint_ranges = vec![];

    for range in ranges {
        add_range(&range, 0, &mut disjoint_ranges);
    }

    // the algorithm above leaves some empty ranges in for simplicity;
    // prune them out.
    disjoint_ranges.retain(|r| !r.range.is_empty());

    disjoint_ranges
}

fn add_range(range: &Match, start_index: usize, disjoint_ranges: &mut Vec<Match>) {
    if range.range.is_empty() {
        return;
    }

    // Find first overlapping range in `disjoint_ranges`, if any.
    match disjoint_ranges[start_index..]
        .iter()
        .position(|r| r.clone().intersects(&range))
    {
        Some(index) => {
            let index = index + start_index;
            let overlapping_range = disjoint_ranges[index].clone();

            // If the range we are trying to add already exists, we're all done.
            if &overlapping_range == range {
                return;
            }

            // Otherwise, we want to create three ranges (some of which may
            // be empty). e.g. imagine one range is `a-z` and the other
            // is `c-l`, we want `a-b`, `c-l`, and `m-z`.
            let min_min = cmp::min(range.range.start, overlapping_range.range.start);
            let mid_min = cmp::max(range.range.start, overlapping_range.range.start);
            let mid_max = cmp::min(range.range.end, overlapping_range.range.end);
            let max_max = cmp::max(range.range.end, overlapping_range.range.end);
            let low_range = Match {
                range: min_min..mid_min,
            };
            let mid_range = Match {
                range: mid_min..mid_max,
            };
            let max_range = Match {
                range: mid_max..max_max,
            };

            assert!(low_range.is_disjoint(&mid_range));
            assert!(low_range.is_disjoint(&max_range));
            assert!(mid_range.is_disjoint(&max_range));

            // Replace the existing range with the low range, and then
            // add the mid and max ranges in. (The low range may be
            // empty, but we'll prune that out later.)
            disjoint_ranges[index] = low_range;
            add_range(&mid_range, index + 1, disjoint_ranges);
            add_range(&max_range, index + 1, disjoint_ranges);
        }

        None => {
            // no overlap -- easy case.
            disjoint_ranges.push(range.clone());
        }
    }
}

pub struct EdgeIter<'a> {
    dfa: &'a DFA,
    state: StateIdx,
    edge_idx: usize,
}

impl<'a> Iterator for EdgeIter<'a> {
    type Item = &'a Edge;

    fn next(&mut self) -> Option<Self::Item> {
        let edge = self.dfa.edges.get(self.edge_idx)?;
        if edge.from != self.state {
            return None;
        }
        self.edge_idx += 1;

        Some(edge)
    }
}

#[cfg(test)]
mod test {
    use regex_syntax::Parser;

    use crate::nfa::NFA;

    use super::DFA;

    #[test]
    fn sanity() {
        let hir = Parser::new().parse("a*ab").unwrap();
        let nfa = NFA::from_regex(&hir);
        let (dfa, _) = DFA::from_nfa(nfa);

        assert!(dfa.test("aaaaaab"));
        assert!(!dfa.test("b"))
    }

    #[test]
    fn sanity2() {
        let hir = Parser::new().parse("(fuck)*fuckyou").unwrap();
        let nfa = NFA::from_regex(&hir);
        let (dfa, _) = DFA::from_nfa(nfa);

        assert!(dfa.test("fuckfuckyou"));
        assert!(!dfa.test("fuckkkk"))
    }

    #[test]
    fn sanity3() {
        let hir = Parser::new().parse("(f|u|c|k)*fuckyou").unwrap();
        let nfa = NFA::from_regex(&hir);
        let (dfa, _) = DFA::from_nfa(nfa);

        assert!(dfa.test("fffuuuucccckkkfuckyou"));
        assert!(dfa.test("fuckkkkfuckyou"));
        assert!(dfa.test("fuckyou"));
        assert!(dfa.test("fuckyou"));
        assert!(dfa.test("ffuckyou"));

        assert!(!dfa.test("zfuckyou"));
    }

    #[test]
    fn sanity4() {
        let hir = Parser::new().parse("q").unwrap();
        let nfa = NFA::from_regex(&hir);
        let (dfa, _) = DFA::from_nfa(nfa);

        assert!(dfa.test("q"));

        assert!(!dfa.test("qqq"));
        assert!(!dfa.test("a"));
        assert!(!dfa.test(""));
    }

    #[test]
    fn number() {
        let hir = Parser::new().parse("[0-9]+").unwrap();
        let nfa = NFA::from_regex(&hir);
        let (dfa, _) = DFA::from_nfa(nfa);

        assert_eq!(dfa.simulate("420"), Some(3));
    }
}
