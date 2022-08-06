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
    dfa_to_nfa: BTreeMap<StateIdx, BTreeSet<StateIdx>>,
    accepting_states: BTreeSet<StateIdx>,
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
    // If this state is accepting, this is an index into a list containing the set of NFA states
    // this DFA state represents, so that it may be consulted to see which NFA states it accepts.
    // accepting_states_idx: OptionUsize<usize>,
}

impl DFA {
    pub const START_IDX: StateIdx = StateIdx(0);

    pub fn accepting_states(&self) -> BTreeMap<StateIdx, Vec<StateIdx>> {
        let mut ret = BTreeMap::<StateIdx, Vec<StateIdx>>::new();

        for (dfa_state, nfa_set) in self.dfa_to_nfa.iter() {
            for nfa_state_idx in nfa_set
                .iter()
                .filter(|idx| self.accepting_states.contains(idx))
            {
                match ret.entry(*dfa_state) {
                    std::collections::btree_map::Entry::Vacant(mut entry) => {
                        entry.insert(vec![*nfa_state_idx]);
                    }
                    std::collections::btree_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().push(*nfa_state_idx);
                    }
                }
            }
        }

        ret
    }

    // pub fn accepting_state(&self, state_idx: usize) -> Option<StateIdx> {
    //     match self.dfa_to_nfa.get(&StateIdx(state_idx)) {
    //         Some(set) => set
    //             .iter()
    //             .find(|nfa_state_idx| self.accepting_states.contains(nfa_state_idx))
    //             .cloned(),
    //         None => None,
    //     }
    // }

    pub fn from_nfa(nfa: NFA) -> Self {
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

        if self.dfa_to_nfa.contains_key(&state_idx) {
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
    pub fn build(mut self, nfa: &NFA, alphabet: Vec<Match>) -> DFA {
        let mut first_edges: Vec<OptionUsize<usize>> = vec![];
        let mut marks = BitVec::from_elem(1, false);
        let mut d_states = vec![Self::hashset_to_btreeset(
            nfa.epsilon_closure([NFA::START_IDX].into()),
        )];
        let mut dfa_to_nfa = BTreeMap::new();

        loop {
            // Loop while there is an unmarked state set
            let t_idx = match (0..d_states.len()).find(|idx| !marks[*idx]) {
                Some(idx) => idx,
                None => break,
            };

            // Mark `t`
            marks.set(t_idx, true);

            first_edges.push(OptionUsize::none());

            // Loop through each possible edge match
            for label in &alphabet {
                // Compute the set of NFA states we are in
                let u = Self::epsilon_closure(nfa, Self::move_set(nfa, &d_states[t_idx], label));
                if u.is_empty() {
                    continue;
                }

                // Get the idx of this set of NFA states
                let idx = match d_states.iter().enumerate().find(|(_, s)| *s == &u) {
                    Some((i, _)) => i,
                    None => {
                        // If not found add it
                        marks.push(false);
                        let new_idx = d_states.len();
                        d_states.push(u.clone());

                        dfa_to_nfa.insert(StateIdx(new_idx), u);

                        new_idx
                    }
                };

                // Set first edge if need be
                let first_edge = &mut first_edges[t_idx];
                if first_edge.is_none() {
                    *first_edge = OptionUsize::some(self.edges.len());
                }

                // Push edge
                self.edges.push(Edge {
                    from: StateIdx(t_idx),
                    label: label.clone(),
                    to: StateIdx(idx),
                });
            }
        }

        let accepting_states = nfa.accepting_states();
        let mut final_states = Vec::with_capacity(d_states.capacity());

        for (i, states) in d_states.into_iter().enumerate() {
            final_states.push(State {
                first_edge: first_edges[i],
            })
        }

        DFA {
            edges: self.edges,
            dfa_to_nfa,
            states: final_states,
            accepting_states,
        }
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
        let dfa = DFA::from_nfa(nfa);

        assert!(dfa.test("aaaaaab"));
        assert!(dfa.test("ab"));
        assert!(!dfa.test("b"))
    }

    #[test]
    fn sanity2() {
        let hir = Parser::new().parse("(fuck)*fuckyou").unwrap();
        let nfa = NFA::from_regex(&hir);
        let dfa = DFA::from_nfa(nfa);

        assert!(dfa.test("fuckfuckyou"));
        assert!(!dfa.test("fuckkkk"))
    }

    #[test]
    fn sanity3() {
        let hir = Parser::new().parse("(f|u|c|k)*fuckyou").unwrap();
        let nfa = NFA::from_regex(&hir);
        let dfa = DFA::from_nfa(nfa);

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
        let dfa = DFA::from_nfa(nfa);

        assert!(dfa.test("q"));

        assert!(!dfa.test("qqq"));
        assert!(!dfa.test("a"));
        assert!(!dfa.test(""));
    }

    #[test]
    fn number() {
        let hir = Parser::new().parse("[0-9]+").unwrap();
        let nfa = NFA::from_regex(&hir);
        let dfa = DFA::from_nfa(nfa);

        assert_eq!(dfa.simulate("420"), Some(3));
    }

    #[test]
    fn optional() {
        let hir = Parser::new().parse("a?bcd").unwrap();

        let nfa = NFA::from_regex(&hir);
        let dfa = DFA::from_nfa(nfa);

        assert!(dfa.test("bcd"));

        assert!(dfa.test("abcd"));
        assert!(!dfa.test("aaaabcd"));
        assert!(!dfa.test("aaaaaaabcd"));

        assert!(!dfa.test("cbcd"));
    }
}
