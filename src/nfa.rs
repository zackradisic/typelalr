use regex_syntax::hir::Class;
use regex_syntax::hir::Hir as RegexHir;
use regex_syntax::hir::HirKind;
use regex_syntax::hir::Literal;
use regex_syntax::hir::RepetitionKind;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::ops::Range;

use crate::option_usize::OptionUsize;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct StateIdx(pub usize);

#[derive(Clone, Debug)]
pub struct NFA {
    pub edges: Vec<Edge>,
    pub states: Vec<State>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Match {
    pub range: Range<u32>,
}

#[derive(Clone, Debug)]
pub struct Edge {
    pub from: StateIdx,
    /// None if epsilon
    pub label: Option<Match>,
    pub to: StateIdx,
}

#[derive(Clone, Debug)]
pub struct State {
    first_edge: OptionUsize<usize>,
    /// `true` when this is a state that accepts the entire NFA
    is_accept: bool,
}

impl StateIdx {
    #[inline]
    pub fn value(self) -> usize {
        self.0
    }
}

impl Into<usize> for StateIdx {
    fn into(self) -> usize {
        self.0
    }
}

impl From<usize> for StateIdx {
    fn from(val: usize) -> Self {
        Self(val)
    }
}

// Regex -> NFA
impl NFA {
    pub const START_IDX: StateIdx = StateIdx(0);
    pub const FINAL_ACCEPT_IDX: StateIdx = StateIdx(1);

    pub fn from_regex(regex: &RegexHir) -> Self {
        let mut this = Self {
            edges: vec![],
            states: vec![
                // start state
                State {
                    first_edge: OptionUsize::none(),
                    is_accept: false,
                },
                // accepting state
                State {
                    first_edge: OptionUsize::none(),
                    is_accept: true,
                },
            ],
        };

        let after_start = this.compile(regex, Self::FINAL_ACCEPT_IDX);

        this.push_edge(
            Self::START_IDX,
            None,
            after_start.unwrap_or(Self::FINAL_ACCEPT_IDX),
        );

        this
    }

    fn push_edge(&mut self, from: StateIdx, label: Option<Match>, to: StateIdx) {
        let new_edge = self.edges.len();
        self.edges.push(Edge { from, label, to });

        let first = &mut self.states[from.0].first_edge;
        if first.is_none() {
            *first = OptionUsize::some(new_edge)
        }
    }

    fn compile(&mut self, regex: &RegexHir, accept: StateIdx) -> OptionUsize<StateIdx> {
        match regex.kind() {
            HirKind::Empty => OptionUsize::none(),
            HirKind::Literal(lit) => self.compile_match(
                match lit {
                    Literal::Unicode(c) => Match::char(*c),
                    Literal::Byte(b) => Match::byte(*b),
                },
                accept,
            ),
            HirKind::Class(class) => {
                let ranges: Vec<Match> = match class {
                    Class::Unicode(class) => class
                        .ranges()
                        .iter()
                        .map(|range| Match {
                            range: range.start() as u32..(range.end() as u32 + 1),
                        })
                        .collect(),
                    Class::Bytes(class) => class
                        .ranges()
                        .iter()
                        .map(|range| Match {
                            range: range.start() as u32..(range.end() as u32 + 1),
                        })
                        .collect(),
                };

                // `f` is the input state than has epsilon edges from each alternation and
                // has one edge to the accepting state
                let f_idx = StateIdx(self.states.len());
                self.states.push(State {
                    first_edge: OptionUsize::none(),
                    is_accept: false,
                });
                self.push_edge(f_idx, Match::EPSILON, accept);

                // `i` is the input state than has epsilon edges to each alternation
                let i_idx = StateIdx(self.states.len());
                self.states.push(State {
                    first_edge: OptionUsize::none(),
                    is_accept: false,
                });

                let targets: Vec<_> = ranges
                    .iter()
                    .map(|alt| self.compile_match(alt.clone(), f_idx))
                    .collect();

                for target in targets {
                    self.push_edge(i_idx, Match::EPSILON, target.unwrap());
                }

                OptionUsize::some(i_idx)
            }
            HirKind::Anchor(_) => todo!(),
            HirKind::WordBoundary(_) => todo!(),
            HirKind::Repetition(rep) => match rep.kind {
                RepetitionKind::ZeroOrOne
                | RepetitionKind::OneOrMore
                | RepetitionKind::ZeroOrMore => {
                    let s_out = StateIdx(self.states.len());
                    self.states.push(State {
                        first_edge: OptionUsize::none(),
                        is_accept: false,
                    });

                    let s_in = self.compile(&rep.hir, s_out).unwrap();
                    self.push_edge(s_out, Match::EPSILON, accept);
                    if matches!(
                        rep.kind,
                        RepetitionKind::OneOrMore | RepetitionKind::ZeroOrMore
                    ) {
                        self.push_edge(s_out, Match::EPSILON, s_in);
                    }

                    let i = StateIdx(self.states.len());
                    self.states.push(State {
                        first_edge: OptionUsize::none(),
                        is_accept: false,
                    });

                    self.push_edge(i, Match::EPSILON, s_in);
                    if matches!(
                        rep.kind,
                        RepetitionKind::ZeroOrOne | RepetitionKind::ZeroOrMore
                    ) {
                        self.push_edge(i, Match::EPSILON, accept);
                    }

                    OptionUsize::some(i)
                }
                RepetitionKind::Range(_) => todo!(),
            },
            HirKind::Group(group) => self.compile(&group.hir, accept),
            HirKind::Concat(exprs) => {
                let mut accept = accept;
                for expr in exprs.iter().rev() {
                    accept = self.compile(expr, accept).unwrap();
                }
                OptionUsize::some(accept)
            }
            HirKind::Alternation(alternations) => {
                // `f` is the input state than has epsilon edges from each alternation and
                // has one edge to the accepting state
                let f_idx = StateIdx(self.states.len());
                self.states.push(State {
                    first_edge: OptionUsize::none(),
                    is_accept: false,
                });
                self.push_edge(f_idx, Match::EPSILON, accept);

                // `i` is the input state than has epsilon edges to each alternation
                let i_idx = StateIdx(self.states.len());
                self.states.push(State {
                    first_edge: OptionUsize::none(),
                    is_accept: false,
                });

                let targets: Vec<_> = alternations
                    .iter()
                    .map(|alt| self.compile(alt, f_idx))
                    .collect();

                for target in targets {
                    self.push_edge(i_idx, Match::EPSILON, target.unwrap());
                }

                OptionUsize::some(i_idx)
            }
        }
    }

    fn compile_match(&mut self, m: Match, accept: StateIdx) -> OptionUsize<StateIdx> {
        let from = StateIdx(self.states.len());
        self.states.push(State {
            first_edge: OptionUsize::none(),
            is_accept: false,
        });

        self.push_edge(from, Some(m), accept);

        OptionUsize::some(from)
    }
}

// Simulation
impl NFA {
    fn starting_state(&self) -> HashSet<StateIdx> {
        let mut start = HashSet::new();
        start.insert(Self::START_IDX);
        self.epsilon_closure(start)
    }

    pub fn simulate(&self, s: &str) -> bool {
        let mut state_set = self.starting_state();

        for c in s.chars() {
            state_set = self.epsilon_closure(self.move_state(state_set, c));
        }

        let accepting_states = {
            self.states
                .iter()
                .enumerate()
                .filter_map(|(i, state)| {
                    if state.is_accept {
                        Some(StateIdx(i))
                    } else {
                        None
                    }
                })
                .collect()
        };

        Self::has_accepting_states(state_set.iter().map(|s| *s).collect(), accepting_states)
    }

    fn move_state(&self, state_set: HashSet<StateIdx>, c: char) -> HashSet<StateIdx> {
        let mut ret = HashSet::new();

        for state_idx in state_set.into_iter() {
            let state = &self.states[state_idx.0];
            if state.first_edge.is_none() {
                continue;
            }

            let mut edge_idx = state.first_edge.unwrap();
            while edge_idx < self.edges.len() {
                let edge = &self.edges[edge_idx];
                edge_idx += 1;
                if edge.from != state_idx {
                    break;
                }

                match &edge.label {
                    Some(label_match) => {
                        if label_match.matches(c) {
                            ret.insert(edge.to);
                        }
                    }
                    _ => (),
                }
            }
        }

        ret
    }

    pub fn epsilon_closure(&self, state_set: HashSet<StateIdx>) -> HashSet<StateIdx> {
        let mut ret = HashSet::with_capacity(state_set.len() + 4);

        let mut stack: Vec<StateIdx> = state_set.into_iter().collect();

        while !stack.is_empty() {
            // SAFETY:
            // We loop while the stack is non empty so this will never return None
            let state_idx = unsafe { stack.pop().unwrap_unchecked() };
            ret.insert(state_idx);

            let state = &self.states[state_idx.0];
            if state.first_edge.is_none() {
                continue;
            }

            let mut edge_idx = state.first_edge.unwrap();
            while edge_idx < self.edges.len() {
                let edge = &self.edges[edge_idx];
                edge_idx += 1;
                if edge.from != state_idx {
                    break;
                }

                match &edge.label {
                    None => {
                        if ret.contains(&edge.to) {
                            continue;
                        }
                        stack.push(edge.to);
                    }
                    _ => (),
                }
            }
        }

        ret
    }

    fn has_accepting_states(
        final_states: HashSet<StateIdx>,
        accepting_states: HashSet<StateIdx>,
    ) -> bool {
        if final_states.is_empty() {
            return false;
        }

        final_states
            .into_iter()
            .any(|state| accepting_states.contains(&state))
    }
}

// Utility
impl NFA {
    pub fn lex_nfa_template(token_nfa_lengths: &[(usize, usize)]) -> Self {
        let edges = Vec::with_capacity(
            token_nfa_lengths
                .iter()
                .fold(0, |acc, (_, edge_len)| acc + *edge_len),
        );
        let states = vec![
            // start state
            State {
                first_edge: OptionUsize::none(),
                is_accept: false,
            },
        ];
        let mut this = Self { edges, states };

        let mut state_offset = 1;
        for (state_len, _) in token_nfa_lengths {
            this.push_edge(
                Self::START_IDX,
                None,
                StateIdx(Self::START_IDX.0 + state_offset),
            );
            state_offset += state_len;
        }

        this
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
    }

    pub fn labels(&self) -> Vec<&Match> {
        let mut ret = vec![];

        for (i, _) in self.states.iter().enumerate() {
            if let Some(edge_iter) = self.edge_iter(StateIdx(i)) {
                for edge in edge_iter {
                    if let Some(label) = &edge.label {
                        ret.push(label)
                    }
                }
            }
        }

        ret
    }

    pub fn accepting_states(&self) -> BTreeSet<StateIdx> {
        let mut ret = BTreeSet::new();
        for (i, s) in self.states.iter().enumerate() {
            if s.is_accept {
                ret.insert(StateIdx(i));
            }
        }
        ret
    }

    pub fn start_state(&self) -> &State {
        &self.states[Self::START_IDX.0]
    }

    pub fn greatest_state_idx(&self) -> StateIdx {
        StateIdx(self.states.len() - 1)
    }

    pub fn edge_iter(&self, state_idx: StateIdx) -> Option<EdgeIter> {
        let edge_idx = self.states[state_idx.0].first_edge.unwrap_or(usize::MAX);
        if edge_idx == usize::MAX {
            return None;
        }

        Some(EdgeIter {
            nfa: self,
            state: state_idx,
            edge_idx,
        })
    }
}

pub struct EdgeIter<'a> {
    nfa: &'a NFA,
    state: StateIdx,
    edge_idx: usize,
}

impl<'a> Iterator for EdgeIter<'a> {
    type Item = &'a Edge;

    fn next(&mut self) -> Option<Self::Item> {
        let edge = self.nfa.edges.get(self.edge_idx)?;
        if edge.from != self.state {
            return None;
        }
        self.edge_idx += 1;

        Some(edge)
    }
}

impl std::fmt::Debug for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Match")
            .field("range", &self.range)
            .field("(pretty range)", &self.range_to_string())
            .finish()
    }
}

impl From<Range<u32>> for Match {
    fn from(range: Range<u32>) -> Self {
        Self { range }
    }
}

impl Ord for Match {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.compare(other)
    }
}

impl PartialOrd for Match {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.compare(other))
    }
}

impl Match {
    pub const EPSILON: Option<Self> = None;

    pub fn contains_u32(&self, c: u32) -> bool {
        c >= self.range.start && c < self.range.end
    }

    pub fn intersects(&self, r: &Self) -> bool {
        !self.range.is_empty()
            && !r.range.is_empty()
            && (self.contains_u32(r.range.start) || r.contains_u32(self.range.start))
    }

    pub fn is_disjoint(&self, r: &Self) -> bool {
        !self.intersects(r)
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        match self.range.start.cmp(&other.range.start) {
            std::cmp::Ordering::Less => std::cmp::Ordering::Less,
            std::cmp::Ordering::Equal => self.range.end.cmp(&other.range.end),
            std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
        }
    }

    fn range_to_string(&self) -> String {
        let mut ret = String::with_capacity(self.range.len().min(256));

        for char_code in self.range.clone().into_iter().take(256) {
            ret.push(char_code as u8 as char);
        }

        if self.range.len() > 256 {
            ret += " (rest omitted)"
        }
        ret
    }

    pub fn char(c: char) -> Self {
        Self {
            range: (c as u32)..(c as u32 + 1),
        }
    }

    pub fn byte(b: u8) -> Self {
        Self {
            range: (b as u32)..(b as u32 + 1),
        }
    }

    pub fn matches(&self, c: char) -> bool {
        let c = c as u32;

        c >= self.range.start && c < self.range.end
    }
}

#[cfg(test)]
mod test {
    use regex_syntax::Parser;

    use crate::nfa::NFA;

    #[test]
    fn test() {
        let hir = Parser::new().parse("lolfucku").unwrap();

        let nfa = NFA::from_regex(&hir);

        assert!(nfa.simulate("lolfucku"));
    }

    #[test]
    fn simple() {
        let hir = Parser::new().parse("lolfucku✅").unwrap();

        let nfa = NFA::from_regex(&hir);

        assert!(nfa.simulate("lolfucku✅"));
    }

    #[test]
    fn alternation() {
        let hir = Parser::new().parse("lmao|lol|bruh").unwrap();

        let nfa = NFA::from_regex(&hir);

        assert!(nfa.simulate("lmao"));
        assert!(nfa.simulate("lol"));
        assert!(nfa.simulate("bruh"));

        assert!(!nfa.simulate("dflsdjf"))
    }

    #[test]
    fn zero_or_more() {
        let hir = Parser::new().parse("a*bcd").unwrap();

        let nfa = NFA::from_regex(&hir);

        assert!(nfa.simulate("bcd"));
        assert!(nfa.simulate("abcd"));
        assert!(nfa.simulate("aaaabcd"));
        assert!(nfa.simulate("aaaaaaabcd"));

        assert!(!nfa.simulate("cbcd"));
    }

    #[test]
    fn one_or_more() {
        let hir = Parser::new().parse("a+bcd").unwrap();

        let nfa = NFA::from_regex(&hir);

        assert!(!nfa.simulate("bcd"));

        assert!(nfa.simulate("abcd"));
        assert!(nfa.simulate("aaaabcd"));
        assert!(nfa.simulate("aaaaaaabcd"));

        assert!(!nfa.simulate("cbcd"));
    }

    #[test]
    fn optional() {
        let hir = Parser::new().parse("a?bcd").unwrap();

        let nfa = NFA::from_regex(&hir);

        assert!(nfa.simulate("bcd"));

        assert!(nfa.simulate("abcd"));
        assert!(!nfa.simulate("aaaabcd"));
        assert!(!nfa.simulate("aaaaaaabcd"));

        assert!(!nfa.simulate("cbcd"));
    }
}
