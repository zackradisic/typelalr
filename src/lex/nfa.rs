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

        Self::has_accepting_states(state_set.iter().copied().collect(), accepting_states)
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
    use std::{collections::BTreeSet, ops::Range};

    use regex_syntax::{
        hir::{Class, HirKind},
        Parser,
    };

    use crate::lex::nfa::NFA;

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

    #[test]
    fn negate() {
        let hir = Parser::new().parse("[^a]*").unwrap();

        let nfa = NFA::from_regex(&hir);

        assert!(nfa.simulate("bcd"));
        assert!(nfa.simulate("cbcd"));

        assert!(!nfa.simulate("abcd"));
        assert!(!nfa.simulate("aaaabcd"));
        assert!(!nfa.simulate("aaaaaaabcd"));
    }

    #[test]
    fn lol() {
        let hir = Parser::new().parse(".").unwrap();
        let ranges = match hir.kind() {
            HirKind::Class(Class::Unicode(uni)) => uni
                .ranges()
                .iter()
                .map(|range| range.start() as u32..(range.end() as u32 + 1))
                .collect::<Vec<_>>(),
            _ => panic!(),
        };

        fn range_to_string(range: Range<u32>) -> String {
            let mut ret = String::with_capacity(range.len());

            for char_code in range {
                ret.push(char_code as u8 as char);
            }

            ret
        }

        let range_chars: Vec<String> = ranges.into_iter().map(range_to_string).collect();
        println!(
            "FUCK: {:#?}",
            BTreeSet::from_iter(range_chars.iter().flat_map(|s| s.chars()))
        );
        let mut final_str = "type Characters = [".to_owned();

        let unicode = [
            9, 10, 11, 12, 13, 32, 127, 160, 173, 847, 1564, 4447, 4448, 6068, 6069, 6155, 6156,
            6157, 6158, 7355, 7356, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199, 8200, 8201,
            8202, 8203, 8204, 8205, 8206, 8207, 8234, 8235, 8236, 8237, 8238, 8239, 8287, 8288,
            8289, 8290, 8291, 8292, 8293, 8294, 8295, 8296, 8297, 8298, 8299, 8300, 8301, 8302,
            8303, 10240, 12288, 12644, 65024, 65025, 65026, 65027, 65028, 65029, 65030, 65031,
            65032, 65033, 65034, 65035, 65036, 65037, 65038, 65039, 65279, 65440, 65520, 65521,
            65522, 65523, 65524, 65525, 65526, 65527, 65528, 65532, 78844, 119155, 119156, 119157,
            119158, 119159, 119160, 119161, 119162, 917504, 917505, 917506, 917507, 917508, 917509,
            917510, 917511, 917512, 917513, 917514, 917515, 917516, 917517, 917518, 917519, 917520,
            917521, 917522, 917523, 917524, 917525, 917526, 917527, 917528, 917529, 917530, 917531,
            917532, 917533, 917534, 917535, 917536, 917537, 917538, 917539, 917540, 917541, 917542,
            917543, 917544, 917545, 917546, 917547, 917548, 917549, 917550, 917551, 917552, 917553,
            917554, 917555, 917556, 917557, 917558, 917559, 917560, 917561, 917562, 917563, 917564,
            917565, 917566, 917567, 917568, 917569, 917570, 917571, 917572, 917573, 917574, 917575,
            917576, 917577, 917578, 917579, 917580, 917581, 917582, 917583, 917584, 917585, 917586,
            917587, 917588, 917589, 917590, 917591, 917592, 917593, 917594, 917595, 917596, 917597,
            917598, 917599, 917600, 917601, 917602, 917603, 917604, 917605, 917606, 917607, 917608,
            917609, 917610, 917611, 917612, 917613, 917614, 917615, 917616, 917617, 917618, 917619,
            917620, 917621, 917622, 917623, 917624, 917625, 917626, 917627, 917628, 917629, 917630,
            917631, 917760, 917761, 917762, 917763, 917764, 917765, 917766, 917767, 917768, 917769,
            917770, 917771, 917772, 917773, 917774, 917775, 917776, 917777, 917778, 917779, 917780,
            917781, 917782, 917783, 917784, 917785, 917786, 917787, 917788, 917789, 917790, 917791,
            917792, 917793, 917794, 917795, 917796, 917797, 917798, 917799, 917800, 917801, 917802,
            917803, 917804, 917805, 917806, 917807, 917808, 917809, 917810, 917811, 917812, 917813,
            917814, 917815, 917816, 917817, 917818, 917819, 917820, 917821, 917822, 917823, 917824,
            917825, 917826, 917827, 917828, 917829, 917830, 917831, 917832, 917833, 917834, 917835,
            917836, 917837, 917838, 917839, 917840, 917841, 917842, 917843, 917844, 917845, 917846,
            917847, 917848, 917849, 917850, 917851, 917852, 917853, 917854, 917855, 917856, 917857,
            917858, 917859, 917860, 917861, 917862, 917863, 917864, 917865, 917866, 917867, 917868,
            917869, 917870, 917871, 917872, 917873, 917874, 917875, 917876, 917877, 917878, 917879,
            917880, 917881, 917882, 917883, 917884, 917885, 917886, 917887, 917888, 917889, 917890,
            917891, 917892, 917893, 917894, 917895, 917896, 917897, 917898, 917899, 917900, 917901,
            917902, 917903, 917904, 917905, 917906, 917907, 917908, 917909, 917910, 917911, 917912,
            917913, 917914, 917915, 917916, 917917, 917918, 917919, 917920, 917921, 917922, 917923,
            917924, 917925, 917926, 917927, 917928, 917929, 917930, 917931, 917932, 917933, 917934,
            917935, 917936, 917937, 917938, 917939, 917940, 917941, 917942, 917943, 917944, 917945,
            917946, 917947, 917948, 917949, 917950, 917951, 917952, 917953, 917954, 917955, 917956,
            917957, 917958, 917959, 917960, 917961, 917962, 917963, 917964, 917965, 917966, 917967,
            917968, 917969, 917970, 917971, 917972, 917973, 917974, 917975, 917976, 917977, 917978,
            917979, 917980, 917981, 917982, 917983, 917984, 917985, 917986, 917987, 917988, 917989,
            917990, 917991, 917992, 917993, 917994, 917995, 917996, 917997, 917998, 917999,
        ];
        let invisible_codepoints: BTreeSet<usize> = BTreeSet::from(unicode);

        let mut char_set: BTreeSet<char> = BTreeSet::new();
        for r in range_chars {
            for char in r.chars() {
                if invisible_codepoints.contains(&(char as usize)) {
                    continue;
                }
                char_set.insert(char);
            }
        }

        for char in char_set {
            final_str += "\n\"";

            if char == '"' {
                final_str += "\\\"";
            } else if char == '\\' {
                final_str += "\\\\"
            } else {
                final_str.push(char);
            }
            final_str += "\",";
        }

        final_str.push(']');

        std::fs::write("./chars.ts", final_str).unwrap();
        println!("HIR: {:?}", hir);
    }
}
