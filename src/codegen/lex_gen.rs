use std::{
    borrow::Cow,
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    ops::Range,
};

use regex_syntax::{
    hir::{Class, HirKind},
    Parser,
};

use crate::lex::lex::Lex;

fn nice_case(s: &str) -> String {
    if s.is_empty() {
        return s.to_string();
    } else {
        s.chars()
            .next()
            .unwrap()
            .to_uppercase()
            .chain(s.chars().skip(1).map(char::to_lowercase).flatten())
            .collect()
    }
}

mod ts {
    use std::collections::{BTreeMap, BTreeSet};

    use crate::lex::{
        dfa::DFA,
        lex::Lex,
        nfa::{Match, StateIdx},
    };

    #[derive(Clone, Debug)]
    pub struct Ctx<'a> {
        pub tokens: Tokens<'a>,
        pub states: States,
        pub dfa_to_nfa: &'a BTreeMap<StateIdx, BTreeSet<StateIdx>>,
        chars: BTreeMap<char, u32>,
    }

    #[derive(Clone, Debug)]
    pub struct State {
        pub labels: Vec<(usize, Match)>,
        pub accepting: bool,
    }
    /// Mapping from state index to token kind name, if it has a value
    pub type Tokens<'a> = BTreeMap<usize, (&'a str, bool)>;
    pub type States = Vec<State>;

    impl<'a> Ctx<'a> {
        pub fn new(lex: &'a Lex, chars: BTreeMap<char, u32>) -> Self {
            println!("{:#?}", lex);

            let tokens: BTreeMap<usize, (&'a str, bool)> =
                BTreeMap::from_iter(lex.tokens.iter().flat_map(|(k, v)| {
                    lex.dfa.states.iter().enumerate().filter_map(|(idx, _)| {
                        if lex.accepting_dfa_to_nfa.get(&StateIdx(idx))?.contains(k) {
                            Some((k.value(), (v.name.as_str(), v.with_val)))
                        } else {
                            None
                        }
                    })
                }));

            let states = lex
                .dfa
                .states
                .iter()
                .enumerate()
                .map(|(idx, _)| State::from_lex_state(&lex.dfa, idx))
                .collect();

            Self {
                tokens,
                states,
                chars,
                dfa_to_nfa: &lex.dfa.dfa_to_nfa,
            }
        }

        pub fn to_string(&self) -> String {
            let mut ret = String::new();

            ret.push_str(
                r#"/**
            * These are generated DFA states for the lexer.
            * Don't edit this file.
            */
            
            "#,
            );

            for (i, (kind, has_value)) in self.tokens.iter() {
                ret.push_str(&format!(
                    r#"type TokenDef{} = {{
                    kind: "{}",
                    hasValue: {}
                }}
                
                "#,
                    i,
                    kind,
                    if *has_value { "true" } else { "false" }
                ));
            }

            ret.push_str("export type Tokens = {\n");
            for (i, _) in self.tokens.iter() {
                ret.push_str(&format!("  {}: TokenDef{};", *i, *i));
            }
            ret.push_str("\n}\n\n");

            for (i, state) in self.states.iter().enumerate() {
                let tos = state
                    .labels
                    .iter()
                    .map(|(i, _)| i.to_string())
                    .collect::<Vec<String>>();

                let labels = state
                    .labels
                    .iter()
                    .map(|(_, m)| format!("{{ from: {}, to: {} }}", m.range.start, m.range.end))
                    .collect::<Vec<String>>();

                ret.push_str(&format!(
                    r#"type State{} = {{
                    to: [{}],
                    labels: [{}],
                    accepting: {}
                }}
                
                "#,
                    i,
                    tos.join(","),
                    labels.join(","),
                    state.accepting
                ));
            }

            let states = self
                .states
                .iter()
                .enumerate()
                .map(|(i, _)| format!("State{}", i))
                .collect::<Vec<_>>();

            ret.push_str(&format!(
                "export type States = [{}];\n\n",
                states.join(", ")
            ));

            fn nfa_set_to_string(set: &BTreeSet<StateIdx>) -> String {
                format!(
                    "[ {} ]",
                    set.iter()
                        .map(|s| format!("{}", s.value()))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            let dfa_to_nfa_str = self
                .dfa_to_nfa
                .iter()
                .map(|(dfa, nfa)| format!("{}: {},\n", dfa.value(), nfa_set_to_string(nfa)))
                .collect::<String>();

            ret.push_str(&format!(
                "export type DFAtoNFA = {{\n  {}}}",
                dfa_to_nfa_str
            ));

            ret
        }
    }

    impl State {
        pub fn from_lex_state(dfa: &DFA, state_idx: usize) -> Self {
            let state_idx = StateIdx(state_idx);
            let mut labels = Vec::new();

            match dfa.edge_iter(state_idx) {
                Some(edge_iter) => {
                    for edge in edge_iter {
                        labels.push((edge.to.value(), edge.label.clone()));
                    }
                }
                None => (),
            }

            Self {
                labels,
                accepting: dfa.dfa_state_is_accepting(state_idx),
            }
        }
    }
}

pub fn generate(lex: Lex) -> String {
    let chars = generate_chars();
    let ctx = ts::Ctx::new(&lex, chars);

    ctx.to_string()
}

fn generate_chars() -> BTreeMap<char, u32> {
    let hir = Parser::new().parse(".").unwrap();
    let ranges = match hir.kind() {
        HirKind::Class(Class::Unicode(uni)) => uni
            .ranges()
            .iter()
            .map(|range| range.start() as u32..(range.end() as u32 + 1))
            .collect::<Vec<_>>(),
        _ => panic!(),
    };

    fn range_to_char_vec(r: Range<u32>) -> Vec<char> {
        let mut chars = Vec::with_capacity(r.len());
        for i in r.into_iter() {
            // just gonna support a limited amount of chars for now
            chars.push(i as u8 as char);
        }
        chars
    }

    let mut range_chars: Vec<char> = ranges
        .into_iter()
        .map(range_to_char_vec)
        .flatten()
        .collect();
    range_chars.push(' ');

    let invisible_codepoints = [
        9, /* 10 ,*/ 11, 12, 13, /* 32, */ 127, 160, 173, 847, 1564, 4447, 4448, 6068,
        6069, 6155, 6156, 6157, 6158, 7355, 7356, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199,
        8200, 8201, 8202, 8203, 8204, 8205, 8206, 8207, 8234, 8235, 8236, 8237, 8238, 8239, 8287,
        8288, 8289, 8290, 8291, 8292, 8293, 8294, 8295, 8296, 8297, 8298, 8299, 8300, 8301, 8302,
        8303, 10240, 12288, 12644, 65024, 65025, 65026, 65027, 65028, 65029, 65030, 65031, 65032,
        65033, 65034, 65035, 65036, 65037, 65038, 65039, 65279, 65440, 65520, 65521, 65522, 65523,
        65524, 65525, 65526, 65527, 65528, 65532, 78844, 119155, 119156, 119157, 119158, 119159,
        119160, 119161, 119162, 917504, 917505, 917506, 917507, 917508, 917509, 917510, 917511,
        917512, 917513, 917514, 917515, 917516, 917517, 917518, 917519, 917520, 917521, 917522,
        917523, 917524, 917525, 917526, 917527, 917528, 917529, 917530, 917531, 917532, 917533,
        917534, 917535, 917536, 917537, 917538, 917539, 917540, 917541, 917542, 917543, 917544,
        917545, 917546, 917547, 917548, 917549, 917550, 917551, 917552, 917553, 917554, 917555,
        917556, 917557, 917558, 917559, 917560, 917561, 917562, 917563, 917564, 917565, 917566,
        917567, 917568, 917569, 917570, 917571, 917572, 917573, 917574, 917575, 917576, 917577,
        917578, 917579, 917580, 917581, 917582, 917583, 917584, 917585, 917586, 917587, 917588,
        917589, 917590, 917591, 917592, 917593, 917594, 917595, 917596, 917597, 917598, 917599,
        917600, 917601, 917602, 917603, 917604, 917605, 917606, 917607, 917608, 917609, 917610,
        917611, 917612, 917613, 917614, 917615, 917616, 917617, 917618, 917619, 917620, 917621,
        917622, 917623, 917624, 917625, 917626, 917627, 917628, 917629, 917630, 917631, 917760,
        917761, 917762, 917763, 917764, 917765, 917766, 917767, 917768, 917769, 917770, 917771,
        917772, 917773, 917774, 917775, 917776, 917777, 917778, 917779, 917780, 917781, 917782,
        917783, 917784, 917785, 917786, 917787, 917788, 917789, 917790, 917791, 917792, 917793,
        917794, 917795, 917796, 917797, 917798, 917799, 917800, 917801, 917802, 917803, 917804,
        917805, 917806, 917807, 917808, 917809, 917810, 917811, 917812, 917813, 917814, 917815,
        917816, 917817, 917818, 917819, 917820, 917821, 917822, 917823, 917824, 917825, 917826,
        917827, 917828, 917829, 917830, 917831, 917832, 917833, 917834, 917835, 917836, 917837,
        917838, 917839, 917840, 917841, 917842, 917843, 917844, 917845, 917846, 917847, 917848,
        917849, 917850, 917851, 917852, 917853, 917854, 917855, 917856, 917857, 917858, 917859,
        917860, 917861, 917862, 917863, 917864, 917865, 917866, 917867, 917868, 917869, 917870,
        917871, 917872, 917873, 917874, 917875, 917876, 917877, 917878, 917879, 917880, 917881,
        917882, 917883, 917884, 917885, 917886, 917887, 917888, 917889, 917890, 917891, 917892,
        917893, 917894, 917895, 917896, 917897, 917898, 917899, 917900, 917901, 917902, 917903,
        917904, 917905, 917906, 917907, 917908, 917909, 917910, 917911, 917912, 917913, 917914,
        917915, 917916, 917917, 917918, 917919, 917920, 917921, 917922, 917923, 917924, 917925,
        917926, 917927, 917928, 917929, 917930, 917931, 917932, 917933, 917934, 917935, 917936,
        917937, 917938, 917939, 917940, 917941, 917942, 917943, 917944, 917945, 917946, 917947,
        917948, 917949, 917950, 917951, 917952, 917953, 917954, 917955, 917956, 917957, 917958,
        917959, 917960, 917961, 917962, 917963, 917964, 917965, 917966, 917967, 917968, 917969,
        917970, 917971, 917972, 917973, 917974, 917975, 917976, 917977, 917978, 917979, 917980,
        917981, 917982, 917983, 917984, 917985, 917986, 917987, 917988, 917989, 917990, 917991,
        917992, 917993, 917994, 917995, 917996, 917997, 917998, 917999,
    ];
    let invisible_codepoints: BTreeSet<usize> = BTreeSet::from(invisible_codepoints);

    let mut char_map: BTreeMap<char, u32> = BTreeMap::new();
    for char in range_chars {
        if invisible_codepoints.contains(&(char as usize)) {
            continue;
        }
        match char_map.entry(char) {
            Entry::Vacant(entry) => {
                entry.insert(char as u8 as u32);
            }
            Entry::Occupied(_) => (),
        }
    }

    char_map
}

fn chars_to_string(char_map: &BTreeMap<char, u32>) -> String {
    let mut final_str = "export type Characters = {".to_owned();

    fn char_escape(c: char) -> Cow<'static, str> {
        match c {
            '"' => Cow::Borrowed("\\\""),
            '\\' => Cow::Borrowed("\\\\"),
            '\n' => Cow::Borrowed(r#"\n"#),
            _ => Cow::Owned(c.to_string()),
        }
    }

    for (char, i) in char_map.iter() {
        let char = *char;
        final_str.push_str(&format!("\n    {}: \"{}\",\n", i, char_escape(char)));
    }
    final_str += "}";

    final_str += "\n export type CharTable = {";
    for (char, i) in char_map.iter() {
        let char = *char;
        final_str += "\n\"";

        final_str.push_str(&char_escape(char));

        final_str += &format!("\": {},", i);
    }
    final_str += "}";

    final_str
}

pub fn gen_arithmetic(buf: &mut String, max_nums: u32) {
    buf.push_str("export type SuccTable = {\n");
    for i in 0..max_nums {
        buf.push_str(&format!("    {}: {};\n", i, i + 1));
    }
    buf.push_str("\n}\n\n");

    buf.push_str("export type PredTable = {\n");
    for i in 0..max_nums {
        println!("{}", i);
        buf.push_str(&format!("    {}: {};\n", i + 1, i));
    }
    buf.push_str("\n}\n");
}

#[cfg(test)]
mod test {
    use regex_syntax::hir::Hir as RegexHir;
    use regex_syntax::Parser;

    use crate::{
        codegen::lex_gen::gen_arithmetic,
        lex::lex::{Lex, TokenDef},
    };

    use super::{chars_to_string, generate, generate_chars};

    #[test]
    fn arith() {
        let mut buf = String::with_capacity(4096);
        gen_arithmetic(&mut buf, 5000);
        println!("{}", buf);
    }

    #[test]
    fn generate__() {
        let chars = generate_chars();
        std::fs::write("./ts/chars.ts", chars_to_string(&chars)).unwrap();
    }

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

    #[test]
    fn gen() {
        let tokens = vec![
            new_token("LET", "let"),
            new_token_val("IDENTIFIER", "[A-Za-z_][A-Za-z_0-9]*"),
            new_token("EQ", "="),
            new_token_val("NUM", "[1-9][0-9]*"),
            new_token("SEMI-COLON", ";"),
        ];
        let lexer = Lex::from_tokens(tokens.clone());
        // let tokens = lexer.lex("let x = 420");
        // println!("TOKENS: {:?}", tokens);

        let str = generate(lexer);
        std::fs::write("./ts/lex_state.gen.ts", str).unwrap();
    }
}
