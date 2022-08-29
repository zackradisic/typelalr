use std::collections::BTreeSet;

use crate::parser::{ast::InputSymbol, Grammar};

pub fn first<'ast, I>(
    // bump: &'ast Bump,
    grammar: &'ast Grammar<'ast>,
    symbols: I,
) -> BTreeSet<&'ast InputSymbol<'ast>>
where
    I: Iterator<Item = &'ast InputSymbol<'ast>>,
{
    let mut set = BTreeSet::new();

    let mut prev_has_epsilon = true;
    for symbol in symbols {
        let _ = first_of_symbol(grammar, &mut set, symbol);
        prev_has_epsilon = set.remove(&InputSymbol::Epsilon);
        if !prev_has_epsilon {
            break;
        }
    }

    if prev_has_epsilon {
        set.insert(&InputSymbol::Epsilon);
    }

    set
}

/// Returns true if the set contains epsilon
fn first_of_symbol<'ast>(
    grammar: &'ast Grammar<'ast>,
    set: &mut BTreeSet<&'ast InputSymbol<'ast>>,
    symbol: &'ast InputSymbol<'ast>,
) -> bool {
    let non_terminal = match symbol.as_non_terminal() {
        Some(non_terminal) => non_terminal,
        None => {
            set.insert(symbol);
            return matches!(symbol, InputSymbol::Epsilon);
        }
    };

    let mut added_epsilon = false;
    for (_, production) in grammar.production_iter(*non_terminal) {
        'input_token_iter: for input_token in production.input_tokens.iter() {
            if !first_of_symbol(grammar, set, input_token) {
                break 'input_token_iter;
            } else {
                added_epsilon = true;
            }
        }
    }

    added_epsilon
}
