use std::collections::BTreeSet;

use crate::parser::{ast::InputToken, Grammar};

pub fn first<'ast, I>(
    // bump: &'ast Bump,
    grammar: &'ast Grammar<'ast>,
    symbols: I,
) -> BTreeSet<&'ast InputToken<'ast>>
where
    I: Iterator<Item = &'ast InputToken<'ast>>,
{
    let mut set = BTreeSet::new();

    let mut prev_has_epsilon = true;
    for symbol in symbols {
        let _ = first_of_symbol(grammar, &mut set, symbol);
        prev_has_epsilon = set.remove(&InputToken::Epsilon);
        if !prev_has_epsilon {
            break;
        }
    }

    if prev_has_epsilon {
        set.insert(&InputToken::Epsilon);
    }

    set
}

/// Returns true if the set contains epsilon
fn first_of_symbol<'ast>(
    grammar: &'ast Grammar<'ast>,
    set: &mut BTreeSet<&'ast InputToken<'ast>>,
    symbol: &'ast InputToken<'ast>,
) -> bool {
    if symbol.is_terminal() {
        set.insert(symbol);
        return matches!(symbol, InputToken::Epsilon);
    }

    let named_input_token = match symbol {
        InputToken::Named(named_input_token) => named_input_token,
        _ => unreachable!(),
    };

    let mut added_epsilon = false;
    for (_, production) in grammar.production_iter(named_input_token.ty) {
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
