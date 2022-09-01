use crate::lalr::item::TokenIdx;
use crate::lex::lex::TokenDef;
use regex_syntax::hir::Hir as RegexHir;
use regex_syntax::Parser as RegexParser;

pub type InitialTs = str;

#[derive(Debug, PartialEq)]
pub struct Production<'ast> {
    pub name: Ident<'ast>,
    pub bodies: Vec<ProductionBody<'ast>>,
    pub is_start: bool,
    pub ts_code: &'ast str,
}

#[derive(Debug, PartialEq)]
pub struct ProductionBody<'ast> {
    pub input_tokens: Vec<InputSymbol<'ast>>,
    pub mapping_fn: &'ast str,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InputSymbol<'ast> {
    StrLit(&'ast str),
    Named(&'ast NamedSymbol<'ast>),
    NonTerminal(Ident<'ast>),
    Regex(&'ast str),
    Epsilon,
    Eof,
}

#[derive(Debug, PartialEq, Eq)]
pub struct NamedSymbol<'ast> {
    pub name: Option<Ident<'ast>>,
    pub ty: &'ast InputSymbol<'ast>,
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident<'ast>(pub &'ast str);

impl<'ast> Production<'ast> {
    // pub fn get_input_token(&self, token_idx: u32) ->
}

impl<'ast> InputSymbol<'ast> {
    pub fn lower(&self) -> crate::parser::Symbol<'ast> {
        use crate::parser::Symbol::*;

        match self {
            InputSymbol::StrLit(a) => StrLit(a),
            InputSymbol::Named(NamedSymbol { ty, .. }) => ty.lower(),
            InputSymbol::NonTerminal(name) => NonTerminal(*name),
            InputSymbol::Regex(regex) => Regex(regex),
            InputSymbol::Epsilon => Epsilon,
            InputSymbol::Eof => Eof,
        }
    }

    pub fn to_token_def_with_regex(&self, token_idx: TokenIdx) -> Option<(TokenDef, RegexHir)> {
        let token_idx = token_idx.0;
        match self {
            InputSymbol::StrLit(str_lit) => Some((
                TokenDef {
                    name: str_lit.to_string(),
                    with_val: false,
                    token_idx: Some(token_idx),
                },
                RegexParser::new().parse(str_lit).unwrap(),
            )),
            InputSymbol::Regex(regex) => Some((
                TokenDef {
                    name: regex.to_string(),
                    with_val: true,
                    token_idx: Some(token_idx),
                },
                RegexParser::new().parse(regex).unwrap(),
            )),
            InputSymbol::Named(NamedSymbol { name: _, ty }) => {
                ty.to_token_def_with_regex(TokenIdx(token_idx))
            }
            InputSymbol::Epsilon => None,
            InputSymbol::Eof => None,
            InputSymbol::NonTerminal(_) => None,
        }
    }

    pub fn as_non_terminal(&self) -> Option<&Ident<'ast>> {
        match self {
            InputSymbol::Named(named) => named.ty.as_non_terminal(),
            InputSymbol::NonTerminal(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            InputSymbol::Eof
            | InputSymbol::Epsilon
            | InputSymbol::Regex(_)
            | InputSymbol::StrLit(_) => true,
            InputSymbol::Named(named) => named.ty.is_terminal(),
            InputSymbol::NonTerminal(_) => false,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            InputSymbol::Epsilon => 0,
            InputSymbol::StrLit(_) => 1,
            InputSymbol::Named(_) => 2,
            InputSymbol::NonTerminal(_) => 4,
            InputSymbol::Regex(_) => 5,
            InputSymbol::Eof => 6,
        }
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        match (self, other) {
            (Self::Epsilon, _) => Less,
            (Self::StrLit(a), Self::StrLit(b)) => a.cmp(b),
            (Self::Regex(a), Self::Regex(b)) => a.cmp(b),
            (Self::Named(a), Self::Named(b)) => match a.ty.cmp(b.ty) {
                Equal => match (a.name.as_ref(), b.name.as_ref()) {
                    (Some(a), Some(b)) => a.cmp(b),
                    (Some(_), None) => Greater,
                    (None, Some(_)) => Less,
                    _ => Less,
                },
                val => val,
            },
            _ => self.precedence().cmp(&other.precedence()),
        }
    }
}

impl<'ast> std::fmt::Display for InputSymbol<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputSymbol::StrLit(str_lit) => f.write_str(str_lit),
            InputSymbol::Named(NamedSymbol { ty, .. }) => f.write_fmt(format_args!("{:?}", ty)),
            InputSymbol::NonTerminal(non_terminal) => f.write_str(non_terminal.as_ref()),
            InputSymbol::Regex(regex) => f.write_str(regex),
            InputSymbol::Epsilon => f.write_str("ε"),
            InputSymbol::Eof => f.write_str("﹩"),
        }
    }
}

impl<'ast> Ord for InputSymbol<'ast> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.compare(other)
    }
}

impl<'ast> PartialOrd for InputSymbol<'ast> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.compare(other))
    }
}

impl<'ast> std::fmt::Display for Ident<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl<'ast> AsRef<str> for Ident<'ast> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn test() {
        println!("SIZE: {}", std::mem::size_of::<Option<&&str>>());
    }
}
