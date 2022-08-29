use crate::lex::lex::TokenDef;
use regex_syntax::hir::Hir as RegexHir;
use regex_syntax::Parser as RegexParser;

pub type InitialTs = str;

#[derive(Debug, PartialEq)]
pub struct Production<'ast> {
    pub name: Ident<'ast>,
    pub bodies: Vec<ProductionBody<'ast>>,
    pub is_start: bool,
}

#[derive(Debug, PartialEq)]
pub struct ProductionBody<'ast> {
    pub input_tokens: Vec<InputToken<'ast>>,
    pub mapping_fn: &'ast str,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InputToken<'ast> {
    StrLit(&'ast str),
    Named(&'ast NamedInputToken<'ast>),
    Regex(&'ast str),
    Epsilon,
    Eof,
}

#[derive(Debug, PartialEq, Eq)]
pub struct NamedInputToken<'input> {
    pub name: Option<Ident<'input>>,
    pub ty: Ident<'input>,
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident<'ast>(pub &'ast str);

impl<'ast> Production<'ast> {
    // pub fn get_input_token(&self, token_idx: u32) ->
}

impl<'ast> InputToken<'ast> {
    pub fn to_token_def_with_regex(&self) -> Option<(TokenDef, RegexHir)> {
        match self {
            InputToken::StrLit(str_lit) => Some((
                TokenDef {
                    name: str_lit.to_string(),
                    with_val: false,
                },
                RegexParser::new().parse(str_lit).unwrap(),
            )),
            InputToken::Regex(regex) => Some((
                TokenDef {
                    name: regex.to_string(),
                    with_val: true,
                },
                RegexParser::new().parse(regex).unwrap(),
            )),
            InputToken::Named(NamedInputToken { name, ty }) => None,
            InputToken::Epsilon => None,
            InputToken::Eof => None,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            InputToken::Eof
            | InputToken::Epsilon
            | InputToken::Regex(_)
            | InputToken::StrLit(_) => true,
            InputToken::Named(_) => false,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            InputToken::Epsilon => 0,
            InputToken::StrLit(_) => 1,
            InputToken::Named(_) => 2,
            InputToken::Regex(_) => 3,
            InputToken::Eof => 4,
        }
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        match (self, other) {
            (Self::Epsilon, _) => Less,
            (Self::StrLit(a), Self::StrLit(b)) => a.cmp(b),
            (Self::Regex(a), Self::Regex(b)) => a.cmp(b),
            (Self::Named(a), Self::Named(b)) => match a.ty.cmp(&b.ty) {
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

impl<'ast> std::fmt::Display for InputToken<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputToken::StrLit(str_lit) => f.write_str(str_lit),
            InputToken::Named(NamedInputToken { ty, .. }) => f.write_str(ty.as_ref()),
            InputToken::Regex(regex) => f.write_str(regex),
            InputToken::Epsilon => f.write_str("ε"),
            InputToken::Eof => f.write_str("﹩"),
        }
    }
}

impl<'ast> Ord for InputToken<'ast> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.compare(other)
    }
}

impl<'ast> PartialOrd for InputToken<'ast> {
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
    use crate::parser::ast::Ident;

    #[test]
    fn test() {
        println!("SIZE: {}", std::mem::size_of::<Option<&&str>>());
    }
}
