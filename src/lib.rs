use bumpalo::Bump;
use parser::ast;

pub mod codegen;
pub mod lalr;
pub mod lex;
pub mod option_usize;
pub mod parser;

pub fn parse_grammar<'ast>(bump: &'ast Bump, grammar_str: &str) -> Vec<ast::Production<'ast>> {
    let mut support = parser::ParserSupport::new(bump);
    let parser = parser::grammar::ProgramParser::new();

    let ast_productions = parser.parse(&mut support, grammar_str).unwrap();

    ast_productions
}

#[cfg(test)]
mod test {
    use bumpalo::Bump;

    use crate::{
        lex::lex::Lex,
        parser::{self, Grammar},
    };

    #[test]
    fn basic() {
        let grammar_str = r#"
            export Foo = [ 
                "hello" "how" "are" "you" => ({ hello: "sir" }),
                r"[0-9]+" => ({ hello: "sir" }),
            ]
        "#;
        // let grammar_str = r#"
        //     export Foo = [
        //         "hi" => ({ hello: "sir" }),
        //         r"[0-9]+" => ({ hello: "sir" }),
        //         <left: Expr> "+" <right: Expr> => ({ kind: "plus", left, right })
        //     ]
        // "#;

        let bump = Bump::new();
        let mut support = parser::ParserSupport::new(&bump);
        let parser = parser::grammar::ProgramParser::new();

        let ast_productions = parser.parse(&mut support, grammar_str).unwrap();
        let productions = Grammar::grammar_from_ast_productions(&bump, &ast_productions);

        let lex = Lex::from_grammar(&productions);

        println!("AST: {:#?}", productions);

        println!("LEX: {:#?}", lex);

        let tokens = lex.lex("hello how are you 420");
        println!("TOKENS: {:?}", tokens);
    }
}
