use crate::{lalr::Lalr, parser::ast};
use bumpalo::Bump;

use super::lex_gen;

mod ts {

    use bumpalo::Bump;

    use crate::{
        lalr::{item::ProductionIdx, table, Lalr},
        parser::{
            ast::{self, Ident, InputSymbol, NamedSymbol},
            Grammar, Symbol,
        },
    };
    use std::{collections::BTreeMap, fmt::Write};

    fn write_condition<W: std::fmt::Write>(
        w: &mut W,
        mut cond: impl FnMut(&mut W) -> std::fmt::Result,
        then: impl Fn(&mut W) -> std::fmt::Result,
        mut otherwise: impl FnMut(&mut W) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cond(w)?;
        w.write_str(" ? ")?;
        then(w)?;
        w.write_str(" : ")?;
        otherwise(w)
    }

    pub struct Ctx<'ast> {
        bump: &'ast Bump,
        symbols: Vec<&'ast str>,
        symbol_variants: BTreeMap<&'ast str, SymbolVariant<'ast>>,
        actions: Vec<Action<'ast>>,
        action_impls: Vec<&'ast ActionImpl<'ast>>,
        lalr: &'ast Lalr<'ast>,
        grammar: &'ast Grammar<'ast>,
    }

    struct SymbolVariant<'ast> {
        production_name: &'ast str,
        ts_code: &'ast str,
    }

    struct Action<'ast> {
        action_impl: &'ast ActionImpl<'ast>,
        pop_amount: usize,
        production_idx: ProductionIdx,
        production_name: Ident<'ast>,
    }

    struct ActionImpl<'ast> {
        name: Ident<'ast>,
        production_idx: ProductionIdx,
        code: &'ast str,
        inputs: Vec<ActionImplParam<'ast>>,
    }

    enum ActionImplParam<'ast> {
        Named(&'ast NamedActionImplParam<'ast>),
        NonTerminal(&'ast str),
        StrLit(&'ast str),
        Regex(&'ast str),
    }

    struct NamedActionImplParam<'ast> {
        name: Ident<'ast>,
        param: ActionImplParam<'ast>,
    }

    impl<'ast> Ctx<'ast> {
        pub fn new(
            bump: &'ast Bump,
            ast_productions: &'ast [ast::Production<'ast>],
            lalr: &'ast Lalr<'ast>,
        ) -> Self {
            let symbol_variants: BTreeMap<_, _> = ast_productions
                .iter()
                .map(|ast_prod| {
                    (
                        ast_prod.name.as_ref(),
                        SymbolVariant {
                            production_name: ast_prod.name.as_ref(),
                            ts_code: ast_prod.ts_code,
                        },
                    )
                })
                .collect();

            let starting_production_idx = 1;
            let action_impls: Vec<_> = ast_productions
                .iter()
                .flat_map(|ast_prod| ast_prod.bodies.iter().map(|body| (ast_prod.name, body)))
                .enumerate()
                .map(|(i, (name, body))| {
                    bump.alloc(ActionImpl {
                        name,
                        production_idx: ProductionIdx((starting_production_idx + i) as u32),
                        code: body.mapping_fn,
                        inputs: body
                            .input_tokens
                            .iter()
                            .filter_map(|symbol| ActionImplParam::from_input_symbols(bump, symbol))
                            .collect(),
                    }) as &'ast _
                })
                .collect();

            let actions: Vec<_> = action_impls
                .iter()
                .map(|action_impl| Action {
                    action_impl,
                    pop_amount: action_impl.inputs.len(),
                    production_idx: action_impl.production_idx,
                    production_name: action_impl.name,
                })
                .collect();

            let symbols = ast_productions
                .iter()
                .map(|production| production.name.0)
                .collect();

            Self {
                bump,
                symbol_variants,
                action_impls,
                actions,
                symbols,
                lalr,
                grammar: &lalr.grammar,
            }
        }

        pub fn generate(&self) -> Result<String, std::fmt::Error> {
            let mut ret = String::new();

            self.generate_type_defs_module(&mut ret)?;
            self.generate_imports(&mut ret)?;
            self.generate_actions_module(&mut ret)?;

            Ok(ret)
        }

        fn generate_type_defs_module<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            w.write_str("export declare module type_defs {\n")?;
            self.generate_symbols(w)?;
            self.generate_tokens(w)?;
            self.generate_productions(w)?;
            self.generate_action_table(w)?;
            self.generate_goto_table(w)?;
            self.generate_tokenkind_to_idx(w)?;
            w.write_str("\n}\n")
        }

        fn generate_imports<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            w.write_str("import * as U from \"./util\"\n")?;
            w.write_str("import * as L from \"./lex\"\n")?;
            w.write_str("\n")?;

            Ok(())
        }

        fn generate_symbols<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            w.write_str(r#"export type Symbol = { kind: "token", token: L.Token }"#)?;

            for sym in &self.symbols {
                write!(w, r#"| {{ kind: "{}", value: actions.{} }}"#, sym, sym)?;
            }

            w.write_str("\n\n")
        }

        fn generate_tokens<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            // Declaration first
            w.write_str(
                r#"
export type Token =
  | { kind: "strlit"; value: string }
  | { kind: "nonterminal"; value: string }
  | { kind: "regex"; value: string }
  | { kind: "eof" };
  
  "#,
            )?;

            w.write_str("export type Tokens = {")?;

            for (i, token) in self.grammar.tokens().enumerate() {
                let (kind, value) = match token {
                    Symbol::StrLit(strlit) => ("strlit", *strlit),
                    Symbol::NonTerminal(nonterminal) => ("nonterminal", nonterminal.0),
                    Symbol::Regex(r) => ("regex", *r),
                    Symbol::Epsilon => todo!(),
                    Symbol::Eof => {
                        write!(w, "\n{}: {{ kind: \"eof\" }}", i)?;
                        continue;
                    }
                };
                write!(
                    w,
                    r#"
                {}: {{ kind: "{}", value: "{}" }}"#,
                    i, kind, value
                )?;
            }

            w.write_str("\n}\n\n")
        }

        fn generate_productions<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            // Type declaration first
            w.write_str(
                r#"export type Production = {
                name: string;
                tokens: Array<keyof Tokens>;
              };
              
              "#,
            )?;

            w.write_str("export type Productions = {")?;
            for (production_idx, production) in self.grammar.iter_productions().enumerate() {
                write!(
                    w,
                    r#"{}: {{ name: "{}", tokens: [{}"#,
                    production_idx, production.name, ""
                )?;
                for token in &production.input_tokens {
                    let idx = self
                        .grammar
                        .get_token_idx(token)
                        .expect("Should have token idx");
                    write!(w, "{},", idx.0)?;
                }
                w.write_str("] }\n")?;
            }
            w.write_str("\n}\n\n")?;

            Ok(())
        }

        fn generate_action_table<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            w.write_str("export type Action = {")?;
            for (state_idx, action_map) in self.lalr.action.iter() {
                write!(w, "{}: {{", state_idx)?;
                for (token_idx, action) in action_map.iter() {
                    write!(w, "{}: {{", token_idx.0)?;
                    match action {
                        table::Action::Shift(j) => write!(w, "shift: {}", j),
                        table::Action::Reduce(r) => write!(w, "reduce: {}", r.0),
                        table::Action::Accept => w.write_str("accept: true"),
                    }?;
                    w.write_str("\n}\n")?;
                }
                w.write_str("\n}\n")?;
            }
            w.write_str("\n}\n")?;
            Ok(())
        }

        fn generate_goto_table<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            w.write_str("export type Goto = {")?;
            for (state_idx, goto_map) in self.lalr.goto.iter() {
                write!(w, "{}: {{", state_idx)?;
                for (production_name, new_state) in goto_map.iter() {
                    write!(w, "\n{}: {}\n", production_name.0, new_state)?;
                }
                w.write_str("\n}\n")?;
            }
            w.write_str("\n}\n")?;
            Ok(())
        }

        fn generate_tokenkind_to_idx<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            w.write_str("export type TokenKindToIdx = {")?;
            for (idx, terminal) in self
                .grammar
                .tokens()
                .enumerate()
                .filter(|(_, s)| Symbol::is_terminal(s))
            {
                let name = match terminal {
                    Symbol::StrLit(strlit) => strlit,
                    Symbol::Regex(r) => r,
                    Symbol::NonTerminal(_) => continue,
                    Symbol::Epsilon => continue,
                    Symbol::Eof => "EOF",
                };
                writeln!(w, "'{}': {}", name, idx)?;
            }
            w.write_str("\n}\n")?;
            Ok(())
        }

        fn generate_actions_module(&self, buf: &mut String) -> std::fmt::Result {
            writeln!(buf, "export declare module actions {{")?;

            // Symbol variants:
            // type S = { kind: "S"; left: C; right: C };
            for (_, symbol_variant) in &self.symbol_variants {
                writeln!(
                    buf,
                    "type {} = {}",
                    symbol_variant.production_name, symbol_variant.ts_code
                )?;
            }

            // EmitProduction type-level function
            write!(
                buf,
                "\ntype EmitProduction<prodIdx extends number, symbolStack extends type_defs.Symbol[]> = "
            )?;
            for action in &self.actions {
                write!(
                    buf,
                    "prodIdx extends {} ? actions.Action{}<symbolStack> :",
                    action.production_idx.0, action.production_idx.0
                )?;
            }
            writeln!(buf, "`error: invalid production idx: ${{prodIdx}}`;")?;

            // Action impls
            // for action_impl in &self.action_impls {
            //     write!(buf, "type Action{}Impl<", action_impl.production_idx.0)?;
            //     for input_symbol in &action_impl.inputs {
            //         if input_symbol.write(buf)? {
            //             buf.push_str(", ");
            //         }
            //     }
            //     writeln!(buf, "> = {}", action_impl.code)?;
            // }

            buf.push('\n');
            // Actions
            for action in &self.actions {
                action.write(buf)?;
                buf.push('\n');
            }

            write!(buf, "\n}}\n")?;
            Ok(())
        }
    }

    impl<'ast> Action<'ast> {
        fn call_impl_args<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            let mut i = 0;

            for sym in &self.action_impl.inputs {
                let named_sym = match sym {
                    ActionImplParam::Named(named) => named,
                    _ => continue,
                };

                match named_sym.param {
                    ActionImplParam::NonTerminal(_) => {
                        write!(w, "{}['value'],", named_sym.name)?;
                    }
                    ActionImplParam::Regex(_) | ActionImplParam::StrLit(_) => {
                        write!(w, "{}['token']['value'],", named_sym.name)?;
                        i += 1;
                    }
                    ActionImplParam::Named(_) => unreachable!(),
                }
            }

            Ok(())
        }

        fn call_impl(&self) -> Result<String, std::fmt::Error> {
            let mut buf = String::new();

            buf.push('[');

            let mut args = String::new();
            self.call_impl_args(&mut args)?;

            write!(
                buf,
                "{{ kind: '{}', value: {} }},",
                self.production_name, self.action_impl.code
            )?;
            buf.push_str("...newStack]");

            Ok(buf)
        }

        fn write<W: std::fmt::Write>(&self, w: &mut W) -> std::fmt::Result {
            // Function declaration
            write!(
                w,
                "type Action{}<symbolStack extends type_defs.Symbol[]> = ",
                self.production_idx.0
            )?;

            write_condition(
                w,
                |w| {
                    write!(w, "U.PopNStackWithValue<symbolStack, {}> extends [infer newStack, infer poppedValues]", self.pop_amount)
                },
                |w| {
                    // w.write_str("symbolStack")?;
                    self.popped_values(w, self.call_impl()?, self.action_impl.production_idx)?;

                    Ok(())
                },
                |w| write!(w, "'unreachable: inferring popped values never fails'"),
            )?;

            Ok(())
        }

        fn popped_values<W: std::fmt::Write>(
            &self,
            w: &mut W,
            call_impl: String,
            action_idx: ProductionIdx,
        ) -> std::fmt::Result {
            write_condition(
                w,
                |w| {
                    w.write_str("poppedValues extends [")?;
                    let mut i = 0;
                    for sym in &self.action_impl.inputs {
                        match sym {
                            ActionImplParam::Named(NamedActionImplParam { name, .. }) => {
                                write!(w, "infer __{},", name.as_ref())?
                            }
                            _ => {
                                write!(w, "infer var{},", i)?;
                                i += 1;
                            }
                        }
                    }
                    w.write_str("]")?;
                    Ok(())
                },
                |w| {
                    write_condition(
                        w,
                        |w| w.write_str("newStack extends type_defs.Symbol[]"),
                        |w| self.validate_params(w, call_impl.clone()),
                        |w| {
                            write!(
                                w,
                                r#"`unreachable: \`newStack\` should always be a type_defs.Symbol[] (Action{})`"#,
                                action_idx.0
                            )
                        },
                    )
                },
                |w| {
                    write!(
                        w,
                        "`error: popping off symbol Stack Action{}`",
                        self.action_impl.production_idx.0
                    )
                },
            )
        }

        fn validate_params<W: std::fmt::Write>(
            &self,
            w: &mut W,
            call_impl: String,
        ) -> std::fmt::Result {
            let mut cond_stack = vec![];
            let mut else_stack = vec![];

            let mut i = 0;
            for sym in &self.action_impl.inputs {
                i += 1;
                let named_sym = match sym {
                    ActionImplParam::Named(named) => named,
                    _ => continue,
                };

                match named_sym.param {
                    ActionImplParam::NonTerminal(non_terminal) => {
                        cond_stack.push(format!(
                            "__{} extends {{ kind : \"{}\", value: infer {} extends {} }}",
                            named_sym.name, non_terminal, named_sym.name, non_terminal
                        ));

                        else_stack.push(format!(
                            r#""error: invalid symbol (pos `{}`, Action{})""#,
                            i, self.production_idx.0
                        ));
                    }
                    ActionImplParam::StrLit(str_lit) => {
                        cond_stack.push(format!(
                            r#"__{} extends {{ kind: "token", token: L.Token }}"#,
                            named_sym.name
                        ));
                        cond_stack.push(format!(
                            "__{}['token']['value'] extends infer {} extends '{}'",
                            named_sym.name, named_sym.name, str_lit,
                        ));
                        else_stack.push(format!(
                            r#"`error: not a token (pos \`{}\`, Action{})`"#,
                            i, self.production_idx.0
                        ));
                        else_stack.push(format!(
                            r#"`error: invalid symbol (pos \`{}\`, Action{})`"#,
                            i, self.production_idx.0
                        ));
                    }
                    ActionImplParam::Regex(_) => {
                        cond_stack.push(format!(
                            r#"__{} extends {{ kind: "token", token: L.Token }}"#,
                            named_sym.name
                        ));
                        cond_stack.push(format!(
                            "__{}['token']['value'] extends infer {} extendsstring",
                            named_sym.name, named_sym.name
                        ));
                        else_stack.push(format!(
                            r#"`error: not a token (pos \`{}\`, Action{})`"#,
                            i, self.production_idx.0
                        ));
                        else_stack.push(format!(
                            r#"`error: invalid symbol (pos \`{}\`, Action{})`"#,
                            i, self.production_idx.0
                        ));
                    }
                    ActionImplParam::Named(_) => unreachable!(),
                };
            }

            for cond in cond_stack.into_iter() {
                w.write_str(&cond)?;
                w.write_str(" ? ")?;
            }
            // w.write_str(self.action_impl.code)?;
            w.write_str(&call_impl)?;
            for otherwise in else_stack.into_iter() {
                w.write_str(" : ")?;
                w.write_str(&otherwise)?;
            }

            Ok(())
        }
    }

    impl<'ast> ActionImplParam<'ast> {
        fn write<W: std::fmt::Write>(&self, w: &mut W) -> Result<bool, std::fmt::Error> {
            self.write_impl(w, true)
        }

        fn write_impl<W: std::fmt::Write>(
            &self,
            w: &mut W,
            is_top: bool,
        ) -> Result<bool, std::fmt::Error> {
            match self {
                ActionImplParam::Named(NamedActionImplParam { name, param }) if is_top => {
                    write!(w, "{} extends ", name.as_ref())?;
                    param.write_impl(w, false)?;
                    Ok(true)
                }
                ActionImplParam::NonTerminal(non_terminal) if !is_top => {
                    write!(w, "{}", non_terminal)?;
                    Ok(true)
                }
                ActionImplParam::StrLit(lit) if !is_top => {
                    write!(w, "\"{}\"", lit)?;
                    Ok(true)
                }
                ActionImplParam::Regex(_r) if !is_top => {
                    write!(w, "string")?;
                    Ok(true)
                }
                _ => Ok(false),
            }
        }

        pub fn from_input_symbols(
            bump: &'ast Bump,
            input_symbol: &'ast InputSymbol<'ast>,
        ) -> Option<Self> {
            match input_symbol {
                InputSymbol::StrLit(s) => Some(ActionImplParam::StrLit(s)),
                InputSymbol::NonTerminal(non_terminal) => {
                    Some(ActionImplParam::NonTerminal(non_terminal.as_ref()))
                }
                InputSymbol::Regex(r) => Some(ActionImplParam::Regex(r)),
                InputSymbol::Named(NamedSymbol { name, ty }) => {
                    Some(ActionImplParam::Named(bump.alloc(NamedActionImplParam {
                        name: name.unwrap(),
                        param: Self::from_input_symbols(bump, ty)?,
                    })))
                }
                InputSymbol::Epsilon => None,
                InputSymbol::Eof => None,
            }
        }
    }
}

pub fn generate<'ast>(
    bump: &'ast Bump,
    ast_productions: &[ast::Production<'ast>],
    lalr: &Lalr<'ast>,
) -> Result<(String, String), std::fmt::Error> {
    let ctx = ts::Ctx::new(bump, ast_productions, lalr);
    let lex_str = lex_gen::generate(&lalr.lexer);
    Ok((lex_str, ctx.generate()?))
}

#[cfg(test)]
mod test {
    use bumpalo::Bump;

    use crate::{lalr::Lalr, parse_grammar, parser::Grammar};

    use super::generate;

    #[test]
    fn basic() {
        let grammar_str = r#"
        export start S: ({ kind: "S", left: C, right: C }) = [ 
            <c1: C> <c2: C> => ({ kind: "S", left: c1, right: c2 })
        ]

        export C: ({ kind: "C", values: string[] }) = [
            <c1: "c"> <c2: C> => ({ kind: "C", values: [c1, ...c2['values']] }),
            <d: "d"> => ({ kind: "C", values: [d]})
        ]
    "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        let lalr = Lalr::new(grammar);

        let (lex_str, str) = generate(&bump, &ast, &lalr).unwrap();
        // println!("{}", str);
        std::fs::write("./ts/parse_state.gen.ts", str).unwrap();
        std::fs::write("./ts/lex_state.gen.ts", lex_str).unwrap();
    }

    #[test]
    fn math() {
        let grammar_str = r#"
        export start Expr: ({ kind: "Expr", left: Num, op: Op, right: Num }) = [ 
            <l: Num> <op: Op> <right: Num> => ({ kind: "Expr", left: l, right: right, op: op })
        ]

        export Op: ({ kind: "Op", op: "add" | "sub" | "div" | "mul" }) = [ 
            <plus: "+"> => ({ kind: "Op", op: "add" }),
            <sub: "-"> => ({ kind: "Op", op: "sub" }),
            <div: "/"> => ({ kind: "Op", op: "div" }),
            <mul: "*"> => ({ kind: "Op", op: "mul" }),
        ]

        export Num: ({ kind: "Num", value: string }) = [
            <num: r"[1-9][0-9]*"> => ({ kind: "Num", value: num })
        ]
    "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        let lalr = Lalr::new(grammar);

        let (lex, str) = generate(&bump, &ast, &lalr).unwrap();
        std::fs::write("./ts/parse_state.gen.ts", str).unwrap();
        std::fs::write("./ts/lex_state.gen.ts", lex).unwrap();
    }

    #[test]
    fn lisp() {
        let grammar_str = r#"
        export start SExpr: ({ kind: "SExpr", exprs: Exprs }) = [
            "(" <exprs: Exprs> ")" => ({ kind: "SExpr", exprs: exprs })
        ]

        export Exprs: (Expr[]) = [
            <eee: Expr> <es: Exprs> => ([eee, ...es]),
            <eee: Expr> => ([eee])
        ]

        export Expr: ({ kind: "int", value: "420" } | { kind: "symbol", value: "add" } | { kind: "sexpr", sexpr: SExpr }) = [ 
            <sexpr: SExpr> => ({ kind: "sexpr", sexpr: sexpr }),
            <int: Int> => ({ kind: "int", value: int }),
            <str: Symbol> => ({ kind: "symbol", value: str }),
        ]

        export Int: ("420") = [
            <int: "420"> => (int)
        ]

        export Symbol: ("add") = [
            <sym: "add"> => (sym)
        ]
    "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        let lalr = Lalr::new(grammar);
        lalr.parse("(add (add 420 420))");

        let (lex, str) = generate(&bump, &ast, &lalr).unwrap();
        // std::fs::write("./ts/parse_state2.gen.ts", str).unwrap();
        // std::fs::write("./ts/lex_state.gen.ts", lex).unwrap();
    }

    #[test]
    fn god_help_me() {
        let grammar_str = r#"
        export start SExpr: ({ kind: "SExpr", exprs: Exprs }) = [
            "(" <exprs: Exprs> ")" => ({ kind: "SExpr", exprs: exprs })
        ]

        export Exprs: (Expr[]) = [
            <eee: Expr> <es: Exprs> => ([eee, ...es]),
            <eee: Expr> => ([eee])
        ]

        export Expr: ({ kind: "symbol", value: "add" } | { kind: "int", value: string } | { kind: "sexpr", sexpr: SExpr }) = [ 
            <sexpr: SExpr> => ({ kind: "sexpr", sexpr: sexpr }),
            <str: "add"> => ({ kind: "symbol", value: str }),
            <int: r"[1-9]+[0-9]*"> => ({ kind: "int", value: int })
        ]
    "#;
        let bump = Bump::new();
        let ast = parse_grammar(&bump, grammar_str);
        let grammar = Grammar::grammar_from_ast_productions(&bump, &ast);

        let lalr = Lalr::new(grammar);
        lalr.parse("(add 420 (add 34 35))");

        let (lex, str) = generate(&bump, &ast, &lalr).unwrap();
        std::fs::write("./ts/parse_state2.gen.ts", str).unwrap();
        std::fs::write("./ts/lex_state.gen.ts", lex).unwrap();
    }
}
