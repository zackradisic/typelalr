pub struct ParserSupport<'ast> {
    ast: &'ast bumpalo::Bump,
}

impl<'ast> ParserSupport<'ast> {
    pub fn new(ast: &'ast bumpalo::Bump) -> Self {
        Self { ast }
    }

    pub fn alloc<T>(&mut self, val: T) -> &'ast mut T {
        self.ast.alloc(val)
    }

    pub fn alloc_str<'a>(&mut self, s: &'a str) -> &'ast mut str {
        self.ast.alloc(s.chars().collect::<String>())
    }

    pub fn alloc_quoted_str<'a>(&mut self, s: &'a str) -> &'ast mut str {
        self.ast.alloc(
            s.chars()
                .skip(1)
                .take(s.chars().count() - 2)
                .collect::<String>(),
        )
    }

    pub fn alloc_ts_code<'a>(&mut self, ts_code_with_paren: &'a str) -> &'ast mut str {
        self.ast.alloc(
            ts_code_with_paren
                .chars()
                .skip(1)
                .take(ts_code_with_paren.chars().count() - 2)
                .collect::<String>(),
        )
    }

    pub fn alloc_regex<'a>(&mut self, regex_str_with_quotes: &'a str) -> &'ast mut str {
        if regex_str_with_quotes.len() < 3 {
            return self.ast.alloc("".to_owned());
        }

        let regex_trimmed = regex_str_with_quotes
            .chars()
            .skip(1)
            .take(regex_str_with_quotes.chars().count() - 2)
            .collect::<String>();

        self.ast.alloc(regex_trimmed)
    }
}
