pub struct Scanner<'code> {
    code: &'code str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'code> Scanner<'code> {
    pub fn new(code: &'code str) -> Self {
        Self {
            code,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token<'code> {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        match self.advance() {
            b'(' => self.make_token(TokenType::LeftParen),
            b')' => self.make_token(TokenType::RightParen),
            b'{' => self.make_token(TokenType::LeftBrace),
            b'}' => self.make_token(TokenType::RightBrace),
            b';' => self.make_token(TokenType::Semicolon),
            b',' => self.make_token(TokenType::Comma),
            b'.' => self.make_token(TokenType::Dot),
            b'-' => self.make_token(TokenType::Minus),
            b'+' => self.make_token(TokenType::Plus),
            b'/' => self.make_token(TokenType::Slash),
            b'*' => self.make_token(TokenType::Star),
            b'!' if self.matches(b'=') => self.make_token(TokenType::BangEqual),
            b'!' => self.make_token(TokenType::Bang),
            b'=' if self.matches(b'=') => self.make_token(TokenType::EqualEqual),
            b'=' => self.make_token(TokenType::Equal),
            b'<' if self.matches(b'=') => self.make_token(TokenType::LessEqual),
            b'<' => self.make_token(TokenType::Less),
            b'>' if self.matches(b'=') => self.make_token(TokenType::GreaterEqual),
            b'>' => self.make_token(TokenType::Greater),
            b'"' => self.string(),
            c if is_digit(c) => self.number(),
            c if is_alpha(c) => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current == self.code.len()
    }

    fn make_token(&self, kind: TokenType) -> Token<'code> {
        Token::new(kind, self.lexeme(), self.line)
    }

    fn lexeme(&self) -> &'code str {
        &self.code[self.start..self.current]
    }

    fn error_token(&self, message: &'static str) -> Token<'static> {
        Token::new(TokenType::Error, message, self.line)
    }

    fn advance(&mut self) -> u8 {
        let char = self.peek();
        self.current += 1;
        char
    }

    fn peek(&self) -> u8 {
        self.char_at(self.current)
    }

    fn char_at(&self, index: usize) -> u8 {
        self.code.as_bytes()[index]
    }

    fn matches(&mut self, expected: u8) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'\n' => {
                    self.line += 1;
                    self.advance();
                }
                b'/' if self.peek_next() == b'/' => {
                    while !self.is_at_end() && self.peek() != b'\n' {
                        self.advance();
                    }
                }
                _ => return,
            };
        }
    }

    fn peek_next(&self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.char_at(self.current + 1)
        }
    }

    fn string(&mut self) -> Token<'code> {
        while !self.is_at_end() && self.peek() != b'"' {
            if self.peek() == b'\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error_token("Unterminated string.")
        } else {
            self.advance();
            self.make_token(TokenType::String)
        }
    }

    fn number(&mut self) -> Token<'code> {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' && is_digit(self.peek_next()) {
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token<'code> {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }

        self.make_token(self.keyword_type().unwrap_or(TokenType::Identifier))
    }

    fn keyword_type(&self) -> Option<TokenType> {
        match self.char_at(self.start) {
            b'a' => self.check_keyword(1, "nd", TokenType::And),
            b'c' => self.check_keyword(1, "lass", TokenType::Class),
            b'e' => self.check_keyword(1, "lse", TokenType::Else),
            b'f' if self.current - self.start > 1 => match self.char_at(self.start + 1) {
                b'a' => self.check_keyword(2, "lse", TokenType::False),
                b'o' => self.check_keyword(2, "r", TokenType::For),
                b'u' => self.check_keyword(2, "n", TokenType::Fun),
                _ => None,
            },
            b'i' => self.check_keyword(1, "f", TokenType::If),
            b'n' => self.check_keyword(1, "il", TokenType::Nil),
            b'o' => self.check_keyword(1, "r", TokenType::Or),
            b'p' => self.check_keyword(1, "rint", TokenType::Print),
            b'r' => self.check_keyword(1, "eturn", TokenType::Return),
            b's' => self.check_keyword(1, "uper", TokenType::Super),
            b't' if self.current - self.start > 1 => match self.char_at(self.start + 1) {
                b'h' => self.check_keyword(2, "is", TokenType::This),
                b'r' => self.check_keyword(2, "ue", TokenType::True),
                _ => None,
            },
            b'v' => self.check_keyword(1, "ar", TokenType::Var),
            b'w' => self.check_keyword(1, "hile", TokenType::While),
            _ => None,
        }
    }

    fn check_keyword(
        &self,
        keyword_start: usize,
        keyword_rest: &str,
        kind: TokenType,
    ) -> Option<TokenType> {
        let scan_len = self.current - self.start;
        let keyword_len = keyword_start + keyword_rest.len();

        let scan_start = self.start + keyword_start;
        let scan_end = scan_start + keyword_rest.len();

        if scan_len == keyword_len && &self.code[scan_start..scan_end] == keyword_rest {
            Some(kind)
        } else {
            None
        }
    }
}

fn is_digit(c: u8) -> bool {
    c.is_ascii_digit()
}

fn is_alpha(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'code> {
    pub kind: TokenType,
    pub lexeme: &'code str,
    pub line: usize,
}

impl<'code> Token<'code> {
    pub fn new(kind: TokenType, lexeme: &'code str, line: usize) -> Self {
        Self { kind, lexeme, line }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}
