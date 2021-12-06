use crate::{
    chunk::{Chunk, Op, Value},
    error::LoxError,
    interner::Interner,
    scanner::{Scanner, Token, TokenType},
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::None,
        }
    }
}

struct ParseRule<'intern, 'code> {
    prefix: Option<ParseFn<'intern, 'code>>,
    infix: Option<ParseFn<'intern, 'code>>,
    precedence: Precedence,
}

enum ParseFn<'intern, 'code> {
    Normal(fn(&mut Parser<'intern, 'code>)),
    Assign(fn(&mut Parser<'intern, 'code>, bool)),
}

impl<'intern, 'code> ParseFn<'intern, 'code> {
    fn apply(self, parser: &mut Parser<'intern, 'code>, can_assign: bool) {
        match self {
            ParseFn::Normal(f) => f(parser),
            ParseFn::Assign(f) => f(parser, can_assign),
        }
    }
}

pub(crate) struct Parser<'intern, 'code> {
    chunk: Chunk,
    scanner: Scanner<'code>,
    interner: &'code mut Interner<'intern>,
    current: Token<'code>,
    previous: Token<'code>,
    had_error: bool,
    panic_mode: bool,
}

impl<'intern, 'code> Parser<'intern, 'code> {
    pub(crate) fn new(interner: &'code mut Interner<'intern>, code: &'code str) -> Self {
        Self {
            chunk: Chunk::new(),
            scanner: Scanner::new(code),
            interner,
            current: Token::new(TokenType::Error, "", 0),
            previous: Token::new(TokenType::Error, "", 0),
            had_error: false,
            panic_mode: false,
        }
    }

    pub(crate) fn compile(mut self) -> Result<Chunk, LoxError> {
        self.advance();

        while !self.matches(TokenType::Eof) {
            self.declaration();
        }

        self.emit_op(Op::Return);
        if self.had_error {
            Err(LoxError::CompileError)
        } else {
            Ok(self.chunk)
        }
    }

    fn emit_ops(&mut self, op1: Op, op2: Op) {
        self.chunk.write(op1, self.previous.line);
        self.chunk.write(op2, self.previous.line);
    }

    fn emit_op(&mut self, op: Op) {
        self.chunk.write(op, self.previous.line);
    }

    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            if self.current.kind != TokenType::Error {
                break;
            }

            self.error_at_current(self.current.lexeme);
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous, msg)
    }

    fn error_at(&mut self, token: Token, message: &str) {
        if self.panic_mode {
            return;
        }

        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => {}
            _ => eprint!(" at '{}'", token.lexeme),
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.current.kind == kind {
            self.advance();
        } else {
            self.error_at_current(message);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let value: f64 = self
            .previous
            .lexeme
            .parse()
            .expect("Parsed value is not a double");
        self.emit_constant(Value::Number(value));
    }

    fn literal(&mut self) {
        match self.previous.kind {
            TokenType::False => self.emit_op(Op::False),
            TokenType::Nil => self.emit_op(Op::Nil),
            TokenType::True => self.emit_op(Op::True),
            _ => panic!("Invalid literal."),
        }
    }

    fn string(&mut self) {
        let lexeme = self.previous.lexeme;
        let value = &lexeme[1..lexeme.len() - 1];
        let str_id = self.interner.intern(value);
        self.emit_constant(Value::String(str_id));
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = self.chunk.add_constant(value);
        match u8::try_from(constant) {
            Ok(index) => index,
            Err(_) => {
                self.error("Too many constants in one chunk.");
                0
            }
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_kind = self.previous.kind;

        self.parse_precedence(Precedence::Unary);

        match operator_kind {
            TokenType::Bang => self.emit_op(Op::Not),
            TokenType::Minus => self.emit_op(Op::Negate),
            _ => panic!("Invalid unary operator."),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let prefix_rule = Parser::get_rule(self.previous.kind).prefix;
        let can_assign = precedence <= Precedence::Assignment;

        match prefix_rule {
            Some(f) => f.apply(self, can_assign),
            None => {
                self.error("Expect expression.");
                return;
            }
        };

        while precedence <= Parser::get_rule(self.current.kind).precedence {
            self.advance();
            let infix_rule = Parser::get_rule(self.previous.kind).infix.unwrap();
            infix_rule.apply(self, can_assign);
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.error("Invalid assignment target.")
        }
    }

    fn binary(&mut self) {
        let operator_kind = self.previous.kind;
        let rule = Parser::get_rule(operator_kind);

        self.parse_precedence(rule.precedence.next());

        match operator_kind {
            TokenType::BangEqual => self.emit_ops(Op::Equal, Op::Not),
            TokenType::EqualEqual => self.emit_op(Op::Equal),
            TokenType::Greater => self.emit_op(Op::Greater),
            TokenType::GreaterEqual => self.emit_ops(Op::Less, Op::Not),
            TokenType::Less => self.emit_op(Op::Less),
            TokenType::LessEqual => self.emit_ops(Op::Greater, Op::Not),
            TokenType::Plus => self.emit_op(Op::Add),
            TokenType::Minus => self.emit_op(Op::Subtract),
            TokenType::Star => self.emit_op(Op::Multiply),
            TokenType::Slash => self.emit_op(Op::Divide),
            _ => panic!("Invalid binary operator."),
        }
    }

    fn get_rule(kind: TokenType) -> ParseRule<'intern, 'code> {
        match kind {
            TokenType::LeftParen => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::grouping)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RightParen => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::LeftBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RightBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Comma => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Dot => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Minus => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::unary)),
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Term,
            },
            TokenType::Plus => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Term,
            },
            TokenType::Semicolon => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Slash => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Factor,
            },
            TokenType::Star => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Factor,
            },
            TokenType::Bang => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::unary)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::BangEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Equality,
            },
            TokenType::Equal => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Equality,
            },
            TokenType::Greater => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Comparison,
            },
            TokenType::GreaterEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Comparison,
            },
            TokenType::Less => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Comparison,
            },
            TokenType::LessEqual => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Normal(Parser::binary)),
                precedence: Precedence::Comparison,
            },
            TokenType::Identifier => ParseRule {
                prefix: Some(ParseFn::Assign(Parser::variable)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::String => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::string)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Number => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::number)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::And => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Class => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Else => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::False => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::number)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::For => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Fun => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::If => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Nil => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::literal)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Or => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Print => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Return => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Super => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::This => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::True => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::literal)),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Var => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::While => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Error => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Eof => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.emit_op(Op::Constant(index))
    }

    fn declaration(&mut self) {
        if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_op(Op::Print);
    }

    fn matches(&mut self, kind: TokenType) -> bool {
        if self.current.kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_op(Op::Pop);
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.previous.kind != TokenType::Eof {
            if self.previous.kind == TokenType::Semicolon {
                return;
            }

            match self.current.kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            };

            self.advance();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_op(Op::Nil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);
        self.identifier_constant(self.previous)
    }

    fn identifier_constant(&mut self, token: Token) -> u8 {
        let identifier = self.interner.intern(token.lexeme);
        self.make_constant(Value::String(identifier))
    }

    fn define_variable(&mut self, index: u8) {
        self.emit_op(Op::DefineGlobal(index))
    }

    fn named_variable(&mut self, token: Token, can_assign: bool) {
        let index = self.identifier_constant(token);
        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_op(Op::SetGlobal(index));
        } else {
            self.emit_op(Op::GetGlobal(index));
        }
    }
}
