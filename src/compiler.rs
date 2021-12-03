use std::collections::HashMap;

use crate::{
    chunk::{Chunk, Operation, Value},
    error::LoxError,
    scanner::{Scanner, Token, TokenType},
};

#[derive(Clone, Copy, PartialEq, PartialOrd)]
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

#[derive(Clone, Copy)]
struct ParseRule<'code> {
    prefix: ParseFn<'code>,
    infix: ParseFn<'code>,
    precedence: Precedence,
}

impl<'code> ParseRule<'code> {
    fn new(prefix: ParseFn<'code>, infix: ParseFn<'code>, precedence: Precedence) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

type ParseFn<'code> = Option<fn(&mut Parser<'code>)>;

pub(crate) struct Parser<'code> {
    chunk: Chunk,
    scanner: Scanner<'code>,
    current: Token<'code>,
    previous: Token<'code>,
    had_error: bool,
    panic_mode: bool,
    rules: HashMap<TokenType, ParseRule<'code>>,
}

impl<'code> Parser<'code> {
    pub(crate) fn new(code: &'code str) -> Self {
        let rules = HashMap::from([
            (
                TokenType::LeftParen,
                ParseRule::new(Some(Parser::grouping), None, Precedence::None),
            ),
            (
                TokenType::RightParen,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::LeftBrace,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::RightBrace,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::RightBrace,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Comma,
                ParseRule::new(None, None, Precedence::None),
            ),
            (TokenType::Dot, ParseRule::new(None, None, Precedence::None)),
            (
                TokenType::Minus,
                ParseRule::new(Some(Parser::unary), Some(Parser::binary), Precedence::Term),
            ),
            (
                TokenType::Plus,
                ParseRule::new(None, Some(Parser::binary), Precedence::Term),
            ),
            (
                TokenType::Semicolon,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Slash,
                ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            ),
            (
                TokenType::Star,
                ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            ),
            (
                TokenType::Bang,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::BangEqual,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Equal,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::EqualEqual,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Greater,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::GreaterEqual,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Less,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::LessEqual,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Identifier,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::String,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Number,
                ParseRule::new(Some(Parser::number), None, Precedence::None),
            ),
            (TokenType::And, ParseRule::new(None, None, Precedence::None)),
            (
                TokenType::Class,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Else,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::False,
                ParseRule::new(None, None, Precedence::None),
            ),
            (TokenType::For, ParseRule::new(None, None, Precedence::None)),
            (TokenType::Fun, ParseRule::new(None, None, Precedence::None)),
            (TokenType::If, ParseRule::new(None, None, Precedence::None)),
            (TokenType::Nil, ParseRule::new(None, None, Precedence::None)),
            (TokenType::Or, ParseRule::new(None, None, Precedence::None)),
            (
                TokenType::Print,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Return,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Super,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::This,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::True,
                ParseRule::new(None, None, Precedence::None),
            ),
            (TokenType::Var, ParseRule::new(None, None, Precedence::None)),
            (
                TokenType::While,
                ParseRule::new(None, None, Precedence::None),
            ),
            (
                TokenType::Error,
                ParseRule::new(None, None, Precedence::None),
            ),
            (TokenType::Eof, ParseRule::new(None, None, Precedence::None)),
        ]);

        Self {
            chunk: Chunk::new(),
            scanner: Scanner::new(code),
            current: Token::new(TokenType::Error, "", 0),
            previous: Token::new(TokenType::Error, "", 0),
            had_error: false,
            panic_mode: false,
            rules,
        }
    }

    pub(crate) fn compile(mut self) -> Result<Chunk, LoxError> {
        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression");

        self.emit_op(Operation::Return);
        if self.had_error {
            Err(LoxError::CompileError)
        } else {
            Ok(self.chunk)
        }
    }

    fn emit_op(&mut self, operation: Operation) {
        self.chunk.write(operation, self.previous.line);
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
            TokenType::Minus => self.emit_op(Operation::Negate),
            _ => panic!("Invalid unary operator."),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let prefix_rule = self.get_rule(self.previous.kind).prefix;

        match prefix_rule {
            Some(f) => f(self),
            None => {
                self.error("Expect expression.");
                return;
            }
        };

        while precedence <= self.get_rule(self.current.kind).precedence {
            self.advance();
            let infix_rule = self.get_rule(self.previous.kind).infix.unwrap();
            infix_rule(self);
        }
    }

    fn binary(&mut self) {
        let operator_kind = self.previous.kind;
        let rule = self.get_rule(operator_kind);

        self.parse_precedence(rule.precedence.next());

        match operator_kind {
            TokenType::Plus => self.emit_op(Operation::Add),
            TokenType::Minus => self.emit_op(Operation::Subtract),
            TokenType::Star => self.emit_op(Operation::Multiply),
            TokenType::Slash => self.emit_op(Operation::Divide),
            _ => panic!("Invalid binary operator."),
        }
    }

    fn get_rule(&self, kind: TokenType) -> ParseRule<'code> {
        self.rules[&kind]
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.emit_op(Operation::Constant(index))
    }
}
