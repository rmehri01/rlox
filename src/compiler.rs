use std::mem;

use arrayvec::ArrayVec;

use crate::{
    chunk::{Chunk, Op, Value},
    error::LoxError,
    memory::{HeapId, Memory},
    object::{FnUpvalue, Function, ObjData},
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
    fn next(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::None,
        }
    }
}

struct ParseRule<'code> {
    prefix: Option<ParseFn<'code>>,
    infix: Option<ParseFn<'code>>,
    precedence: Precedence,
}

enum ParseFn<'code> {
    Normal(fn(&mut Parser<'code>)),
    Assign(fn(&mut Parser<'code>, bool)),
}

impl<'code> ParseFn<'code> {
    fn apply(self, parser: &mut Parser<'code>, can_assign: bool) {
        match self {
            ParseFn::Normal(f) => f(parser),
            ParseFn::Assign(f) => f(parser, can_assign),
        }
    }
}

pub struct Parser<'code> {
    scanner: Scanner<'code>,
    memory: &'code mut Memory,
    compiler: Compiler<'code>,
    current: Token<'code>,
    previous: Token<'code>,
    had_error: bool,
    panic_mode: bool,
}

impl<'code> Parser<'code> {
    pub fn new(memory: &'code mut Memory, code: &'code str) -> Self {
        Self {
            scanner: Scanner::new(code),
            memory,
            compiler: Compiler::new(None, FunctionType::Script),
            current: Token::new(TokenType::Error, "", 0),
            previous: Token::new(TokenType::Error, "", 0),
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(mut self) -> Result<Function, LoxError> {
        self.advance();

        while !self.matches(TokenType::Eof) {
            self.declaration();
        }

        self.emit_return();

        if self.had_error {
            Err(LoxError::CompileError)
        } else {
            Ok(self.compiler.function)
        }
    }

    fn emit_ops(&mut self, op1: Op, op2: Op) {
        let line1 = self.previous.line;
        self.current_chunk_mut().write(op1, line1);

        let line2 = self.previous.line;
        self.current_chunk_mut().write(op2, line2);
    }

    fn emit_op(&mut self, op: Op) {
        let line = self.previous.line;
        self.current_chunk_mut().write(op, line);
    }

    fn advance(&mut self) {
        self.previous = self.current;

        self.current = self.scanner.scan_token();
        if self.check(TokenType::Error) {
            self.error_at_current(self.current.lexeme)
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message)
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
        if self.check(kind) {
            self.advance()
        } else {
            self.error_at_current(message)
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(&Precedence::Assignment)
    }

    fn number(&mut self) {
        let value: f64 = self
            .previous
            .lexeme
            .parse()
            .expect("parsed value to be a double");
        self.emit_constant(Value::Number(value))
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
        let str_id = self.memory.intern(value);

        self.emit_constant(Value::String(str_id))
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign)
    }

    fn and(&mut self) {
        let end_jump = self.emit_jump(Op::JumpIfFalse);

        self.emit_op(Op::Pop);
        self.parse_precedence(&Precedence::And);

        self.patch_jump(end_jump)
    }

    fn or(&mut self) {
        let else_jump = self.emit_jump(Op::JumpIfFalse);
        let end_jump = self.emit_jump(Op::Jump);

        self.patch_jump(else_jump);
        self.emit_op(Op::Pop);

        self.parse_precedence(&Precedence::Or);
        self.patch_jump(end_jump)
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = self.current_chunk_mut().add_constant(value);
        u8::try_from(constant).unwrap_or_else(|_| {
            self.error("Too many constants in one chunk.");
            0
        })
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.identifier_constant(self.previous);

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_op(Op::SetProperty(name));
        } else {
            self.emit_op(Op::GetProperty(name));
        }
    }

    fn unary(&mut self) {
        let operator_kind = self.previous.kind;

        self.parse_precedence(&Precedence::Unary);

        match operator_kind {
            TokenType::Bang => self.emit_op(Op::Not),
            TokenType::Minus => self.emit_op(Op::Negate),
            _ => panic!("Invalid unary operator."),
        };
    }

    fn parse_precedence(&mut self, precedence: &Precedence) {
        self.advance();

        let prefix_rule = Parser::get_rule(self.previous.kind).prefix;
        let can_assign = precedence <= &Precedence::Assignment;

        match prefix_rule {
            Some(f) => f.apply(self, can_assign),
            None => self.error("Expect expression."),
        };

        while precedence <= &Parser::get_rule(self.current.kind).precedence {
            self.advance();
            let infix_rule = Parser::get_rule(self.previous.kind)
                .infix
                .expect("infix parse rule");
            infix_rule.apply(self, can_assign);
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.error("Invalid assignment target.")
        }
    }

    fn binary(&mut self) {
        let operator_kind = self.previous.kind;
        let rule = Parser::get_rule(operator_kind);

        self.parse_precedence(&rule.precedence.next());

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
        };
    }

    fn get_rule(kind: TokenType) -> ParseRule<'code> {
        match kind {
            TokenType::LeftParen => ParseRule {
                prefix: Some(ParseFn::Normal(Parser::grouping)),
                infix: Some(ParseFn::Normal(Parser::call)),
                precedence: Precedence::Call,
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
                infix: Some(ParseFn::Assign(Parser::dot)),
                precedence: Precedence::Call,
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
                infix: Some(ParseFn::Normal(Parser::and)),
                precedence: Precedence::And,
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
                prefix: Some(ParseFn::Normal(Parser::literal)),
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
                infix: Some(ParseFn::Normal(Parser::or)),
                precedence: Precedence::Or,
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
        self.emit_op(Op::Constant(index));
    }

    fn declaration(&mut self) {
        if self.matches(TokenType::Class) {
            self.class_declaration();
        } else if self.matches(TokenType::Fun) {
            self.fun_declaration()
        } else if self.matches(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement()
        } else if self.matches(TokenType::For) {
            self.for_statement()
        } else if self.matches(TokenType::If) {
            self.if_statement()
        } else if self.matches(TokenType::Return) {
            self.return_statement()
        } else if self.matches(TokenType::While) {
            self.while_statement()
        } else if self.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_op(Op::Print);
    }

    fn matches(&mut self, kind: TokenType) -> bool {
        if self.check(kind) {
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

        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            0
        } else {
            self.identifier_constant(self.previous)
        }
    }

    fn identifier_constant(&mut self, token: Token) -> u8 {
        let identifier = self.memory.intern(token.lexeme);
        self.make_constant(Value::String(identifier))
    }

    fn define_variable(&mut self, index: u8) {
        if self.compiler.scope_depth > 0 {
            self.compiler.mark_initialized();
            return;
        }

        self.emit_op(Op::DefineGlobal(index));
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let get_op;
        let set_op;

        if let Some(res) = self.compiler.resolve_local(name) {
            let arg = res.unwrap_or_else(|msg| {
                self.error(msg);
                0
            });

            get_op = Op::GetLocal(arg);
            set_op = Op::SetLocal(arg);
        } else if let Some(res) = self.compiler.resolve_upvalue(name) {
            let arg = res.unwrap_or_else(|msg| {
                self.error(msg);
                0
            });

            get_op = Op::GetUpvalue(arg);
            set_op = Op::SetUpvalue(arg);
        } else {
            let index = self.identifier_constant(name);
            get_op = Op::GetGlobal(index);
            set_op = Op::SetGlobal(index);
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_op(set_op);
        } else {
            self.emit_op(get_op);
        }
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")
    }

    fn check(&self, kind: TokenType) -> bool {
        self.current.kind == kind
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while let Some(local) = self
            .compiler
            .locals
            .last()
            .filter(|local| local.depth > self.compiler.scope_depth)
        {
            if local.is_captured {
                self.emit_op(Op::CloseUpvalue);
            } else {
                self.emit_op(Op::Pop);
            }
            self.compiler.locals.pop();
        }
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let prev = self.previous;

        if self.compiler.is_local_declared(prev) {
            self.error("Already a variable with this name in this scope.")
        } else {
            self.add_local(prev)
        }
    }

    fn add_local(&mut self, token: Token<'code>) {
        if self.compiler.locals.len() == Compiler::MAX_LOCALS {
            self.error("Too many local variables in function.")
        } else {
            let local = Local::new(token, -1);
            self.compiler.locals.push(local);
        }
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(Op::JumpIfFalse);
        self.emit_op(Op::Pop);
        self.statement();

        let else_jump = self.emit_jump(Op::Jump);
        self.patch_jump(then_jump);
        self.emit_op(Op::Pop);

        if self.matches(TokenType::Else) {
            self.statement()
        }
        self.patch_jump(else_jump)
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk().last_index() - offset;

        if let Ok(jump) = u16::try_from(jump) {
            match self.current_chunk_mut().code[offset] {
                Op::JumpIfFalse(ref mut o) | Op::Jump(ref mut o) => *o = jump,
                _ => panic!("Attempting to patch non-jump op"),
            }
        } else {
            self.error("Too much code to jump over.");
        }
    }

    fn emit_jump(&mut self, make_jump: fn(u16) -> Op) -> usize {
        self.emit_op(make_jump(0xffff));
        self.current_chunk().last_index()
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(Op::JumpIfFalse);
        self.emit_op(Op::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_op(Op::Pop);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().code.len() - loop_start + 1;

        if let Ok(offset) = u16::try_from(offset) {
            self.emit_op(Op::Loop(offset))
        } else {
            self.error("Loop body too large.")
        }
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.matches(TokenType::Semicolon) {
            // No initializer.
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().code.len();

        let mut exit_jump = None;
        if !self.matches(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            exit_jump = Some(self.emit_jump(Op::JumpIfFalse));
            self.emit_op(Op::Pop);
        }

        if !self.matches(TokenType::RightParen) {
            let body_jump = self.emit_jump(Op::Jump);
            let increment_start = self.current_chunk().code.len();

            self.expression();
            self.emit_op(Op::Pop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_op(Op::Pop);
        }

        self.end_scope();
    }

    fn current_chunk(&self) -> &Chunk {
        &self.compiler.function.chunk
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiler.function.chunk
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        let class_name = self.previous;
        let name_constant = self.identifier_constant(class_name);

        self.declare_variable();
        self.emit_op(Op::Class(name_constant));
        self.define_variable(name_constant);
        self.named_variable(class_name, false);

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");

        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.method();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.emit_op(Op::Pop);
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");

        let name_constant = self.identifier_constant(self.previous);
        self.function(FunctionType::Function);

        self.emit_op(Op::Method(name_constant));
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.compiler.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn function(&mut self, kind: FunctionType) {
        self.push_compiler(kind);

        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                self.compiler.function.arity += 1;
                if self.compiler.function.arity > 255 {
                    return self.error_at_current("Can't have more than 255 parameters.");
                }

                let index = self.parse_variable("Expect parameter name.");
                self.define_variable(index);

                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");

        self.block();

        let function = self.pop_compiler();
        let fun_id = self.memory.alloc(ObjData::Function(function));
        let index = self.make_constant(Value::Function(fun_id));
        self.emit_op(Op::Closure(index));
    }

    fn push_compiler(&mut self, kind: FunctionType) {
        let function_name = self.memory.intern(self.previous.lexeme);
        let inner = Compiler::new(Some(function_name), kind);
        let enclosing = mem::replace(&mut self.compiler, inner);
        self.compiler.enclosing = Some(Box::new(enclosing));
    }

    fn pop_compiler(&mut self) -> Function {
        self.emit_return();

        let enclosing = self.compiler.enclosing.take();
        let inner = mem::replace(&mut self.compiler, *enclosing.expect("enclosing compiler"));
        inner.function
    }

    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_op(Op::Call(arg_count));
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;

        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();

                if arg_count == 255 {
                    self.error("Can't have more than 255 arguments.");
                }

                arg_count += 1;

                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    fn emit_return(&mut self) {
        self.emit_op(Op::Nil);
        self.emit_op(Op::Return);
    }

    fn return_statement(&mut self) {
        if self.compiler.function_type == FunctionType::Script {
            self.error("Can't return from top-level code.");
        }

        if self.matches(TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_op(Op::Return);
        }
    }
}

#[derive(Debug, PartialEq)]
enum FunctionType {
    Function,
    Script,
}

#[derive(Debug)]
pub struct Compiler<'code> {
    enclosing: Option<Box<Compiler<'code>>>,
    function: Function,
    function_type: FunctionType,
    locals: ArrayVec<Local<'code>, { Compiler::MAX_LOCALS }>,
    scope_depth: i32,
}

impl Compiler<'_> {
    pub const MAX_LOCALS: usize = u8::MAX as usize + 1;

    fn new(function_name: Option<HeapId>, kind: FunctionType) -> Self {
        let mut locals = ArrayVec::new();
        locals.push(Local::new(Token::new(TokenType::Error, "", 0), 0)); // TODO: initializer

        Self {
            enclosing: None,
            function: Function::new(function_name),
            function_type: kind,
            locals,
            scope_depth: 0,
        }
    }

    fn is_local_declared(&self, name: Token) -> bool {
        self.locals
            .iter()
            .rev()
            .take_while(|local| local.depth == -1 || local.depth >= self.scope_depth)
            .any(|local| local.name.lexeme == name.lexeme)
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth != 0 {
            let last = self.locals.last_mut().expect("non-empty locals");
            last.depth = self.scope_depth;
        }
    }

    fn resolve_local(&self, name: Token) -> Option<Result<u8, &'static str>> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name.lexeme == name.lexeme)
            .map(|(i, local)| {
                (local.depth != -1)
                    .then(|| i as u8)
                    .ok_or("Can't read local variable in its own initializer.")
            })
    }

    fn resolve_upvalue(&mut self, name: Token) -> Option<Result<u8, &'static str>> {
        self.enclosing.as_mut().and_then(|enclosing| {
            enclosing
                .resolve_local(name)
                .map(|res| {
                    let index = res?;
                    enclosing.locals[index as usize].is_captured = true;
                    Compiler::add_upvalue(&mut self.function.upvalues, index, true)
                })
                .or_else(|| {
                    enclosing.resolve_upvalue(name).map(|res| {
                        let index = res?;
                        Compiler::add_upvalue(&mut self.function.upvalues, index, false)
                    })
                })
        })
    }

    fn add_upvalue(
        upvalues: &mut Vec<FnUpvalue>,
        index: u8,
        is_local: bool,
    ) -> Result<u8, &'static str> {
        for (i, upvalue) in upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return Ok(i.try_into().unwrap());
            }
        }

        let upvalue_count = upvalues.len();

        if upvalue_count == Compiler::MAX_LOCALS {
            Err("Too many closure variables in function.")
        } else {
            let upvalue = FnUpvalue::new(index, is_local);
            upvalues.push(upvalue);

            Ok(upvalue_count as u8)
        }
    }
}

#[derive(Debug)]
struct Local<'code> {
    name: Token<'code>,
    depth: i32,
    is_captured: bool,
}

impl<'code> Local<'code> {
    fn new(name: Token<'code>, depth: i32) -> Self {
        Self {
            name,
            depth,
            is_captured: false,
        }
    }
}
