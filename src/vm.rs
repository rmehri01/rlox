use std::collections::HashMap;

use crate::{
    chunk::{Chunk, Op, Value},
    compiler::Parser,
    error::LoxError,
    interner::{Interner, StrId},
    object::{FunId, Functions},
};

pub(crate) struct Vm<'intern> {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    interner: Interner<'intern>,
    functions: Functions,
    globals: HashMap<StrId, Value>,
}

impl<'intern, 'code> Vm<'intern> {
    const FRAMES_MAX: usize = 64;
    const STACK_MAX: usize = Vm::FRAMES_MAX * (u8::MAX as usize + 1);

    pub(crate) fn new(interner: Interner<'intern>) -> Self {
        Self {
            frames: Vec::with_capacity(Vm::FRAMES_MAX),
            stack: Vec::with_capacity(Vm::STACK_MAX),
            interner,
            functions: Functions::new(),
            globals: HashMap::new(),
        }
    }

    pub(crate) fn interpret(&mut self, code: &'code str) -> Result<(), LoxError> {
        let function = Parser::new(&mut self.interner, &mut self.functions, code).compile()?;

        let fun_id = self.functions.add(function);
        self.stack.push(Value::Function(fun_id));
        self.frames.push(CallFrame::new(fun_id, 0));

        self.run()
    }

    pub(crate) fn run(&mut self) -> Result<(), LoxError> {
        loop {
            match self.read_op() {
                Op::Constant(index) => {
                    let constant = self.current_chunk().read_constant(index);
                    self.push(constant);
                }
                Op::Add => {
                    let (b, a) = (self.pop(), self.pop());
                    match (a, b) {
                        (Value::String(a), Value::String(b)) => {
                            let str_a = self.interner.lookup(a);
                            let str_b = self.interner.lookup(b);
                            let result = str_a.to_owned() + str_b;
                            let result = self.interner.intern(&result);
                            self.push(Value::String(result));
                        }
                        (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a + b)),
                        _ => {
                            return self
                                .runtime_error("Operands must be two numbers or two strings.");
                        }
                    };
                }
                Op::Nil => self.push(Value::Nil),
                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Pop => {
                    self.pop();
                }
                Op::GetLocal(index) => {
                    let slot = self.current_frame().slot + index as usize;
                    let value = self.stack[slot];
                    self.push(value);
                }
                Op::SetLocal(index) => {
                    let slot = self.current_frame().slot + index as usize;
                    self.stack[slot] = self.peek(0);
                }
                Op::GetGlobal(index) => {
                    let str_id = self.current_chunk().read_string(index);
                    match self.globals.get(&str_id) {
                        Some(&value) => self.push(value),
                        None => {
                            let name = self.interner.lookup(str_id);
                            let msg = format!("Undefined variable '{}'.", name);
                            return self.runtime_error(&msg);
                        }
                    }
                }
                Op::DefineGlobal(index) => {
                    let str_id = self.current_chunk().read_string(index);
                    let name = self.pop();
                    self.globals.insert(str_id, name);
                }
                Op::SetGlobal(index) => {
                    let str_id = self.current_chunk().read_string(index);
                    let value = self.peek(0);
                    if self.globals.insert(str_id, value).is_none() {
                        self.globals.remove(&str_id);
                        let name = self.interner.lookup(str_id);
                        let msg = format!("Undefined variable '{}'.", name);
                        return self.runtime_error(&msg);
                    }
                }
                Op::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b));
                }
                Op::Greater => self.binary_op(|a, b| a > b, Value::Bool)?,
                Op::Less => self.binary_op(|a, b| a < b, Value::Bool)?,
                Op::Subtract => self.binary_op(|a, b| a - b, Value::Number)?,
                Op::Multiply => self.binary_op(|a, b| a * b, Value::Number)?,
                Op::Divide => self.binary_op(|a, b| a / b, Value::Number)?,
                Op::Not => {
                    let value = self.pop();
                    self.push(Value::Bool(value.is_falsey()));
                }
                Op::Negate => {
                    if let Value::Number(value) = self.peek(0) {
                        self.pop();
                        self.push(Value::Number(-value));
                    } else {
                        return self.runtime_error("Operand must be a number.");
                    }
                }
                Op::Print => {
                    // TODO: should use display trait somehow
                    match self.pop() {
                        Value::Bool(value) => println!("{}", value),
                        Value::Nil => println!("nil"),
                        Value::Number(value) => println!("{}", value),
                        Value::String(str_id) => println!("{}", self.interner.lookup(str_id)),
                        Value::Function(fun_id) => {
                            let fn_name = self.functions.lookup(fun_id).name;
                            println!("<fn {}>", self.interner.lookup(fn_name));
                        }
                    };
                }
                Op::Jump(offset) => {
                    self.current_frame_mut().ip += offset as usize;
                }
                Op::JumpIfFalse(offset) => {
                    if self.peek(0).is_falsey() {
                        self.current_frame_mut().ip += offset as usize;
                    }
                }
                Op::Loop(offset) => {
                    self.current_frame_mut().ip -= offset as usize;
                }
                Op::Return => {
                    return Ok(());
                }
            }
        }
    }

    pub(crate) fn binary_op<T>(
        &mut self,
        op: fn(f64, f64) -> T,
        to_val: fn(T) -> Value,
    ) -> Result<(), LoxError> {
        let b = self.pop();
        let a = self.pop();
        match (b, a) {
            (Value::Number(b_val), Value::Number(a_val)) => {
                self.push(to_val(op(a_val, b_val)));
                Ok(())
            }
            _ => self.runtime_error("Operands must be numbers."),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("empty stack")
    }

    fn read_op(&mut self) -> Op {
        let frame = self.current_frame();
        let op = self.current_chunk().read(frame.ip);
        self.current_frame_mut().ip += 1;
        op
    }

    fn peek(&self, distance: usize) -> Value {
        let size = self.stack.len();
        self.stack[size - 1 - distance]
    }

    fn runtime_error(&self, message: &str) -> Result<(), LoxError> {
        eprintln!("{}", message);
        let frame = self.current_frame();
        let line = self.current_chunk().lines[frame.ip - 1];
        eprintln!("[line {}] in script", line);
        Err(LoxError::RuntimeError)
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        let fun_id = self.current_frame().fun_id;
        let function = self.functions.lookup(fun_id);
        &function.chunk
    }
}

struct CallFrame {
    fun_id: FunId,
    ip: usize,
    slot: usize,
}
impl CallFrame {
    fn new(fun_id: FunId, slot: usize) -> Self {
        Self {
            fun_id,
            ip: 0,
            slot,
        }
    }
}
