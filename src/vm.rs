use crate::{
    chunk::{Chunk, Operation, Value},
    compiler::Parser,
    error::LoxError,
};

pub(crate) struct Vm {
    chunk: Chunk,
    stack: Vec<Value>,
    ip: usize,
}

impl Vm {
    const CAPACITY: usize = 256;

    pub(crate) fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            stack: Vec::with_capacity(Vm::CAPACITY),
            ip: 0,
        }
    }

    pub(crate) fn interpret(&mut self, code: &str) -> Result<(), LoxError> {
        let chunk = Parser::new(code).compile()?;

        self.chunk = chunk;
        self.ip = 0;

        self.run()
    }

    pub(crate) fn run(&mut self) -> Result<(), LoxError> {
        loop {
            match self.read_op() {
                Operation::Constant(index) => {
                    let constant = self.chunk.read_constant(index);
                    self.push(constant);
                }
                Operation::Add => self.binary_op(|a, b| a + b, Value::Number)?,
                Operation::Nil => self.push(Value::Nil),
                Operation::True => self.push(Value::Bool(true)),
                Operation::False => self.push(Value::Bool(false)),
                Operation::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b));
                }
                Operation::Greater => self.binary_op(|a, b| a > b, Value::Bool)?,
                Operation::Less => self.binary_op(|a, b| a < b, Value::Bool)?,
                Operation::Subtract => self.binary_op(|a, b| a - b, Value::Number)?,
                Operation::Multiply => self.binary_op(|a, b| a * b, Value::Number)?,
                Operation::Divide => self.binary_op(|a, b| a / b, Value::Number)?,
                Operation::Not => {
                    let value = self.pop();
                    self.push(Value::Bool(value.is_falsey()));
                }
                Operation::Negate => {
                    if let Value::Number(value) = self.peek(0) {
                        self.pop();
                        self.push(Value::Number(-value));
                    } else {
                        return self.runtime_error("Operand must be a number.");
                    }
                }
                Operation::Return => {
                    println!("{:?}", self.pop());
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

    fn read_op(&mut self) -> Operation {
        let op = self.chunk.read(self.ip);
        self.ip += 1;
        op
    }

    fn peek(&self, distance: usize) -> Value {
        let size = self.stack.len();
        self.stack[size - 1 - distance]
    }

    fn runtime_error(&self, message: &str) -> Result<(), LoxError> {
        eprintln!("{}", message);
        let line = self.chunk.lines[self.ip - 1];
        Err(LoxError::RuntimeError)
    }
}
