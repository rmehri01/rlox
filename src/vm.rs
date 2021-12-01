use crate::{
    chunk::{Chunk, OpCode, Value},
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

    pub(crate) fn run(&mut self) -> Result<(), LoxError> {
        loop {
            match self.read_op() {
                OpCode::Constant(index) => {
                    let constant = self.chunk.read_constant(index);
                    self.push(constant);
                }
                OpCode::Add => self.binary_op(|a, b| a + b),
                OpCode::Subtract => self.binary_op(|a, b| a - b),
                OpCode::Multiply => self.binary_op(|a, b| a * b),
                OpCode::Divide => self.binary_op(|a, b| a / b),
                OpCode::Negate => {
                    let val = -self.pop();
                    self.push(val)
                }
                OpCode::Return => {
                    println!("{:?}", self.pop());
                    return Ok(());
                }
            }
        }
    }

    pub(crate) fn binary_op(&mut self, op: fn(f64, f64) -> f64) {
        let b = self.pop();
        let a = self.pop();
        self.push(op(a, b))
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("empty stack")
    }

    fn read_op(&mut self) -> OpCode {
        let op = self.chunk.read(self.ip);
        self.ip += 1;
        op
    }
}
