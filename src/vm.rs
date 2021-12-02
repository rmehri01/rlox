use crate::{
    chunk::{Chunk, Operation, Value},
    compiler,
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
        compiler::compile(code);
        Ok(())
    }

    pub(crate) fn run(&mut self) -> Result<(), LoxError> {
        loop {
            match self.read_op() {
                Operation::Constant(index) => {
                    let constant = self.chunk.read_constant(index);
                    self.push(constant);
                }
                Operation::Add => self.binary_op(|a, b| a + b),
                Operation::Subtract => self.binary_op(|a, b| a - b),
                Operation::Multiply => self.binary_op(|a, b| a * b),
                Operation::Divide => self.binary_op(|a, b| a / b),
                Operation::Negate => {
                    let val = -self.pop();
                    self.push(val);
                }
                Operation::Return => {
                    println!("{:?}", self.pop());
                    return Ok(());
                }
            }
        }
    }

    pub(crate) fn binary_op(&mut self, op: fn(f64, f64) -> f64) {
        let b = self.pop();
        let a = self.pop();
        self.push(op(a, b));
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
}
